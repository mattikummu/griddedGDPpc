from __future__ import annotations

import argparse
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Tuple

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from catboost import CatBoostRegressor
from lightgbm import LGBMRegressor
from scipy.stats import ttest_1samp
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import HistGradientBoostingRegressor
from sklearn.impute import SimpleImputer
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.pipeline import Pipeline
from xgboost import XGBRegressor


FEATURES = [
    "adm0GDP",
    "adm0gini",
    "adm0travelTime",
    "adm0urb",
    "travelTimeRatio",
    "urbRatio",
]
TARGET = "gdpRatio"
STRAT_COL = "RegName"


@dataclass
class Config:
    """Runtime configuration for training, diagnostics, and inference outputs."""
    train_csv: Path
    infer_csv: Path
    output_dir: Path
    output_csv: Path
    test_size: float = 0.20
    random_seed: int = 42
    outlier_q_low: float = 0.0025
    outlier_q_high: float = 0.9975


def parse_args() -> Config:
    """Parse CLI arguments and return a typed config object."""
    parser = argparse.ArgumentParser(description="GDP downscaling training and inference pipeline")
    parser.add_argument("--train-csv", type=Path, default=Path("adm1DataForDownscaling_ratio.csv"))
    parser.add_argument("--infer-csv", type=Path, default=Path("adm2DataForDownscaling_ratio.csv"))
    parser.add_argument("--output-dir", type=Path, default=Path("."))
    parser.add_argument("--output-csv", type=Path, default=Path("adm2DataWithPredictions.csv"))
    parser.add_argument("--test-size", type=float, default=0.20)
    parser.add_argument("--random-seed", type=int, default=42)
    args = parser.parse_args()

    return Config(
        train_csv=args.train_csv,
        infer_csv=args.infer_csv,
        output_dir=args.output_dir,
        output_csv=args.output_csv,
        test_size=args.test_size,
        random_seed=args.random_seed,
    )


def panel_label(i: int) -> str:
    """Generate panel labels a, b, c, ... for subplot annotations."""
    return chr(97 + i) if i < 26 else f"a{i - 25}"


def load_and_split(cfg: Config) -> Tuple[pd.DataFrame, pd.DataFrame, pd.Series, pd.Series, pd.Series]:
    """Load training data, apply outlier trimming, and stratified train/test split."""
    df = pd.read_csv(cfg.train_csv)

    missing = set(FEATURES + [TARGET, STRAT_COL]) - set(df.columns)
    if missing:
        raise ValueError(f"Missing columns in training CSV: {missing}")

    lower = df[TARGET].quantile(cfg.outlier_q_low)
    upper = df[TARGET].quantile(cfg.outlier_q_high)
    df = df[(df[TARGET] >= lower) & (df[TARGET] <= upper)].copy()

    print(
        f"Kept {len(df)} rows after outlier removal. "
        f"min={df[TARGET].min():.4f}, max={df[TARGET].max():.4f}"
    )

    X = df[FEATURES]
    y = df[TARGET]
    strata = df[STRAT_COL]

    X_train, X_test, y_train, y_test, _, strat_test = train_test_split(
        X,
        y,
        strata,
        test_size=cfg.test_size,
        random_state=cfg.random_seed,
        stratify=strata,
    )
    return X_train, X_test, y_train, y_test, strat_test


def get_model_defs(random_seed: int):
    """Return preprocessing and model/grid definitions for all candidate regressors."""
    preprocess = ColumnTransformer([
        ("num", SimpleImputer(strategy="median"), FEATURES),
    ])

    model_defs = [
        (
            "xgb",
            XGBRegressor(
                objective="reg:squarederror",
                random_state=random_seed,
                tree_method="hist",
                n_jobs=-1,
            ),
            {
                "xgb__n_estimators": [400, 800, 1200],
                "xgb__learning_rate": [0.03, 0.1, 0.3],
                "xgb__max_depth": [4, 6, 8],
                "xgb__subsample": [0.8, 1.0],
                "xgb__colsample_bytree": [0.8, 1.0],
                "xgb__reg_lambda": [1, 3, 10],
            },
        ),
        (
            "lgbm",
            LGBMRegressor(objective="regression", random_state=random_seed, n_jobs=-1),
            {
                "lgbm__n_estimators": [400, 800, 1200],
                "lgbm__learning_rate": [0.03, 0.1, 0.3],
                "lgbm__max_depth": [4, 6, 8, -1],
                "lgbm__subsample": [0.8, 1.0],
                "lgbm__colsample_bytree": [0.8, 1.0],
                "lgbm__reg_lambda": [1, 3, 10],
            },
        ),
        (
            "catb",
            CatBoostRegressor(loss_function="RMSE", random_seed=random_seed, verbose=0),
            {
                "catb__iterations": [400, 800, 1200],
                "catb__learning_rate": [0.03, 0.1, 0.3],
                "catb__depth": [4, 6, 8],
                "catb__subsample": [0.8, 1.0],
                "catb__l2_leaf_reg": [1, 3, 10],
            },
        ),
        (
            "hgb",
            HistGradientBoostingRegressor(loss="squared_error", random_state=random_seed),
            {
                "hgb__max_iter": [400, 800, 1200],
                "hgb__learning_rate": [0.03, 0.1, 0.3],
                "hgb__max_depth": [4, 6, 8],
                "hgb__l2_regularization": [1, 3, 10],
            },
        ),
    ]
    return preprocess, model_defs


def train_models(
    X_train: pd.DataFrame,
    y_train: pd.Series,
    X_test: pd.DataFrame,
    y_test: pd.Series,
    random_seed: int,
):
    """Tune each model with GridSearchCV and return ranked test-set performance."""
    preprocess, model_defs = get_model_defs(random_seed)

    best_models: Dict[str, Pipeline] = {}
    results: List[dict] = []

    for name, model, grid in model_defs:
        print(f"\n==== Training and tuning: {name.upper()} ====")
        pipe = Pipeline([
            ("prep", preprocess),
            (name, model),
        ])

        gcv = GridSearchCV(
            estimator=pipe,
            param_grid=grid,
            cv=5,
            n_jobs=-1,
            verbose=1,
            scoring="neg_mean_squared_error",
        )
        gcv.fit(X_train, y_train)

        y_pred = gcv.predict(X_test)
        r2 = r2_score(y_test, y_pred)
        print(f"Best params: {gcv.best_params_}")
        print(f"Test R2: {r2:.3f}")

        best_models[name] = gcv.best_estimator_
        results.append({"model": name, "R2": float(r2), "params": gcv.best_params_})

    results_df = pd.DataFrame(results).sort_values("R2", ascending=False).reset_index(drop=True)
    best_model_name = results_df.loc[0, "model"]
    best_model = best_models[best_model_name]

    print("\n=== Model comparison on test set ===")
    print(results_df[["model", "R2"]])

    return best_models, results_df, best_model_name, best_model


def save_model_artifacts(
    cfg: Config,
    best_model_name: str,
    best_model: Pipeline,
    results_df: pd.DataFrame,
) -> Path:
    """Persist the best model and a metadata JSON summary to disk."""
    cfg.output_dir.mkdir(parents=True, exist_ok=True)

    model_path = cfg.output_dir / f"best_model_{best_model_name}.joblib"
    joblib.dump(best_model, model_path, compress=3)

    meta = {
        "best_model_name": best_model_name,
        "best_r2": float(results_df.loc[0, "R2"]),
        "all_test_r2": {
            row["model"]: float(row["R2"]) for _, row in results_df.iterrows()
        },
    }
    meta_path = cfg.output_dir / f"best_model_{best_model_name}_meta.json"
    with meta_path.open("w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)

    print(f"Saved model: {model_path}")
    print(f"Saved metadata: {meta_path}")
    return model_path


def plot_pred_vs_true_by_model(
    best_models: Dict[str, Pipeline],
    model_order: List[str],
    X_test: pd.DataFrame,
    y_test: pd.Series,
    output_path: Path,
) -> None:
    """Create predicted-vs-true test scatter subplots for all trained models."""
    n_models = len(model_order)
    n_cols = 2
    n_rows = int(np.ceil(n_models / n_cols))

    # Cache predictions once so plotting and metrics use consistent values.
    pred_cache = {name: best_models[name].predict(X_test) for name in model_order}

    all_preds = np.concatenate([pred_cache[name] for name in model_order])
    true_vals = y_test.to_numpy()
    axis_min = float(min(true_vals.min(), all_preds.min()))
    axis_max = float(max(true_vals.max(), all_preds.max()))

    colors = plt.cm.tab10(np.linspace(0, 1, max(n_models, 3)))
    fig, axes = plt.subplots(n_rows, n_cols, figsize=(6 * n_cols, 5 * n_rows), sharex=True, sharey=True)
    axes = np.array(axes).reshape(-1)

    for i, name in enumerate(model_order):
        ax = axes[i]
        y_hat = pred_cache[name]
        r2_val = r2_score(y_test, y_hat)
        rmse_val = np.sqrt(mean_squared_error(y_test, y_hat))

        ax.scatter(y_test, y_hat, s=14, alpha=0.65, color=colors[i], edgecolors="none")
        ax.plot([axis_min, axis_max], [axis_min, axis_max], "k--", lw=1)
        ax.set_xlim(axis_min, axis_max)
        ax.set_ylim(axis_min, axis_max)
        ax.set_title(f"{name.upper()} (R2={r2_val:.3f}, RMSE={rmse_val:.3f})")

        ax.text(
            0.0,
            1.02,
            panel_label(i),
            transform=ax.transAxes,
            va="bottom",
            ha="left",
            fontsize=11,
            fontweight="bold",
            clip_on=False,
        )

        row_idx = i // n_cols
        col_idx = i % n_cols
        ax.set_ylabel("Predicted" if col_idx == 0 else "")
        ax.set_xlabel("True" if row_idx == n_rows - 1 else "")

    for ax in axes[n_models:]:
        ax.remove()

    plt.tight_layout()
    fig.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved figure: {output_path}")


def plot_error_histograms(
    y_test: pd.Series,
    y_pred: np.ndarray,
    strat_test: pd.Series,
    output_path: Path,
) -> Tuple[pd.DataFrame, np.ndarray]:
    """Create region-wise error histograms and return error frame + region ordering."""
    errors = y_pred - y_test
    err_df = pd.DataFrame({"RegionID": strat_test.values, "Error": errors})

    regions = err_df["RegionID"].unique()
    n_reg = len(regions)
    n_cols = 3
    n_rows = int(np.ceil(n_reg / n_cols))

    # Keep a common x-range to allow direct visual comparison across regions.
    global_min = float(err_df["Error"].min())
    global_max = float(err_df["Error"].max())

    fig, axes = plt.subplots(n_rows, n_cols, figsize=(4 * n_cols, 3 * n_rows), sharex=False)
    axes = axes.flatten()

    for i, (ax, reg) in enumerate(zip(axes, regions)):
        e = err_df.loc[err_df["RegionID"] == reg, "Error"]
        sns.histplot(e, kde=True, ax=ax)
        ax.axvline(0, color="k", ls="--")
        ax.set_xlim(global_min, global_max)
        ax.set_title(str(reg))

        ax.text(
            0.0,
            1.02,
            panel_label(i),
            transform=ax.transAxes,
            va="bottom",
            ha="left",
            fontsize=11,
            fontweight="bold",
            clip_on=False,
        )

        mean_e = float(e.mean())
        std_e = float(e.std(ddof=1)) if len(e) > 1 else 0.0
        ax.text(
            0.98,
            0.98,
            f"mean={mean_e:.3f}\nstd={std_e:.3f}",
            transform=ax.transAxes,
            va="top",
            ha="right",
            fontsize=8,
            bbox=dict(boxstyle="round,pad=0.2", facecolor="white", alpha=0.7, edgecolor="none"),
        )

        row_idx = i // n_cols
        col_idx = i % n_cols
        ax.set_ylabel("Count" if col_idx == 0 else "")
        ax.set_xlabel("Prediction error" if row_idx == n_rows - 1 else "")

    for ax in axes[len(regions):]:
        ax.remove()

    plt.tight_layout()
    fig.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved figure: {output_path}")

    return err_df, regions


def plot_error_scatter_by_region(err_df: pd.DataFrame, regions: np.ndarray, output_path: Path) -> None:
    """Create region-wise raw error scatter subplots with shared y-axis."""
    n_reg = len(regions)
    n_cols = 3
    n_rows = int(np.ceil(n_reg / n_cols))

    fig, axes = plt.subplots(n_rows, n_cols, figsize=(4 * n_cols, 3 * n_rows), sharey=True)
    axes = np.array(axes).reshape(-1)

    for i, (ax, reg) in enumerate(zip(axes, regions)):
        e = err_df.loc[err_df["RegionID"] == reg, "Error"].to_numpy()
        x = np.arange(len(e))

        ax.scatter(x, e, s=12, alpha=0.65, color="tab:blue", edgecolors="none")
        ax.axhline(0, color="k", ls="--", lw=1)
        ax.set_title(str(reg))

        ax.text(
            0.0,
            1.02,
            panel_label(i),
            transform=ax.transAxes,
            va="bottom",
            ha="left",
            fontsize=11,
            fontweight="bold",
            clip_on=False,
        )

        mean_e = float(np.mean(e))
        std_e = float(np.std(e, ddof=1)) if len(e) > 1 else 0.0
        ax.text(
            0.98,
            0.98,
            f"mean={mean_e:.3f}\nstd={std_e:.3f}",
            transform=ax.transAxes,
            va="top",
            ha="right",
            fontsize=8,
            bbox=dict(boxstyle="round,pad=0.2", facecolor="white", alpha=0.7, edgecolor="none"),
        )

        row_idx = i // n_cols
        col_idx = i % n_cols
        ax.set_ylabel("Prediction error" if col_idx == 0 else "")
        ax.set_xlabel("Observation" if row_idx == n_rows - 1 else "")

    for ax in axes[len(regions):]:
        ax.remove()

    plt.tight_layout()
    fig.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved figure: {output_path}")


def print_bias_diagnostics(err_df: pd.DataFrame, regions: np.ndarray) -> None:
    """Print overall and per-region mean-error significance diagnostics."""
    errors = err_df["Error"].to_numpy()
    overall_bias = float(np.mean(errors))
    _, p_val = ttest_1samp(errors, popmean=0.0)

    print("\n--- Bias diagnostics (mean error) ---")
    print(f"Overall mean error: {overall_bias:.4f}")
    print(f"Overall t-test p-value: {p_val:.4f}")

    print("\nRegion  | mean error | p-value")
    print("------------------------------")
    for reg in regions:
        e = err_df.loc[err_df["RegionID"] == reg, "Error"]
        mean_bias = float(e.mean())
        _, p_val_r = ttest_1samp(e, popmean=0.0)
        flag = " **bias?**" if p_val_r < 0.05 else ""
        print(f"{str(reg):>22} | {mean_bias:11.4f} | {p_val_r:7.4f}{flag}")


def run_inference(cfg: Config, model_path: Path) -> Path:
    """Load best model, predict ADM2 ratios, and write prediction CSV."""
    df_adm2 = pd.read_csv(cfg.infer_csv)
    missing = set(FEATURES) - set(df_adm2.columns)
    if missing:
        raise ValueError(f"Missing columns in inference CSV: {missing}")

    model = joblib.load(model_path)
    df_adm2["Predicted_gdpRatio"] = model.predict(df_adm2[FEATURES])

    output_csv_path = cfg.output_dir / cfg.output_csv
    df_adm2.to_csv(output_csv_path, index=False)
    print(f"Saved predictions to {output_csv_path}")
    return output_csv_path


def main() -> None:
    """Execute the full training, evaluation, plotting, and inference pipeline."""
    cfg = parse_args()
    cfg.output_dir.mkdir(parents=True, exist_ok=True)

    X_train, X_test, y_train, y_test, strat_test = load_and_split(cfg)

    best_models, results_df, best_model_name, best_model = train_models(
        X_train=X_train,
        y_train=y_train,
        X_test=X_test,
        y_test=y_test,
        random_seed=cfg.random_seed,
    )

    model_path = save_model_artifacts(
        cfg=cfg,
        best_model_name=best_model_name,
        best_model=best_model,
        results_df=results_df,
    )

    y_pred_best = best_model.predict(X_test)
    print("\nBest-model test metrics")
    print(f"R2   : {r2_score(y_test, y_pred_best):.4f}")
    print(f"MAE  : {mean_absolute_error(y_test, y_pred_best):.4f}")
    print(f"RMSE : {np.sqrt(mean_squared_error(y_test, y_pred_best)):.4f}")

    plot_pred_vs_true_by_model(
        best_models=best_models,
        model_order=results_df["model"].tolist(),
        X_test=X_test,
        y_test=y_test,
        output_path=cfg.output_dir / "pred_vs_true_by_model.png",
    )

    err_df, regions = plot_error_histograms(
        y_test=y_test,
        y_pred=y_pred_best,
        strat_test=strat_test,
        output_path=cfg.output_dir / "error_histograms_by_region.png",
    )

    plot_error_scatter_by_region(
        err_df=err_df,
        regions=regions,
        output_path=cfg.output_dir / "error_scatter_by_region.png",
    )

    print_bias_diagnostics(err_df=err_df, regions=regions)
    run_inference(cfg=cfg, model_path=model_path)


if __name__ == "__main__":
    main()
