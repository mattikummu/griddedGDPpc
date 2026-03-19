# GDP Downscaling Pipeline

A clean, reproducible Python pipeline for training and comparing multiple regressors to downscale GDP ratios, generating publication-ready diagnostics, and exporting ADM2 predictions.

## Features

- Trains and tunes 4 models with `GridSearchCV`:
  - XGBoost (`xgb`)
  - LightGBM (`lgbm`)
  - CatBoost (`catb`)
  - HistGradientBoosting (`hgb`)
- Uses a consistent preprocessing pipeline (median imputation).
- Performs stratified train/test split by region (`RegName`).
- Produces figures for:
  - Predicted vs true (all models, subplot panel)
  - Region-wise histogram of errors
  - Region-wise scatter of errors
- Saves:
  - Best model as `.joblib`
  - Metadata JSON with model scores
  - ADM2 predictions CSV

## Repository Files

- `gdp_downscaling_pipeline.py`: main script
- `adm1DataForDownscaling_ratio.csv`: training dataset (ADM1)
- `adm2DataForDownscaling_ratio.csv`: inference dataset (ADM2)

## Requirements

Python 3.10+ recommended.

Install dependencies:

```bash
pip install numpy pandas scipy matplotlib seaborn scikit-learn xgboost lightgbm catboost joblib
```

## Input Schema

### Required columns in training data (`--train-csv`)

- `adm0GDP`
- `adm0gini`
- `adm0travelTime`
- `adm0urb`
- `travelTimeRatio`
- `urbRatio`
- `gdpRatio` (target)
- `RegName` (stratification column)

### Required columns in inference data (`--infer-csv`)

- `adm0GDP`
- `adm0gini`
- `adm0travelTime`
- `adm0urb`
- `travelTimeRatio`
- `urbRatio`

## How to Run

1. Open a terminal in the project folder.
2. (Optional but recommended) create and activate a virtual environment.
3. Install dependencies.
4. Run the pipeline.

### 1) Open Project Folder

```bash
# after cloning, move into the repository root
cd <repository-folder>
```

### 2) Create and Activate Virtual Environment (Optional)

Windows PowerShell:

```powershell
python -m venv .venv
.\.venv\Scripts\Activate.ps1
```

macOS/Linux (bash/zsh):

```bash
python -m venv .venv
source .venv/bin/activate
```

### 3) Install Dependencies

```bash
pip install -r requirements.txt
```

### 4) Run Script

Run with defaults:

```bash
python gdp_downscaling_pipeline.py
```

Run with explicit paths:

```bash
python gdp_downscaling_pipeline.py --train-csv adm1DataForDownscaling_ratio.csv --infer-csv adm2DataForDownscaling_ratio.csv --output-dir . --output-csv adm2DataWithPredictions.csv --test-size 0.20 --random-seed 42
```

### 5) Check Outputs

After completion, check the files listed in the `Outputs` section below.

## Outputs

Written to `--output-dir`:

- `best_model_<model>.joblib`
- `best_model_<model>_meta.json`
- `pred_vs_true_by_model.png`
- `error_histograms_by_region.png`
- `error_scatter_by_region.png`
- `<output-csv>` (default: `adm2DataWithPredictions.csv`)

## Reproducibility Notes

- Seed-controlled where supported (`--random-seed`).
- Training data is trimmed for extreme target outliers using quantiles:
  - low = `0.0025`
  - high = `0.9975`
- Model ranking is based on test-set `R2` after CV tuning.

## Script Structure

Main stages inside `gdp_downscaling_pipeline.py`:

1. Parse arguments and configuration
2. Load/validate/split data
3. Train and tune candidate models
4. Save best model and metadata
5. Evaluate and generate figures
6. Run ADM2 inference and export predictions

## License

This project is released under the **BSD 3-Clause License**, a permissive and
widely used license in scientific open-source software.

See the full text in `LICENSE`.
