# Downscaled gridded global dataset for Gross Domestic Product (GDP) per capita at purchasing power parity (PPP) over 1990–2024

## Citation

These codes were used to create the dataset published in:

> Kummu, M. et al. (2025). Downscaled gridded global dataset for Gross Domestic Product (GDP) per capita at purchasing power parity (PPP) over 1990–2022. *Scientific Data*, 12, 567. https://doi.org/10.1038/s41597-025-04487-x

**Please cite this paper if you use the code or data.**

---

## Overview

This repository contains the R and Matlab code used to produce a global, gridded GDP per capita (PPP, constant 2021 USD) dataset at multiple spatial resolutions (ADM0, ADM1, ADM2, 5 arc-min, 30 arc-min) for the period 1990–2024.

Note:
- the dataset itself is available separately on Zenodo; see "Data availability" section below.
- we have updated the dataset to include the years 2023-2024, and the code in this repository reflects those updates.

The processing pipeline:
1. Compiles and gap-fills national (ADM0) GDP per capita from World Bank, IMF, and CIA World Factbook sources.
2. Compiles subnational (ADM1) GDP per capita from DOSE v2, OECD TL2/TL3, Eurostat NUTS2, and country-specific sources.
3. Rasterises ADM0 and ADM1 data and combines them into a consistent global grid.
4. Trains a downscaling model (Matlab) using population, urbanisation rate, and Gini inequality as predictors.
5. Predicts GDP per capita at ADM2 resolution (5 arc-min).
6. Derives total GDP rasters by multiplying GDP per capita by population count (GHS-POP).
7. Produces validation statistics, metadata, and publication-quality maps.

---

## Data availability

**Input and output data** are available on Zenodo:
https://doi.org/10.5281/zenodo.10976733

Extract `code_input_data.zip` into the same folder as the scripts (it will unpack as `code_input_data/`).

The final gridded results are also available in the same Zenodo repository.

Please note: some large GIS and raster files (e.g., GADM boundaries, travel time raster) are not included in the Zenodo archive due to size constraints. These must be downloaded separately from their original sources (see "External data dependencies" section below).

---

## Software requirements

- **R** ≥ 4.3 (developed with 4.3.2). See `0_install_packages.R` for the full package list.
- **Matlab** ≥ 2023b for the downscaling step (see `downscalingMatlab/` for its own README).

Run `0_install_packages.R` first to install all required R packages. To execute the full pipeline automatically, set `run_pipeline <- TRUE` in that script.

---

## Pipeline scripts

The scripts are numbered in the order they should be run.

| Script | Description |
|--------|-------------|
| `0_install_packages.R` | Installs required R packages. Set `run_pipeline <- TRUE` to run the full pipeline sequentially. |
| `1_gdp_prepare_adm0.R` | Compiles national (ADM0) GDP per capita from World Bank, IMF, and CIA sources; interpolates and extrapolates missing values; rasterises to 5 arc-min. |
| `2_gdp_prep_adm1.R` | Compiles subnational (ADM1) GDP per capita from DOSE v2, OECD TL2/TL3, Eurostat NUTS2, and individual country datasets; interpolates missing values; derives population-weighted ADM0 aggregates. |
| `2_2_municipality_level_BRA.R` | Matches Brazilian municipality GDP data to GADM ADM2 units using exact and fuzzy name matching; outputs a municipality–ADM2 correspondence table. |
| `2_2_prefecture_level_CHN.r` | Prepares China prefecture-level GDP and GNI data (2010–2023) with population-weighted downscaling. |
| `3_gdp_prep_spatial.R` | Combines ADM0 and ADM1 data into a global GeoPackage and raster stack; creates the combined ADM0+ADM1 polygon layer used in downscaling. |
| `4_downscaling_train.R` | Prepares training data (population, urbanisation, Gini, travel time) for the neural-network downscaling model. |
| `5_downscaling_predict.R` | Applies the trained downscaling model to predict GDP per capita at ADM2 resolution (5 arc-min) for 1990–2024. |
| `5_2_uncert_downscaling_predict.R` | Uncertainty estimation for the downscaling prediction. |
| `downscalingMatlab/` | Matlab code for training and running the neural-network downscaling model (see folder README). |
| `6_gdpTotal.R` | Calculates total GDP (PPP) by multiplying ADM2 GDP per capita by GHS-POP population counts; produces outputs at 30 arc-sec (selected years), 5 arc-min, and 30 arc-min resolution for 1990–2024. |
| `7_gdp_plot_maps.R` | Produces maps shown in the manuscript. |
| `8_gdp_metadata_collect.R` | Collects and summarises dataset metadata (data sources, country coverage, year ranges). |
| `9_adm2_validation.R` | Validates ADM2-level GDP per capita against independent subnational data. |
| `10_downscalingErrorToMap.R` | Maps the spatial distribution of downscaling errors. |
| `11_storeFinalFiles.R` | Rounds and writes the final output GeoTIFFs. |
| `functions/` | Helper functions used across the pipeline. |

---

## External data dependencies

Several large GIS and raster files are not included in the Zenodo archive and must be downloaded separately.

### GADM administrative boundaries

| File path used in code | Download |
|------------------------|----------|
| `/Users/.../GIS_data_common/gadm_410-levels.gpkg` | [GADM 4.1](https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip) |
| `/Users/.../migration_data_bee/data_in/gadm_level0.gpkg` | [GADM 3.6 levels](https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_levels_gpkg.zip) |
| `/Users/.../migration_data_bee/data_in/gadm_lev1.gpkg` | Same GADM 3.6 archive above |
| `/Users/.../migration_data_bee/data_in/OECD_TL3_2020_fixed_valid.gpkg` | Via Zenodo input archive |

### Natural Earth country boundaries

| File path used in code | Download |
|------------------------|----------|
| `/Users/.../GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp` | [Natural Earth 50m cultural vectors](https://www.naturalearthdata.com/downloads/50m-cultural-vectors/) — or use the `rnaturalearth` R package ([vignette](https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html)) |

### Travel time raster

| File path used in code | Download |
|------------------------|----------|
| `/Users/.../GIS_data_common/travel_time_cities/travel_time_to_cities_11.tif` | [Weiss et al. 2018 – Figshare](https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134) |

### DOSE subnational shapefiles

| File path used in code | Download |
|------------------------|----------|
| `data_gis/DOSE_shapefiles.gpkg` | [DOSE v2 – Zenodo](https://zenodo.org/records/7573249) |

### Gini inequality raster

| File path used in code | Download |
|------------------------|----------|
| `/Users/.../subnatGini/results/rast_gini_disp_1990_2023.tif` | [Subnational Gini dataset – Zenodo](https://zenodo.org/records/15055369) (file: `rast_adm1_gini_disp_1990_2021.tif`) |

### GHS-POP population rasters

| File path used in code | Download |
|------------------------|----------|
| `/Users/.../GIS_data_common/GHS_POP/` (individual epoch TIFFs) | [GHS-POP – JRC](https://ghsl.jrc.ec.europa.eu/download.php?ds=pop) |
| `data_gis/r_pop_GHS_1989_2024_5arcmin.tif` | Derived from GHS-POP by script `2_gdp_prep_adm1.R` (auto-generated if absent) |

---

## Key intermediate and output files

| File | Description |
|------|-------------|
| `results/adm0_gdp_pc_long_interpExtrap.csv` | National GDP per capita, interpolated/extrapolated, long format |
| `results/adm1_gdp_ratio_interp_feb2024.csv` | Subnational GDP ratio relative to national, interpolated |
| `results/gisData_GDP_pc_combined_feb2024.gpkg` | Combined ADM1 GeoPackage with GDP per capita values |
| `results/rast_adm2_gdp_perCapita_1990_2024.tif` | ADM2-level GDP per capita raster stack (35 layers, 5 arc-min) |
| `results_final/rast_gdpTot_1990_2024_5arcmin.tif` | Total GDP raster stack, 5 arc-min resolution |
| `results_final/rast_gdpTot_1990_2024_30arcmin.tif` | Total GDP raster stack, 30 arc-min resolution |
| `results_final/rast_gdpPerCap_1990_2024_30arcmin.tif` | GDP per capita raster stack, 30 arc-min resolution |
| `results_final/rast_gdpTot_1990_2024_30arcsec.tif` | Total GDP raster, 30 arc-sec (selected 5-year steps + 2025 pop) |

---

## Contact

For questions, please contact **Matti Kummu** (matti.kummu@aalto.fi), Aalto University.
