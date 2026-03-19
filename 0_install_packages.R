### 0_install_packages.R
# Install all required packages and optionally run the full processing pipeline.
# Subnational GDP per capita dataset — Matti Kummu, Aalto University (matti.kummu@aalto.fi)

# List of required packages
packages <- c(
  "raster", "openxlsx", "zoo", "broom", "data.table", "sf", "terra", "tidyverse",
  "dplyr", "tidyterra", "scico", "tmap", "tidyr", "Rfast", "ggpubr", "gridExtra",
  "readxl", "rnaturalearth", "rmapshaper", "purrr", "mblm", "fasterize"
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Install required packages
invisible(lapply(packages, install_if_missing))

# Verify all packages load successfully
message("\nVerifying packages load correctly...")
failed <- c()
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    failed <- c(failed, pkg)
  }
}
if (length(failed) > 0) {
  stop("The following packages failed to load: ", paste(failed, collapse = ", "))
} else {
  message("All packages verified.")
}

# Load packages
library(raster)
library(openxlsx)
library(zoo)
library(broom)
library(data.table)
library(sf)
library(terra)
library(tidyverse)
library(dplyr)
library(tidyterra)
library(scico)
library(tmap)
library(tidyr)
library(Rfast)
library(ggpubr)
library(gridExtra)
library(readxl)
library(rnaturalearth)
library(rmapshaper)
library(purrr)
library(mblm)
library(fasterize)


#### Run full pipeline -----
# Set run_pipeline <- TRUE to execute all scripts sequentially.
# Scripts are sourced from the project root working directory.

run_pipeline <- FALSE

if (run_pipeline) {

  # Set working directory to project root
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  message("\n========================================")
  message("Starting full pipeline run")
  message("========================================\n")

  # 1. Prepare national (ADM0) GDP per capita data
  message("\n--- 1_gdp_prepare_adm0.R ---")
  source("1_gdp_prepare_adm0.R")

  # 2. Process subnational (ADM1) GDP data
  message("\n--- 2_gdp_prep_adm1.R ---")
  source("2_gdp_prep_adm1.R")

  # 2b. Brazil municipality → GADM ADM2 name matching
  message("\n--- 2_2_municipality_level_BRA.R ---")
  source("2_2_municipality_level_BRA.R")

  # 3. Prepare spatial data and rasterise values
  message("\n--- 3_gdp_prep_spatial.R ---")
  source("3_gdp_prep_spatial.R")

  # 4. Prepare data and train downscaling model
  message("\n--- 4_downscaling_train.R ---")
  source("4_downscaling_train.R")

  # 5. Downscaling prediction
  message("\n--- 5_downscaling_predict.R ---")
  source("5_downscaling_predict.R")

  # 5b. Uncertainty downscaling prediction
  message("\n--- 5_2_uncert_downscaling_predict.R ---")
  source("5_2_uncert_downscaling_predict.R")

  # 6. Calculate total GDP from GDP per capita and population rasters
  message("\n--- 6_gdpTotal.R ---")
  source("6_gdpTotal.R")

  # 7. Plot GDP maps
  message("\n--- 7_gdp_plot_maps.R ---")
  source("7_gdp_plot_maps.R")

  # 8. Collect dataset metadata
  message("\n--- 8_gdp_metadata_collect.R ---")
  source("8_gdp_metadata_collect.R")

  # 9. Validation at ADM2 level
  message("\n--- 9_adm2_validation.R ---")
  source("9_adm2_validation.R")

  # 10. Plot downscaling error maps
  message("\n--- 10_downscalingErrorToMap.R ---")
  source("10_downscalingErrorToMap.R")

  # 11. Store and round final output files
  message("\n--- 11_storeFinalFiles.R ---")
  source("11_storeFinalFiles.R")

  message("\n========================================")
  message("Full pipeline complete!")
  message("========================================")

}
