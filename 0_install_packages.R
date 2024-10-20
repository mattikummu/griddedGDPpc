## install all required packages

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

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