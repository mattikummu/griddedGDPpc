
# plot the error maps

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

library(sf)
library(terra)
library(tidyterra)
library(openxlsx) #

library(scico)
library(tmap)

library(broom)
library(tidyr)
library(tidyverse)
library(dplyr) 



# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1 load data -----


# 1.1 load cntry_info
cntry_info <- read_csv("data_in/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso3 = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso3 = ifelse(iso3 == 'XKX','KSV',iso3)) %>%  
  # northern cyprus
  mutate(iso3 = ifelse(iso3 == 'XNC','ZNC',iso3)) %>%  
  distinct(iso3, .keep_all = T)

v_subnat_gis_combined <- vect("results/polyg_adm1_gdp_perCapita_1990_2022.gpkg")
sf_subnat_gis_combined <- read_sf("results/polyg_adm1_gdp_perCapita_1990_2022.gpkg")
# 
# temp_sf_subnat_gis_combined <- sf_subnat_gis_combined %>% 
#   st_drop_geometry()

df_adm0_data <- read_csv("results/adm0_gdp_pc_long_interpExtrap.csv")


## 1.2 read error data

error_predicted <- read.csv('downscalingMatlab/adm1_prediction_error.csv') %>% 
  as_tibble() %>% 
  rename(Error = absolute_error) %>% 
  rename(predicted = ens_out)

df_error_maxYear <- error_predicted %>% 
  as_tibble() %>% 
  select(GID_nmbr, Error, year, predicted) %>% 
  # find out latest year in each admin area
  group_by(GID_nmbr) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  rename(errorLastYear = Error) %>% 
  mutate(errorLastYearRel = errorLastYear / predicted) %>% 
  select(-year, - predicted)

df_error_minYear <-error_predicted %>% 
  as_tibble() %>% 
  select(GID_nmbr, Error, year, predicted) %>% 
  # find out latest year in each admin area
  group_by(GID_nmbr) %>% 
  filter(year == min(year)) %>% 
  ungroup()  %>% 
  rename(errorFirstYear = Error) %>% 
  mutate(errorFirstYearRel = errorFirstYear / predicted) %>% 
  select(-year, - predicted)

gdp_adm0adm1_polyg <- st_read('data_gis/gdp_adm0adm1_polyg_feb2024.gpkg')

gdp_adm0adm1_polyg_simpl <- st_read('data_gis/gdp_Adm0Adm1_polyg_simple.gpkg')

sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')

## 2. error to sf ----

sf_gdp_adm0adm1_polyg_simpl_Maxerror <- gdp_adm0adm1_polyg_simpl %>% 
  left_join(df_error_minYear) %>% 
  left_join(df_error_maxYear) 


## 3. plot maps ----

source('functions/f_Plot_errorMap.R')

# 3.1 get the thresholds

minError <- quantile(c(df_error_minYear$errorFirstYear, df_error_maxYear$errorLastYear), .025, na.rm = T)
maxError <- quantile(c(df_error_minYear$errorFirstYear, df_error_maxYear$errorLastYear), .975, na.rm = T)

minErrorRel <- quantile(c(df_error_minYear$errorFirstYearRel, df_error_maxYear$errorLastYearRel), .025, na.rm = T)
maxErrorRel <- quantile(c(df_error_minYear$errorFirstYearRel, df_error_maxYear$errorLastYearRel), .975, na.rm = T)

errorRange <- seq(round(minError,1),round(maxError,1),by=.025) 
errorRelRange <- seq(round(minErrorRel,1),round(maxErrorRel,1),by=.025) 

p_df_error_firstYear <- f_Plot_errorMap(sf_gdp_adm0adm1_polyg_simpl_Maxerror,'errorFirstYear',errorRange)
  p_df_error_lastYear <- f_Plot_errorMap(sf_gdp_adm0adm1_polyg_simpl_Maxerror,'errorLastYear',errorRange)


p_df_errorRel_firstYear <- f_Plot_errorMap(sf_gdp_adm0adm1_polyg_simpl_Maxerror,'errorFirstYearRel',errorRelRange)
p_df_errorRel_lastYear <- f_Plot_errorMap(sf_gdp_adm0adm1_polyg_simpl_Maxerror,'errorLastYearRel',errorRelRange)


# 3.2 store maps

if (dir.exists('figures/figErrorMap/')) {
  
} else {
  dir.create('figures/figErrorMap/')  
}

layers <- list(p_df_error_firstYear, p_df_error_lastYear, 
               p_df_errorRel_firstYear, p_df_errorRel_lastYear)

nameLayers <- c('p_df_error_firstYear', 'p_df_error_lastYear', 
                'p_df_errorRel_firstYear', 'p_df_errorRel_lastYear')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/figErrorMap/fig_',nameLayers[i],'.png'),width = 400, units='mm', dpi = 600)
  
}

p_errorMaps <- tmap_arrange(p_df_error_firstYear, p_df_error_lastYear,
                             ncol = 2)

tmap_save(p_errorMaps,filename = paste0('figures/figErrorMap/fig_error_maps','.pdf'),
          width = 180, height=90, units='mm')

