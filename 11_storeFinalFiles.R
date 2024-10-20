

### store final files, round them

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

library(terra)
library(sf)

library(dplyr)
library(tidyverse)

library(tidyterra)

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### 1. read files -----

yearsIn <- 1990:2022


r_adm0 <- rast(paste0('results/rast_gdp_pc_adm0_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                             '.tif') ) 

r_adm1 <- rast(paste0('results/rast_adm1_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                             '.tif') ) 

r_adm2 <- rast(paste0('results/rast_adm2_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                             '.tif') ) 

r_adm2_30arcmin <- rast(paste0('results/rast_gdpPerCap_1990_2020_30arcmin.tif') ) 

p_adm0 <- st_read(paste0('results/polyg_gdp_pc_adm0_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm0 <- vect(paste0('results/polyg_gdp_pc_adm0_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

p_adm1 <- st_read(paste0('results/polyg_adm1_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm1 <- vect(paste0('results/polyg_adm1_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

p_adm2 <- st_read(paste0('results/polyg_adm2_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))
v_adm2 <- vect(paste0('results/polyg_adm2_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))


#### 2. write rasters ----


# create folder if not exist

if (dir.exists('results_final/')) {
  
} else {
  dir.create('results_final/')  
}


terra::writeRaster(round(r_adm0),paste0('results_final/rast_adm0_gdp_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(round(r_adm1),paste0('results_final/rast_adm1_gdp_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(round(r_adm2),paste0('results_final/rast_adm2_gdp_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(round(r_adm2_30arcmin),paste0('results_final/rast_adm2_gdp_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                        '_30arcmin.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)


#### 3. round and write polygons ----


v_gdp_pc_adm0 <- v_adm0 %>% 
  mutate(across(where(is.numeric), ~round(., 0))) %>% 
  select(-cntry_id) 

v_gdp_pc_adm1 <- v_adm1 %>% 
  mutate(across(where(is.numeric), ~round(., 0)))

v_gdp_pc_adm2 <- v_adm2 %>% 
  mutate(across(where(is.numeric), ~round(., 0))) %>% 
  select(-rowNmb)



writeVector(v_gdp_pc_adm0,paste0('results_final/polyg_adm0_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)

writeVector(v_gdp_pc_adm1,paste0('results_final/polyg_adm1_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)

writeVector(v_gdp_pc_adm2,paste0('results_final/polyg_adm2_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'), overwrite=T)



#### 4.  write tabulated results ----

t_adm0 <- as_tibble(v_gdp_pc_adm0)
t_adm1 <- as_tibble(v_gdp_pc_adm1)
t_adm2 <- as_tibble(v_gdp_pc_adm2)


write_csv(t_adm0, 'results_final/tabulated_adm0_gdp_perCapita.csv')
write_csv(t_adm1, 'results_final/tabulated_adm1_gdp_perCapita.csv')
write_csv(t_adm2, 'results_final/tabulated_adm2_gdp_perCapita.csv')


