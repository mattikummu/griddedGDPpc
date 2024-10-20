
# prepare national data for GDP per cap

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

library(raster)
library(openxlsx)
library(zoo)
library(broom)

library(data.table)
library(sf)
library(terra)
library(mblm)

library(tidyverse)
library(dplyr) #

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set time steps
timestep <- c(seq(1990, 2022))
step <- c(seq(1,length(timestep)))


##### 1. read data -----

# 1.1. load cntry_info
cntry_info <- read_csv("data_in/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso_code = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso_code = ifelse(iso_code == 'XKX','KSV',iso_code)) %>%  
  # northern cyprus
  mutate(iso_code = ifelse(iso_code == 'XNC','ZNC',iso_code)) %>%  
  distinct(iso_code, .keep_all = T)


# 1.2 load national databases

# World Bank national data
adm0_WB_reg <- readxl::read_excel('data_in/WB_gdp_pc_ppp_2017USD.xls', skip = 0, 
                                  sheet = 'Metadata - Countries') %>% 
  filter(is.na(Region)) %>% 
  rename(iso3 = 'Country Code') 

adm0_WB <- readxl::read_excel('data_in/P_Data_Extract_From_World_Development_Indicators.xlsx', skip = 0) %>% 
  as_tibble() %>% 
  rename(iso3 = 'Country Code') %>% 
  mutate(iso3 = ifelse(iso3 == 'XKX', 'KSV', iso3)) %>% 
  select(-c('Series Name', 'Series Code', 'Country Name')) %>% 
  # filter out the regional values
  filter(!iso3 %in% adm0_WB_reg$iso3) %>% 
  set_names(c('iso3', paste0(1990:2022)))



# IMF national data
adm0_IMF <- readxl::read_excel('data_in/IMF_gdp_pc_ppp_2017USD.xlsx', skip = 3) %>% 
  rename(iso3 = 'Country - ISO') %>% 
  mutate(iso3 = ifelse(iso3 == 'UVK', 'KSV', iso3)) %>% 
  select(c(iso3, paste0(1990:2021)))


# CIA WF national data
adm0_CIA <- readxl::read_excel('data_in/IndexMundi_CIA_WF_gdp_pc_ppp.xlsx', skip = 8) %>% 
  select(-Country)


#### 2. combine data -----

# let's combine the WB, IMF and CIA data
# https://stackoverflow.com/questions/72940045/replace-na-in-a-table-with-values-in-column-with-another-table-by-conditions-in

adm0_comb <- adm0_WB %>% 
  # add countries from IMF that do not exist in WB (Taiwan, West Bank)
  bind_rows(adm0_IMF %>% filter(!iso3 %in% adm0_WB$iso3)) %>% 
  # add countries from CIA that do not exist in WB (Western Sahara)
  bind_rows(adm0_CIA %>% filter(!iso3 %in% adm0_WB$iso3)) %>% 
  # Replace NA values in WB from data in IMF
  dplyr::rows_patch(adm0_IMF, by = 'iso3') %>% 
  # Replace NA values in WB + IMF from data in CIA
  dplyr::rows_patch(adm0_CIA, by = 'iso3') %>% 
  filter(!is.na(iso3))


write_csv(adm0_comb, 'results/adm0_reported_data.csv')


adm0_source_WB <- adm0_WB %>% 
  pivot_longer(-'iso3', names_to = 'year', values_to = 'gdp_pc') %>% 
  drop_na() %>% 
  distinct(iso3) %>% 
  mutate(source = 'WB') 

adm0_source_IMF <- adm0_IMF %>% 
  filter(!iso3 %in% adm0_source_WB$iso3) %>% 
  pivot_longer(-'iso3', names_to = 'year', values_to = 'gdp_pc') %>% 
  drop_na() %>% 
  distinct(iso3) %>% 
  mutate(source = 'IMF') 

adm0_source_CIA <- adm0_CIA %>% 
  filter(!iso3 %in% adm0_source_WB$iso3) %>% 
  filter(!iso3 %in% adm0_source_IMF$iso3) %>% 
  pivot_longer(-'iso3', names_to = 'year', values_to = 'gdp_pc') %>% 
  drop_na() %>% 
  distinct(iso3) %>% 
  mutate(source = 'CIA') 
  
adm0_source_comb <- bind_rows(adm0_source_WB, adm0_source_IMF, adm0_source_CIA) %>% 
  arrange(iso3)

write_csv(adm0_source_comb, 'results/adm0_source_comb.csv')

#### 3. interpolation and extrapolation -----

adm0_comb_long <- adm0_comb %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'gdp_pc')

# load function

source('functions/f_interpExtrap_adm0.R')


# calculate centroids of each country

if (file.exists('data_GIS/p_adm0_centroids.gpkg')) {
  p_adm0_centroids <- vect('data_GIS/p_adm0_centroids.gpkg')
  
} else { # create it
  v_cntryGIS <- terra::simplifyGeom(vect('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer = 'ADM_0'))
  v_cntryGIS_EE <- project(v_cntryGIS, '+proj=eqearth')
  terra::writeVector(v_cntryGIS, 'data_GIS/v_cntryGISsimplif.gpkg')
  terra::writeVector(v_cntryGIS_EE, 'data_GIS/v_cntryGISsimplif_equalEarthProj.gpkg')
  p_adm0_centroids <- terra::centroids(v_cntryGIS_EE)
  terra::writeVector(p_adm0_centroids, 'data_GIS/p_adm0_centroids.gpkg')
}



# apply function

gdp_pc <- f_interpExtrap_adm0(nameIndic = 'gdp_pc')

gdp_pc_wide <- gdp_pc %>% 
  pivot_wider(names_from = 'year', values_from = 'gdp_pc')


# write results

fwrite(gdp_pc, 'results/adm0_gdp_pc_long_interpExtrap.csv')
fwrite(gdp_pc_wide, 'results/adm0_gdp_pc_wide_interpExtrap.csv')





#### 4. preparation of puting data to polygons and raster -----

# 4.1 read data

adm0_comb_interpExtrap <- read.csv( 'results/adm0_gdp_pc_long_interpExtrap.csv')

n_cntry <- adm0_comb_interpExtrap %>% 
  distinct(iso3)

# 4.2 modify data

# some of the adm1 levels are divided to those that are officially in a country and those that are 
# on conflict zones (between CHN, IND and PAK)


adm0_polyg <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
  mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) %>% # kosovo to correct iso3
  rename(iso3 = GID_0) # %>% 


# for China, Pakistan, India let's use older version of GADM so that these Z areas
# will be correctly represented. Also Hong Kong and Macao will this way be there as
# individual countries

adm0_gadm_old <- read_sf('/Users/mkummu/R/migration_data_bee/data_in/gadm_level0.gpkg') %>% 
  rename(iso3 = GID_0) %>% 
  rename(COUNTRY = NAME_0) %>% 
  filter(iso3 %in% c('CHN', 'PAK', 'IND', 'HKG', 'MAC'))

sf_adm0_polyg_diss <- adm0_polyg %>% 
  filter(!iso3 %in% c('CHN', 'PAK', 'IND', 'HKG', 'MAC')) %>% 
  bind_rows(adm0_gadm_old)


# 4.3 join cntry info

adm0_polyg_final <- sf_adm0_polyg_diss %>% 
  #st_drop_geometry() %>% 
  rename(Country = COUNTRY) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO','KSV',iso3)) %>% 
  filter(iso3 != 'ALA' & iso3 != 'XCA' & iso3 != 'ATA') %>%  # remove Åland (part of Finland); Caspian Sea (not needed), Antarctica
  mutate(GID_nmbr = paste0(iso3,'t')) %>% 
  left_join(cntry_info[,c(2,4)] %>%  rename(iso3 = iso_code)) %>% 
  mutate(GID_nmbr = cntry_id) %>% 
  filter(!is.na(GID_nmbr)) %>% 
  select(Country, GID_nmbr, iso3,GID_nmbr, geom)


temp <- adm0_polyg_final %>% 
  st_drop_geometry()


### 5. put data to raster -----------------------------------------------------


# 5.1 create adm0  raster

if (file.exists('data_gis/gdp_Adm0_raster_5arcmin.tif')){
  # load it
  r_gdp_adm0_polyg_5arcmin <- rast('data_gis/gdp_Adm0_raster_5arcmin.tif')
} else { 
  # create it
  
  #create ref raster
  # ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)
  ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)
  # rasterise to 1 arc min resolutions
  
  r_gdp_adm0_polyg_1arcmin <-  rasterize(adm0_polyg_final,ref_raster_1arcmin,field="GID_nmbr")
  
  
  # aggregate to 5 arc-min
  r_gdp_adm0_polyg_5arcmin <- terra::aggregate(r_gdp_adm0_polyg_1arcmin,fact=5,fun=modal,na.rm=T)
  
  
  # write raster
  terra::writeRaster(r_gdp_adm0_polyg_5arcmin,'data_gis/gdp_adm0_raster_5arcmin.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
}

# load function

source("functions/f_gdp_data2raster_adm0.R")

varNames <- c('gdp_pc' )

for (iVar in 1:length(varNames)) {
  
  rast_varName <- f_gdp_data2raster_adm0(inYears = 1990:2022, 
                                        IndexName = varNames[iVar], 
                                        inDataAdm0 = adm0_comb_interpExtrap) 
  
}



### 6. put data to polygons (gpkg file) ----

# 6.1 create polygon layer

if (file.exists('data_gis/gdp_adm0_polyg_simple.gpkg')){
  # load it
  gdp_adm0_polyg_simpl <- st_read('data_gis/gdp_adm0_polyg_simple.gpkg') 
} else { 
  # create it
  
  p <- as.polygons(r_gdp_adm0_polyg_5arcmin)
  as.data.frame(p)
  
  writeVector(p, 'data_gis/gdp_adm0_polyg_simple.gpkg', overwrite=T)
  
  gdp_adm0_polyg_simpl <- st_read('data_gis/gdp_adm0_polyg_simple.gpkg') 
  
}



# 6.2 put data to gpkg (and slope to raster)

source('functions/f_gdp_data2gpkg_adm0.R')


varNames <- c('gdp_pc')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- f_gdp_data2gpkg_adm0(inYears = 1990:2022, 
                                      IndexName = varNames[iVar], 
                                      inDataAdm0 = adm0_comb_interpExtrap) 
  
}





