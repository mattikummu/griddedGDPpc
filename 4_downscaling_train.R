

### prepare data for downscaling 

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)


library(sf)
library(terra)
library(openxlsx) #

library(broom)
library(tidyr)
library(tidyverse)
library(dplyr) 
library(tidyterra)


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1. load data -----

cntry_info <- read_csv("data_in/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso3 = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso3 = ifelse(iso3 == 'XKX','KSV',iso3)) %>% 
  # northern cyprus
  mutate(iso3 = ifelse(iso3 == 'XNC','ZNC',iso3)) %>%  
  distinct(iso3, .keep_all = T)

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(iso3, .keep_all = T) %>% 
  left_join(cntry_info %>% select(iso3, cntry_id)) %>% 
  select(-cntry_code)

v_subnat_gis_combined <- vect("results/polyg_adm1_gdp_perCapita_1990_2022.gpkg")
sf_subnat_gis_combined <- read_sf("results/polyg_adm1_gdp_perCapita_1990_2022.gpkg")

df_adm0_data <- read_csv("results/adm0_gdp_pc_long_interpExtrap.csv")


v_subnat_gis_adm1 <- v_subnat_gis_combined %>% 
  filter(GID_nmbr > 1000)

terra::gdalCache(10000)

##### 2. prepare downscaling data -----


# 2.1 independent variables for downscaling 

# population data created in 2_gdp_prep_adm1.R
r_popCount_mod <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')

r_urbanisation <- subset(rast('/Users/mkummu/R/misc/percentileNormalisation/output/urbanisationCntryWise_GHS2023a.tif'),1:33)

sf_CntryUrbRate <- read.csv("/Users/mkummu/R/misc/percentileNormalisation/output/urbRateNational.csv") %>% 
  as_tibble() %>% 
  select(-c(X, Country, cntry_code, GADM_code)) %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'cntryUrbRatio') %>% 
  mutate(year = as.numeric(substring(year,2)))

# 2.2 target values

# subnat gdp ratio over national value
sf_adm1gdpReported <- read.csv('results/subnat_gis_combined_feb2024.csv') %>% 
  pivot_longer(-c(names(.[1:6])), names_to = 'year', values_to = 'gdpRatio') %>% 
  filter(!year == 'X1989')

sf_adm1gdpInterp <- read.csv('results/adm1_gdp_ratio_interp_feb2024.csv') %>% 
  as_tibble() %>% 
  #rename(year = name, gdpRatio = value) %>% 
  filter(!year == '1989') 


##### 3. sample data for training ----

n_sample = 10

set.seed(21)

st_data_available_all <- sf_adm1gdpReported %>% 
  filter(GID_nmbr > 1000) %>% # filter out if national values are included
  filter(!is.na(gdpRatio)) %>% 
  group_by(iso3) %>% 
  summarise(merged_column = paste(year)) %>% 
  distinct(.keep_all = T) %>%
  ungroup() %>% 
  group_by(iso3) %>% 
  
  ## this samples three samples of each country
  summarize(sampled_years = list({
    unique_years <- unique(merged_column)
    num_unique_years <- length(unique_years)
    if (num_unique_years == 1) {
      rep(unique_years, n_sample)
    } else if (num_unique_years %in% c(2:(n_sample-1))) {
      sample(unique_years, n_sample, replace = TRUE)
    } else {
      sample(unique_years, n_sample)
    }
  })) %>%
  unnest(cols = sampled_years) %>%
  arrange(iso3,sampled_years) %>%
  

  # add ID
  mutate(ID = paste0(iso3, "_", sampled_years)) %>% 
  ungroup()

# 
# # for western Europe, let's use only 1/5 of the reported valúes instead of 1/2, as there is overrepresentation of reported values
# st_data_available_WE <- st_data_available_all %>% 
#   left_join(cntry_info) %>% 
#   filter(region_id == 12) %>% 
#   group_by(iso3) %>% 
#   slice_sample(prop=0.4) %>% 
#   ungroup() %>% 
#   select(iso3, merged_column, ID)

# # combine
# st_data_available <- st_data_available_all %>% 
#   filter(!iso3 %in%unique(st_data_available_WE$iso3)) %>% 
#   bind_rows(st_data_available_WE) %>% 
#   arrange(ID)
#  

st_data_available <- st_data_available_all


# select only the sampled admin area - yr combinations
adm1_units_for_downscaling <- sf_adm1gdpInterp %>% 
  mutate(ID = paste0(iso3, "_X", year)) %>% 
  filter(ID %in% st_data_available$ID) %>% 
  select(iso3, GID_nmbr, year, gdpRatio)




##### 4. get data for each adm1 unit -----



# 4.1 extract data to adm1 boundaries

if (file.exists('results/ext_data_adm1_mar2024.RData')){
  # load it
  load('results/ext_data_adm1_mar2024.RData') 
} else { #create it
  
  ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)
  
  r_popCount <- r_popCount_mod
  
  r_urbanisation_ext <- extend(r_urbanisation,ref_raster_5arcmin)
  ext(r_urbanisation_ext) <- ext(ref_raster_5arcmin)
  
  r_gini <- rast('/Users/mkummu/R/gini/results/rast_gini_disp_1990_2021.tif')
  
  r_travelTime <- rast('/Users/mkummu/R/GIS_data_common/travel_time_cities/travel_time_to_cities_11.tif')
  r_travelTime_crop <- aggregate(crop(r_travelTime,ext(subset(r_popCount,1))), fact=10, fun='mean', na.rm=T)
  r_travelTime_crop <- extend(r_travelTime_crop, subset(r_popCount,1))
  
  
  sf_gdpUnits <- sf_subnat_gis_combined %>% 
    mutate(GID_nmbr = as.character(GID_nmbr))
  
  v_gdpUnits <- vect(sf_gdpUnits)
  
  r_adm1adm0comb <- rasterize(v_gdpUnits, subset(r_popCount,1), field = 'GID_nmbr')
 
  
  v_subnat_gis_adm1 <- v_subnat_gis_combined %>% 
    filter(GID_nmbr > 1000)
  
  # extract values to adm1 areas. For some, temporal scale do not match with population, so we 
  # repeated the last year to cover the missing last years 
  
  ext_pop_adm1 <- terra::extract(r_popCount,v_subnat_gis_adm1, fun = sum, na.rm=T)
  ext_urb_x_pop_adm1 <- terra::extract(r_popCount*r_urbanisation_ext,
                                           v_subnat_gis_adm1, fun = sum, na.rm=T)  
  ext_gini_x_pop_adm1 <- terra::extract(r_popCount*
                                          c(r_gini, subset(r_gini,32)),
                                            v_subnat_gis_adm1, fun = sum, na.rm=T)
  ext_travelTime_x_pop_adm1 <- terra::extract(r_popCount*r_travelTime_crop,v_subnat_gis_adm1, fun = sum, na.rm=T)  
  
  save(ext_pop_adm1, ext_urb_x_pop_adm1, ext_gini_x_pop_adm1,
       ext_travelTime_x_pop_adm1,  
       file = "results/ext_data_adm1_mar2024.RData")
}


## 4.2 to long format

adm1_polyg_comb <- st_as_sf(v_subnat_gis_adm1) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(GID_nmbr)


adm1_polyg_comb_gdp_pc <- st_as_sf(v_subnat_gis_adm1) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(GID_nmbr, paste0(1990:2022)) %>% 
  pivot_longer(-c('GID_nmbr'), names_to = 'year', values_to = 'adm1gdp_pc')


adm1_polyg_comb_pop <- adm1_polyg_comb %>% 
  bind_cols(ext_pop_adm1) %>% 
  select(-c(ID)) %>% 
  set_names('GID_nmbr',  paste0(1990:2022) ) %>% 
  pivot_longer(-c('GID_nmbr'), names_to = 'year', values_to = 'pop')


adm1_polyg_comb_urb <- adm1_polyg_comb %>% 
  bind_cols(ext_urb_x_pop_adm1 / ext_pop_adm1) %>% 
  select(-c(ID)) %>% 
  # # for year 2021, let's use year 2020
  # mutate(yr2021 = ppp_2020) %>% 
  set_names('GID_nmbr',  paste0(1990:2022) ) %>% 
  pivot_longer(-c('GID_nmbr'), names_to = 'year', values_to = 'urb')


adm1_polyg_comb_gini <- adm1_polyg_comb %>% 
  bind_cols(ext_gini_x_pop_adm1 / ext_pop_adm1 )  %>% 
  select(-c(ID)) %>% 
  set_names('GID_nmbr',  paste0(1990:2022) ) %>% 
  pivot_longer(-c('GID_nmbr'),  names_to = 'year', values_to = 'gini')

dim(ext_travelTime_x_pop_adm1)
dim(ext_pop_adm1)

#ext_travelTime_x_pop_adm1[,33] <- ext_travelTime_x_pop_adm1[,32]

adm1_polyg_comb_traveTime <- adm1_polyg_comb %>% 
  bind_cols(ext_travelTime_x_pop_adm1 / ext_pop_adm1 ) %>% 
  select(-c(ID)) %>% 
  set_names( 'GID_nmbr', paste0(1990:2022) ) %>% 
  pivot_longer(-c('GID_nmbr'),  names_to = 'year', values_to = 'travelTime')

adm1_polyg_comb_adm0unit <- adm1_polyg_comb %>% 
  left_join(st_as_sf(v_subnat_gis_adm1) %>% 
              st_drop_geometry() %>% 
              as_tibble() %>% 
              select(GID_nmbr, iso3) )


# 4.3combine all

adm1_polyg_comb_wData <- adm1_polyg_comb_urb %>% 
  left_join(adm1_polyg_comb_gdp_pc) %>% 
  left_join(adm1_polyg_comb_gini) %>% 
  left_join(adm1_polyg_comb_traveTime) %>% 
  left_join(adm1_polyg_comb_adm0unit) %>% 
  left_join(adm1_polyg_comb_pop) %>% 
  left_join( st_as_sf(v_subnat_gis_adm1) %>% 
               st_drop_geometry() %>% 
               as_tibble() %>% 
               select(GID_nmbr, iso3, Subnat)) %>% 
  select(iso3, GID_nmbr, Subnat, everything())


# 4.4  adm0 data

adm0_gdp_pc <- read.csv('results/adm0_gdp_pc_long_interpExtrap.csv') %>% 
  as_tibble() %>% 
  mutate(year = as.character(year))


# adm0 urb

adm0urb <- adm1_polyg_comb_wData %>% 
  mutate(urb_x_pop = urb*pop) %>% 
  group_by(iso3, year) %>% 
  summarise(urbAdm0 = sum(urb_x_pop, na.rm=T)/sum(pop, na.rm=T) )

# adm0 traveltime

adm0travelTime <- adm1_polyg_comb_wData %>% 
  mutate(TT_x_pop = travelTime*pop) %>% 
  group_by(iso3, year) %>% 
  summarise(travelTimeAdm0 = sum(TT_x_pop, na.rm=T) / sum(pop, na.rm=T))


# adm0 gini

adm0gini <- read.csv('/Users/mkummu/R/gini/results/gini_adm0_DispMkt_extrap.csv') %>% 
  as_tibble() %>% 
  select(-gini_mkt) %>% 
  mutate(year = as.character(year))




# 4.5 combine these

adm1_polyg_comb_wData <- adm1_polyg_comb_wData %>% 
  left_join(adm0_gdp_pc) %>% 
  left_join(adm0urb) %>% 
  left_join(adm0travelTime) %>% 
  left_join(adm0gini)

write.csv(adm1_polyg_comb_wData, 'results/adm1_polyg_comb_wData.csv')



##### 5.  prepare file for downscaling itself in matlab -----



# 5.1 combine 'urb','travelTime','adm0GDP','adm0urb','adm0gini'
adm1_polyg_comb_Matlab <- adm1_units_for_downscaling %>% 
  mutate(year = as.character(year)) %>% 
  left_join(adm1_polyg_comb_wData) %>% 
  #mutate(gdpRatio = adm1gdp_pc / gdp_pc) %>% 
  rename(adm0gini = gini_disp) %>% 
  rename(adm0GDP = gdp_pc) %>% 
  rename(adm0urb = urbAdm0) %>%
  rename(adm0travelTime = travelTimeAdm0) %>% 
  select('iso3','GID_nmbr', 'Subnat','year', 'gdpRatio','urb','travelTime','adm0GDP','adm0urb','adm0gini', 'adm0travelTime') %>% 
  drop_na() # drop admin units that are not full


if (dir.exists('downscalingMatlab/')) {
} else {
  dir.create('downscalingMatlab/')  
}


### 5.2 add region code and name


cntryID_reg <- read_csv("data_in/countries_codes_and_coordinates_reg.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  #select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(iso3, .keep_all = T) %>% 
  left_join(cntry_info %>% select(iso3, cntry_id)) %>% 
  select(-cntry_code) %>% 
  select(iso3, RegionID, RegName)


adm1_polyg_comb_Matlab_reg <- adm1_polyg_comb_Matlab %>% 
  #select(-c(...1,  ...2)) %>% 
  left_join(cntryID_reg)

# 5.3 write file

write.csv(adm1_polyg_comb_Matlab_reg, 'downscalingMatlab/adm1DataForDownscaling.csv')




