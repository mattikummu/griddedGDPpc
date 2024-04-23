

### interpolating and extrapolating data
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

#### load data -----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(iso3, .keep_all = T)

v_subnat_gis_combined <- vect("results/polyg_adm1_gdp_pc_1990_2022.gpkg")
sf_subnat_gis_combined <- read_sf("results/polyg_adm1_gdp_pc_1990_2022.gpkg")

df_adm0_data <- read_csv("results/adm0_gdp_pc_long_interpExtrap.csv")


v_subnat_gis_adm1 <- v_subnat_gis_combined %>% 
  filter(GID_nmbr > 1000)

##### prepare downscaling -----


##### independent & dependendent variables for downscaling -----

r_popCount <- rast('data_gis/popRaster_1990_2020.tif')

# pop missing from some areas for 1990-1999; let's use year 2000 to fill those

r_popCount_1990_99 <- subset(r_popCount, 1:10)
r_popCount_2000 <- subset(r_popCount, 11)
r_popCount_1990 <-  subset(r_popCount, 1)

r_popCount_1990[is.na(r_popCount_1990)] <- r_popCount_2000

r_popCount_1990_99[is.na(subset(r_popCount_1990_99,10))] <- r_popCount_2000
r_popCount_1990_99[subset(r_popCount_1990_99,10) == 0] <- r_popCount_2000

r_popCount_mod <- c(r_popCount_1990_99, subset(r_popCount,11:31))

r_urbanisation <- rast('data_gis/urbanisationCntryWise_19jul2023.tif')
 
sf_CntryUrbRate <- read.csv("data_gis/urbRateNational.csv") %>% 
  as_tibble() %>% 
  select(-c(X, Country, cntry_code, GADM_code)) %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'cntryUrbRatio') %>% 
  mutate(year = as.numeric(substring(year,2)))



# subnat gdp ratio over national value
sf_adm1gdpReported <- read.csv('results/subnat_gis_combined_feb2024.csv') %>% 
  pivot_longer(-c(names(.[1:6])), names_to = 'year', values_to = 'gdpRatio') %>% 
  filter(!year == 'X1989')

sf_adm1gdpInterp <- read.csv('results/adm1_gdp_ratio_interp_feb2024.csv') %>% 
  as_tibble() %>% 
  #rename(year = name, gdpRatio = value) %>% 
  filter(!year == '1989') 


##### prepared data for model training ----

# select observations so that 50% of data for each country is selected randomly

set.seed(21)

st_data_available <- sf_adm1gdpReported %>% 
  filter(GID_nmbr > 1000) %>% # filter out if national values are included
  filter(!is.na(gdpRatio)) %>% 
  group_by(iso3) %>% 
  summarise(merged_column = paste(year)) %>% 
  distinct(.keep_all = T) %>%
  ungroup() %>% 
  group_by(iso3) %>% 
  # sample 50% of the observed values
  slice_sample(prop=0.5) %>% 
  mutate(ID = paste0(iso3, "_", merged_column)) %>% 
  ungroup()

adm1_units_for_downscaling <- sf_adm1gdpInterp %>% 
  mutate(ID = paste0(iso3, "_X", year)) %>% 
  filter(ID %in% st_data_available$ID) %>% 
  select(iso3, GID_nmbr, year, gdpRatio)




##### get data for each adm1 unit -----



# extract data to adm1 boundaries

if (file.exists('results/ext_data_adm1_mar2024.RData')){
  # load it
  load('results/ext_data_adm1_mar2024.RData') 
} else { #create it
  
  r_popCount <- r_popCount_mod
  #r_pop_2015 <- crop(subset(r_popCount, 26),ext(v_subnat_gis_combined))
  
  
  r_urbanisation <- rast('/Users/mkummu/R/misc/percentileNormalisation/output/urbanisationCntryWise_19jul2023.tif')
  
  # gini coefficient data can be requested from matti.kummu@aalto.fi
  r_gini <- rast('/Users/mkummu/R/gini/results/rast_gini_disp_1990_2021.tif')
  
  # travel time data is available from 
  # https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3
  r_travelTime <- rast('/Users/mkummu/R/GIS_data_common/travel_time_cities/travel_time_to_cities_11.tif')
  r_travelTime_crop <- aggregate(crop(r_travelTime,ext(subset(r_popCount,1))), fact=10, fun='mean', na.rm=T)
  r_travelTime_crop <- extend(r_travelTime_crop, subset(r_popCount,1))
  
  # sf_gdpUnits <- st_read('results/polyg_gdp_1990_2021.gpkg') %>% 
  #   mutate(GID_nmbr = paste0('GID',as.character(GID_nmbr)))
  
  sf_gdpUnits <- sf_subnat_gis_combined %>% 
    mutate(GID_nmbr = as.character(GID_nmbr))
  
  v_gdpUnits <- vect(sf_gdpUnits)
  
  r_adm1adm0comb <- rasterize(v_gdpUnits, subset(r_popCount,1), field = 'GID_nmbr')
  #r_adm1adm0comb <- as.factor(r_adm1adm0comb)
  
  
  
  #v_adm2_polyg_comb <- vect(adm2_polyg_comb)
  
  v_subnat_gis_adm1 <- v_subnat_gis_combined %>% 
    filter(GID_nmbr > 1000)
  
  ext_pop_adm1 <- terra::extract(r_popCount,v_subnat_gis_adm1, fun = sum, na.rm=T)
  ext_urb_x_pop_adm1 <- terra::extract(r_popCount*crop(r_urbanisation, ext(r_popCount)),v_subnat_gis_adm1, fun = sum, na.rm=T)  
  ext_gini_x_pop_adm1 <- terra::extract(c(r_popCount,subset(r_popCount,31))*
                                          crop(r_gini, ext(r_popCount)),v_subnat_gis_adm1, fun = sum, na.rm=T)
  ext_travelTime_x_pop_adm1 <- terra::extract(r_popCount*r_travelTime_crop,v_subnat_gis_adm1, fun = sum, na.rm=T)  
  
  save(ext_pop_adm1, ext_urb_x_pop_adm1, ext_gini_x_pop_adm1,
       ext_travelTime_x_pop_adm1,  
       file = "results/ext_data_adm1_mar2024.RData")
}


## to long format

adm1_polyg_comb <- st_as_sf(v_subnat_gis_adm1) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(GID_nmbr)


adm1_polyg_comb_gdp_pc <- st_as_sf(v_subnat_gis_adm1) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(GID_nmbr, paste0(1990:2021)) %>% 
  pivot_longer(-c('GID_nmbr'), names_to = 'year', values_to = 'adm1gdp_pc')

# year 2021 = year 2020
ext_pop_adm1[,33] = ext_pop_adm1[,32]

adm1_polyg_comb_pop <- adm1_polyg_comb %>% 
  bind_cols(ext_pop_adm1) %>% 
  select(-c(ID)) %>% 
  set_names('GID_nmbr',  paste0(1990:2021) ) %>% 
  pivot_longer(-c('GID_nmbr'), names_to = 'year', values_to = 'pop')


adm1_polyg_comb_urb <- adm1_polyg_comb %>% 
  bind_cols(ext_urb_x_pop_adm1 / ext_pop_adm1[,1:32]) %>% 
  select(-c(ID)) %>% 
  # for year 2021, let's use year 2020
  mutate(yr2021 = ppp_2020) %>% 
  set_names('GID_nmbr',  paste0(1990:2021) ) %>% 
  pivot_longer(-c('GID_nmbr'), names_to = 'year', values_to = 'urb')


adm1_polyg_comb_gini <- adm1_polyg_comb %>% 
  bind_cols(ext_gini_x_pop_adm1 / ext_pop_adm1 )  %>% 
  select(-c(ID)) %>% 
  set_names('GID_nmbr',  paste0(1990:2021) ) %>% 
  pivot_longer(-c('GID_nmbr'),  names_to = 'year', values_to = 'gini')

dim(ext_travelTime_x_pop_adm1)
dim(ext_pop_adm1)

ext_travelTime_x_pop_adm1[,33] <- ext_travelTime_x_pop_adm1[,32]

adm1_polyg_comb_traveTime <- adm1_polyg_comb %>% 
  bind_cols(ext_travelTime_x_pop_adm1 / ext_pop_adm1 ) %>% 
  select(-c(ID)) %>% 
  set_names( 'GID_nmbr', paste0(1990:2021) ) %>% 
  pivot_longer(-c('GID_nmbr'),  names_to = 'year', values_to = 'travelTime')

adm1_polyg_comb_adm0unit <- adm1_polyg_comb %>% 
  left_join(st_as_sf(v_subnat_gis_adm1) %>% 
              st_drop_geometry() %>% 
              as_tibble() %>% 
              select(GID_nmbr, iso3) )


#combine all

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


# add still adm0 gdp per capita

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
# gini coefficient data can be acquired from matti.kummu@aalto.fi
adm0gini <- read.csv('/Users/mkummu/R/gini/results/gini_adm0_DispMkt_extrap.csv') %>% 
  as_tibble() %>% 
  select(-gini_mkt) %>% 
  mutate(year = as.character(year))




# combine these

adm1_polyg_comb_wData <- adm1_polyg_comb_wData %>% 
  left_join(adm0_gdp_pc) %>% 
  left_join(adm0urb) %>% 
  left_join(adm0travelTime) %>% 
  left_join(adm0gini)

write.csv(adm1_polyg_comb_wData, 'results/adm1_polyg_comb_wData.csv')

#test <- read_csv('results/adm1_polyg_comb_wData_OLD.csv')

## prepare file for downscaling itself in matlab



# 'urb','travelTime','adm0GDP','adm0urb','adm0gini'
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


if (!dir.exists('downscalingMatlab/')) {
  dir.create('downscalingMatlab/')
}

write.csv(adm1_polyg_comb_Matlab, 'downscalingMatlab/adm1DataForDownscaling_mar2024.csv')


adm1_polyg_comb_Matlab <- read_csv('downscalingMatlab/adm1DataForDownscaling_mar2024.csv')

#### test linear model ----


set.seed(21)

#create ID column
adm1_polyg_comb_Matlab$id <- 1:nrow(adm1_polyg_comb_Matlab)

# remove outliers
subnatDataForDownscaling_rmvOutL <- adm1_polyg_comb_Matlab %>% 
  filter(!gdpRatio < quantile(adm1_polyg_comb_Matlab$gdpRatio, 0.0025) &
           !gdpRatio > quantile(adm1_polyg_comb_Matlab$gdpRatio, 0.9975) )

#use 75% of dataset as training set and 25% as test set 
train <- subnatDataForDownscaling_rmvOutL %>% dplyr::sample_frac(0.75)
test  <- dplyr::anti_join(subnatDataForDownscaling_rmvOutL, train, by = 'id')

mod <- lm(gdpRatio ~ (travelTime/adm0travelTime) + urb + (urb/adm0urb) + adm0GDP + adm0gini, data = train)
summary(mod)   



testData <- test %>% 
  mutate(gdp = gdpRatio*adm0GDP) %>% 
  bind_cols(predict(mod, test, interval = "confidence") )


scatter.smooth(x=testData$gdpRatio, y=testData$fit)


