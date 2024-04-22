

### interpolating and extrapolating data
library(sf)
library(terra)
library(tidyterra)
library(openxlsx) #

library(broom)
library(tidyr)
library(tidyverse)
library(dplyr) 



# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### load data -----

# cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
#   dplyr::select(-cntry_code) %>% 
#   rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
#   select(cntry_code,iso2,iso3,Country) %>% 
#   mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
#   distinct(iso3, .keep_all = T)

# load cntry_info
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


#### create admin2 polygon layer -----

if (file.exists('results/adm2_polyg_comb.gpkg')){
  # load it
  adm2_polyg_comb <- st_read('results/adm2_polyg_comb.gpkg') 
} else { #create it
  
  adm0_polyg <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
    mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) %>% # kosovo to correct iso3
    rename(iso3 = GID_0) %>% 
    filter(!iso3 == 'ALA') # remove åland, as part of finland
  
  
  adm1_polyg <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_1') %>% 
    mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) %>% # kosovo to correct iso3
    rename(iso3 = GID_0) %>% 
    filter(!iso3 == 'ALA') # remove åland, as part of finland
  
  
  adm2_polyg <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_2') %>% 
    mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) %>% # kosovo to correct iso3
    rename(iso3 = GID_0) # %>% 
  #filter(!iso3 == 'ALA') # remove åland, as part of finland
  
  adm2_polyg_comb <- adm2_polyg %>% 
    # if not adm2 data, use adm1 data
    bind_rows(adm1_polyg %>% filter(!iso3 %in% unique(adm2_polyg$iso3))) %>% 
    # finally, if not in either adm2 or adm1, use adm0
    bind_rows(adm0_polyg %>% filter(!iso3 %in% c(unique(adm2_polyg$iso3),unique(adm1_polyg$iso3)))) %>%
    mutate(NAME_2 = ifelse(is.na(NAME_2) & is.na(NAME_1), COUNTRY, 
                           ifelse(is.na(NAME_2), NAME_1, NAME_2) )) %>% 
    mutate(GID_2 = ifelse(is.na(GID_2) & is.na(GID_1), iso3, 
                          ifelse(is.na(GID_2), GID_1, GID_2))) %>% 
    select(iso3, GID_2, NAME_2) %>% 
    # some of the adm1 levels are divided to those that are officially in a country and those that are 
    # on conflict zones (between CHN, IND and PAK)
    # let's rename them with those that we have data for
    mutate(iso3 = ifelse(iso3 %in% c('Z02', 'Z03', 'Z08'),'CHN',
                         ifelse(iso3 %in% c('Z06'), 'PAK', 
                                ifelse(iso3 %in% c('Z01', 'Z04','Z05','Z07','Z09'), 'IND', 
                                       iso3)) ) ) 
  
  
  
  st_write(adm2_polyg_comb, 'results/adm2_polyg_comb.gpkg', delete_dsn=T)
}

temp <- adm2_polyg_comb %>% 
  st_drop_geometry() 



##### get data for each adm2 unit -----




# extract data to adm2 boundaries

if (file.exists('results/ext_data.RData')){
  # load it
  load('results/ext_data.RData') 
} else { #create it
  
   r_popCount <- rast('data_gis/popRaster_1990_2020.tif')
  
  # pop missing from some areas for 1990-1999; let's use year 2000 to fill those
  
  r_popCount_1990_99 <- subset(r_popCount, 1:10)
  r_popCount_2000 <- subset(r_popCount, 11)
  r_popCount_1990 <-  subset(r_popCount, 1)
  
  r_popCount_1990[is.na(r_popCount_1990)] <- r_popCount_2000
  
  r_popCount_1990_99[is.na(subset(r_popCount_1990_99,10))] <- r_popCount_2000
  r_popCount_1990_99[subset(r_popCount_1990_99,10) == 0] <- r_popCount_2000
  
  r_popCount_mod <- c(r_popCount_1990_99, subset(r_popCount,11:31))
  
  # global(r_popCount_mod, sum, na.rm=T)
  # global(r_popCount, sum, na.rm=T)
  # 
  r_urbanisation <- rast('data_gis/urbanisationCntryWise_19jul2023.tif')
  
  r_gini <- rast('/Users/mkummu/R/gini/results/rast_gini_disp_1990_2021.tif')
  
  r_travelTime <- rast('/Users/mkummu/R/GIS_data_common/travel_time_cities/travel_time_to_cities_11.tif')
  r_travelTime_crop <- aggregate(crop(r_travelTime,ext(subset(r_popCount_mod,1))), fact=10, fun='mean', na.rm=T)
  r_travelTime_crop <- extend(r_travelTime_crop, subset(r_popCount_mod,1))
  
  # sf_gdpUnits <- st_read('results/polyg_gdp_1990_2021.gpkg') %>% 
  #   mutate(GID_nmbr = paste0('GID',as.character(GID_nmbr)))
  
  sf_gdpUnits <- st_read('results/polyg_adm1_gdp_perCapita_1990_2022.gpkg') %>% 
    mutate(GID_nmbr = as.character(GID_nmbr))
  
  v_gdpUnits <- vect('results/polyg_adm1_gdp_perCapita_1990_2022.gpkg')
  
  r_adm1adm0comb_1arcmin <- rasterize(v_gdpUnits, rast(ncol=360*60, nrow=180*60), field = 'GID_nmbr')
  r_adm1adm0comb_5arcmin <- terra::aggregate(r_adm1adm0comb_1arcmin,fact=5,fun=modal,na.rm=T)
  
  #r_adm1adm0comb <- as.factor(r_adm1adm0comb)
  
  #v_adm2_polyg_comb <- vect(adm2_polyg_comb)
  v_adm2_polyg_comb <- vect('results/adm2_polyg_comb.gpkg')
  
  ext_pop_adm2 <- terra::extract(r_popCount_mod,v_adm2_polyg_comb, fun = sum, na.rm=T)
  ext_urb_x_pop_adm2 <- terra::extract(r_popCount_mod*crop(r_urbanisation, ext(r_popCount_mod)),v_adm2_polyg_comb, fun = sum, na.rm=T)  
  ext_gini_x_pop_adm2 <- terra::extract(c(r_popCount_mod,subset(r_popCount_mod,31))*
                                          crop(r_gini, ext(r_popCount_mod)),v_adm2_polyg_comb, fun = sum, na.rm=T)
  ext_travelTime_x_pop_adm2 <- terra::extract(r_popCount_mod*r_travelTime_crop,v_adm2_polyg_comb, fun = sum, na.rm=T)  
  
  ext_adm1units <- terra::extract(r_adm1adm0comb_5arcmin,v_adm2_polyg_comb, na.rm=T)  
  head(ext_adm1units)
  
  ext_adm1units_modal <- ext_adm1units %>% 
    group_by(ID) %>% 
    count(ID,GID_nmbr) %>%
    slice(which.max(n))
  
  head(ext_adm1units_modal)
  
  save(ext_pop_adm2, ext_urb_x_pop_adm2, ext_gini_x_pop_adm2,
       ext_travelTime_x_pop_adm2, ext_adm1units_modal, 
       file = "results/ext_data.RData")
}


## to long format

if (file.exists('results/adm2_polyg_comb_wData.csv')){
  # load it
  adm2_polyg_comb_wData <- as_tibble(data.table::fread('results/adm2_polyg_comb_wData.csv') )
} else { #create it
  
  # year 2021 = year 2020
  ext_pop_adm2[,33] = ext_pop_adm2[,32]
  
  adm2_polyg_comb_pop <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    bind_cols(ext_pop_adm2) %>% 
    select(-c(ID, iso3, NAME_2)) %>% 
    set_names('GID_2',  paste0(1990:2021) ) %>% 
    pivot_longer(-c('GID_2'), names_to = 'year', values_to = 'pop') %>% 
    drop_na()
  
  
  
  
  # # for some adm2 units population is missing for years 1990-1999 as no data in HYDE were available
  # # let's use the population of 2000 for those
  # 
  # countNA <- adm2_polyg_comb_pop_temp %>%
  #   mutate(pop = ifelse(pop == 0, NA, pop)) %>%
  #   #mutate(pop = ifelse(is.nan(pop), NA, pop)) %>%
  #   group_by(GID_2) %>% summarise(na_count = sum(is.na(pop))) %>%
  #   # let's focus on thoe that are missing the first 10 years
  #   filter(na_count > 5 & na_count < 15)
  # 
  # temp_pop_replaceNA <- adm2_polyg_comb_pop_temp %>%
  #   filter(GID_2 %in% countNA$GID_2) %>%
  #   group_by(GID_2) %>%
  #   mutate(pop2000TF = ifelse(year == 2000, TRUE, FALSE)) %>%
  #   mutate(pop2000 = pop[pop2000TF]) %>%
  #   mutate(pop_mod = ifelse(is.na(pop) | pop == 0, pop2000, pop)) %>%
  #   select(GID_2, year, pop_mod)
  # 
  # adm2_polyg_comb_pop <- adm2_polyg_comb_pop_temp %>%
  #   left_join(temp_pop_replaceNA) %>%
  #   mutate(pop = ifelse(is.na(pop) | pop == 0, pop_mod, pop)) %>%
  #   select(-pop_mod)
  
  
  temp2 <- adm2_polyg_comb_pop %>% 
    filter(grepl('SJM',GID_2))
  temp2
  
  
  # update also the ext_pop_adm2
  
  # adm2_polyg_comb_pop_wide <- adm2_polyg_comb_pop %>% 
  #   distinct(GID_2, year, .keep_all = T) %>% 
  #   pivot_wider(names_from = 'year', values_from = 'pop')
  # 
  # ext_pop_adm2_mod <- adm2_polyg_comb %>% 
  #   st_drop_geometry() %>% 
  #   select(GID_2) %>% 
  #   left_join(adm2_polyg_comb_pop_wide)
  
  
  # urbanisation
  
  adm2_polyg_comb_urb <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    bind_cols(ext_urb_x_pop_adm2 / ext_pop_adm2[,1:32]) %>% 
    select(-c(ID, iso3, NAME_2)) %>% 
    # for year 2021, let's use year 2020
    mutate(yr2021 = ppp_2020) %>% 
    set_names('GID_2',  paste0(1990:2021) ) %>% 
    pivot_longer(-c('GID_2'), names_to = 'year', values_to = 'urb') #%>% 
  #drop_na()
  
  # for some admin areas missing values; let's use the mean of the existing data for those
  
  # identify those admin units where there is nan urban values
  urb_NAN <- adm2_polyg_comb_urb %>% 
    filter(is.na(urb)) 
  
  urb_NAN_unique <- unique(urb_NAN$GID_2)
  
  # calculate mean urb for those with both NaN values and non-NaN values
  urb_both_NAN_nonNAN <- adm2_polyg_comb_urb %>% 
    filter(GID_2 %in% urb_NAN_unique) %>% 
    drop_na() %>% 
    group_by(GID_2) %>% 
    summarise(meanUrb = mean(urb))
  
  # fill those back to main database
  
  adm2_polyg_comb_urb <- adm2_polyg_comb_urb %>% 
    left_join(urb_both_NAN_nonNAN) %>% 
    mutate(urb = ifelse(is.na(urb), meanUrb,urb)) %>% 
    select(-meanUrb) %>% 
    drop_na()
  
  
  temp2 <- adm2_polyg_comb_urb %>% 
    filter(grepl('SJM',GID_2))
  temp2
  
  
  ## gini
  
  adm2_polyg_comb_gini <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    bind_cols(ext_gini_x_pop_adm2 / ext_pop_adm2 )  %>% 
    select(-c(ID, iso3, NAME_2)) %>% 
    set_names('GID_2', paste0(1990:2021) ) %>% 
    pivot_longer(-c('GID_2'), names_to = 'year', values_to = 'gini') %>% 
    drop_na()
  
  temp2 <- adm2_polyg_comb_gini %>% 
    filter(grepl('SJM',GID_2))
  temp2
  
  # dim(ext_travelTime_x_pop_adm2)
  # dim(ext_pop_adm2)
  
  adm2_polyg_comb_traveTime <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    bind_cols(ext_travelTime_x_pop_adm2 / ext_pop_adm2[,1:32] ) %>% 
    # for year 2021, let's use year 2020
    mutate(yr2021 = ppp_2020) %>% 
    select(-c(ID, iso3, NAME_2)) %>% 
    set_names('GID_2', paste0(1990:2021) )%>% 
    pivot_longer(-c('GID_2'), names_to = 'year', values_to = 'travelTime')%>% 
    drop_na()
  
  temp2 <- adm2_polyg_comb_traveTime %>% 
    filter(grepl('BRB.8',GID_2))
  temp2
  
  adm2_polyg_comb_adm1unit <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    bind_cols(ext_adm1units_modal ) %>% 
    select(-ID, -n, -iso3, -NAME_2)  %>% 
    drop_na()
  
  adm2_polyg_comb_adm1unit <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    bind_cols(ext_adm1units_modal ) %>% 
    select(-ID, -n, -iso3, -NAME_2)  %>% 
    drop_na() %>% 
    distinct(GID_nmbr, GID_2, .keep_all = T)
  
  
  
 
  
  temp2 <- adm2_polyg_comb_adm1unit %>% 
    filter(grepl('BRB.8',GID_2))
  temp2
  #combine all
  
  adm2_polyg_comb_wData <- adm2_polyg_comb_urb %>% 
    left_join(adm2_polyg_comb_gini) %>% 
    left_join(adm2_polyg_comb_traveTime) %>% 
    left_join(adm2_polyg_comb_adm1unit) %>% 
    left_join(adm2_polyg_comb_pop) %>% 
    left_join(adm2_polyg_comb %>% st_drop_geometry()) %>% 
    select(iso3, GID_2, NAME_2, everything())
  
  
  temp2 <- adm2_polyg_comb_wData %>% 
    filter(grepl('BRB',GID_2))
  temp2
  # Gini missing for some small island states; let's use 0.5 for them
  
  adm2_polyg_comb_wData <- adm2_polyg_comb_wData %>% 
    mutate(gini = ifelse(is.na(gini), 0.5, gini))
  
  
  temp2 <- adm2_polyg_comb_wData %>% 
    filter(grepl('SJM',GID_2))
  temp2
  
  # add still gdp per capita
  
  adm1_gdp_pc <- read.csv('results/tabulated_gdp_perCapita.csv') %>% 
    as_tibble() %>% 
    select(-c(Country, iso3, Subnat, slope)) %>% 
    set_names('GID_nmbr', paste0(1990:2022) ) %>% 
    pivot_longer(-c('GID_nmbr'), names_to = 'year', values_to = 'gdp_adm1') %>% 
    mutate(gdp_adm1 = as.numeric(gdp_adm1)) %>% 
    #mutate(GID_nmbr = as.factor(GID_nmbr))%>% mutate(GID_nmbr = as.numeric(GID_nmbr)) %>% 
    distinct(GID_nmbr, year, .keep_all = T)
  
  temp3 <- adm1_gdp_pc %>% 
    filter(GID_nmbr == 1105036)
  temp3
  
  adm2_polyg_comb_wData_adm1gdp <- adm2_polyg_comb_wData %>% 
    left_join(adm1_gdp_pc )
  
  temp3 <- adm2_polyg_comb_wData_adm1gdp %>% 
    filter(GID_nmbr == 1105036)
  temp3
  
 
  temp2 <- adm2_polyg_comb_wData_adm1gdp %>% 
    filter(grepl('ALA',GID_2))
  temp2
  
  temp2 <- adm2_polyg_comb_wData_adm1gdp %>% 
    filter(grepl('IND.36.19',GID_2))
  temp2
  
  adm2_polyg_comb_wData <- adm2_polyg_comb_wData_adm1gdp
  
  data.table::fwrite(adm2_polyg_comb_wData, 'results/adm2_polyg_comb_wData.csv')
  
  
  
}



## prepare file for downscaling in matlab



if (file.exists('downscalingMatlab/adm2DataForDownscaling.csv')){
  
} else { #create it
  
  
  adm1urb <- adm2_polyg_comb_wData %>% 
    mutate(urb_x_pop = urb*pop) %>% 
    group_by(GID_nmbr, year) %>% 
    summarise(urbAdm1 = sum(urb_x_pop) / sum(pop) )
  
  adm1travelTime <- adm2_polyg_comb_wData %>% 
    mutate(TT_x_pop = travelTime*pop) %>% 
    group_by(GID_nmbr, year) %>% 
    summarise(travelTimeAdm1 = sum(TT_x_pop) / sum(pop))
  
  
  # 'urb','travelTime','adm0GDP','adm0urb','adm0gini'
  adm2_polyg_comb_Matlab <- adm2_polyg_comb_wData %>% 
    rename(adm0gini = gini) %>% 
    rename(adm0GDP = gdp_adm1) %>% 
    left_join(adm1urb) %>% 
    left_join(adm1travelTime) %>% 
    rename(adm0urb = urbAdm1) %>% 
    rename(adm0travelTime = travelTimeAdm1) %>% 
    select('GID_2', 'GID_nmbr', 'year', 'urb','travelTime','adm0GDP','adm0urb','adm0gini', 'adm0travelTime')
  
  temp2 <- adm2_polyg_comb_Matlab %>% 
    filter(grepl('MYT',GID_2))
  temp2
  
  test_na <- adm2_polyg_comb_Matlab %>% 
    filter(is.na(GID_nmbr))
  
  data.table::fwrite(adm2_polyg_comb_Matlab, 'downscalingMatlab/adm2DataForDownscaling.csv')
  
}

# adm2_polyg_comb_Matlab_test <- adm2_polyg_comb_Matlab[100000:400000,]
# 
# write.csv(adm2_polyg_comb_Matlab_test, 'downscalingMatlab/adm2DataForDownscaling_test.csv')


### run matlab script



#### load downscaled GDP ratio -----


if (file.exists('results/adm2_gdp.csv')){
  # load it
  adm2_gdp <- as_tibble(data.table::fread('results/adm2_gdp.csv') )
} else { #create it
  
  downscaledGDPratio <- as_tibble(data.table::fread('/Users/mkummu/R/subnat_gdp_2023/downscalingMatlab/downsclaling_output.csv')) %>% 
    rename(Estimated.gdpratio = 'ENS_Prediction') %>% 
    rename(Corrected.Estimated.gdpratio = 'Corrected_Prediction') %>% 
    # na to 1
    mutate(Corrected.Estimated.gdpratio = ifelse(is.na(Corrected.Estimated.gdpratio), 1,
                                                 Corrected.Estimated.gdpratio)) %>% 
    mutate(year = as.character(year)) #%>% 
  # some negative values, let's add 10 to each
  # mutate(Corrected.Estimated.gdpratio = Corrected.Estimated.gdpratio + 10)
  
  
  adm2_polyg_comb_Matlab_downScaled_noYear2022 <- adm2_polyg_comb_wData %>% 
    select(iso3, GID_2, NAME_2, year, gdp_adm1, pop, GID_nmbr) %>% 
    mutate(year = as.character(year)) %>% 
    left_join(downscaledGDPratio) %>% 
    select(iso3,  GID_2, NAME_2, year, gdp_adm1, GID_nmbr, Corrected.Estimated.gdpratio, pop) %>% 
    rename(gdpRatio = Corrected.Estimated.gdpratio) %>% 
    mutate(gdp_adm1 = as.numeric(gdp_adm1))
  
  
  # let's use year 2021 ratio for year 2022 (as no downscaling data for that year)
  
  adm1_gdp_pc <- read.csv('results/tabulated_gdp_perCapita.csv') %>% 
    as_tibble() %>% 
    select(-c(Country, iso3, Subnat, slope)) %>% 
    set_names('GID_nmbr', paste0(1990:2022) ) %>% 
    pivot_longer(-c('GID_nmbr'), names_to = 'year', values_to = 'gdp_adm1') %>% 
    mutate(gdp_adm1 = as.numeric(gdp_adm1)) %>% 
    #mutate(GID_nmbr = as.factor(GID_nmbr))%>% mutate(GID_nmbr = as.numeric(GID_nmbr)) %>% 
    distinct(GID_nmbr, year, .keep_all = T) %>% 
    filter(year == 2022)
  
  adm2_polyg_comb_Matlab_downScaled_2022 <- adm2_polyg_comb_wData %>% 
    select(iso3, GID_2, NAME_2, year, gdp_adm1, pop, GID_nmbr) %>% 
    mutate(year = as.character(year)) %>% 
    left_join(downscaledGDPratio) %>% 
    select(iso3,  GID_2, NAME_2, year, gdp_adm1, GID_nmbr, Corrected.Estimated.gdpratio, pop) %>% 
    rename(gdpRatio = Corrected.Estimated.gdpratio) %>% 
    mutate(gdp_adm1 = as.numeric(gdp_adm1)) %>% 
    filter(year == 2021) %>% 
    mutate(year = 2022) %>% 
    select(-gdp_adm1) %>% 
    left_join(adm1_gdp_pc %>% mutate(year = as.numeric(year))) %>% 
    mutate(year = as.character(year)) 
  
  # add to other data
  
  adm2_polyg_comb_Matlab_downScaled <- adm2_polyg_comb_Matlab_downScaled_noYear2022 %>% 
    mutate(year = as.character(year)) %>% 
    bind_rows(adm2_polyg_comb_Matlab_downScaled_2022)  %>% 
    arrange(iso3, GID_2, year) %>% 
    mutate(gdp_adm2 = gdpRatio*gdp_adm1) %>% 
    mutate(gdp_adm2_x_pop = gdp_adm2 * pop)
  
  
  
  temp2 <- adm2_polyg_comb_Matlab_downScaled %>% 
    filter(GID_2 == 'IND.36.19_1')
  
  temp2 <- adm2_polyg_comb_Matlab_downScaled %>% 
    filter(grepl('BRB.8',GID_2))
  temp2
  
  adm1gdp_from_adm2gdp <- adm2_polyg_comb_Matlab_downScaled %>% 
    group_by(GID_nmbr, year) %>% 
    summarise(gdp_adm1_fromAdm2 = sum(gdp_adm2_x_pop) / sum(pop)) %>% 
    ungroup() %>% 
    #distinct(GID_nmbr, year, .keep_all=T) %>% 
    left_join(adm2_polyg_comb_Matlab_downScaled %>% select(GID_nmbr,gdp_adm1, year) %>% distinct()) %>% 
    mutate(corrRatio = gdp_adm1 / gdp_adm1_fromAdm2) %>% 
    select(GID_nmbr, year, corrRatio) %>% 
    distinct(.keep_all=T) 
  
  
  adm2_gdp_temp <- adm2_polyg_comb_Matlab_downScaled %>% 
    select(iso3,  GID_2,     NAME_2,  year,  GID_nmbr, gdp_adm2) %>% 
    left_join(adm1gdp_from_adm2gdp) %>% 
    mutate(gdp_adm2corr = corrRatio*gdp_adm2) %>% 
    left_join(cntry_info[,c(2,4)]) 
  
  
  adm2_gdp_rowNbr <- adm2_gdp_temp %>% 
    select(iso3, GID_2, cntry_id) %>% 
    distinct() %>% 
    group_by(iso3) %>% 
    mutate(rowNumb = row_number()) %>% 
    ungroup() %>% 
    mutate(adm2ID = 2*10^7 + cntry_id*10^4 + rowNumb) 
  
  adm2_gdp <- adm2_gdp_temp %>% 
    left_join(adm2_gdp_rowNbr)
  
  temp2 <- adm2_gdp_temp %>% 
    filter(grepl('SJM', GID_2))
  temp2
  
  ## add Svalbard (not all data for downscaling available, so not in the datalist)
  SJM_data <- read.csv('results/tabulated_gdp_perCapita.csv') %>% 
    as_tibble() %>% 
    filter(iso3 == 'SJM') %>% 
    select(-c(Subnat, slope)) %>% 
    set_names('GID_nmbr','iso3', 'Country', paste0(1990:2022) ) 
  
  SJM_data_adm1 <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    filter(grepl('SJM', GID_2)) %>% 
    left_join(SJM_data) %>% 
    select(-Country) %>% 
    pivot_longer(-c('GID_nmbr','iso3', 'NAME_2', 'GID_2'), names_to = 'year', values_to = 'gdp_adm2corr') %>% 
    mutate(gdp_adm2corr = as.numeric(gdp_adm2corr)) %>% 
    mutate(GID_nmbr = as.factor(GID_nmbr)) %>% 
    mutate(year = as.character(year)) %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    mutate(rowNumb = as.numeric(substr(GID_2, 5,5 ))) %>% 
    mutate(adm2ID = 2*10^7 + cntry_id*10^4 + rowNumb) %>% 
    select(iso3, GID_2, NAME_2, year, GID_nmbr, gdp_adm2corr,cntry_id, adm2ID)
  
  # add also part of Greenland
  
  GRL_data <- read.csv('results/tabulated_gdp_perCapita.csv') %>% 
    as_tibble() %>% 
    filter(iso3 == 'GRL') %>% 
    select(-c(Subnat, slope)) %>% 
    set_names('GID_nmbr','iso3', 'Country', paste0(1990:2022) ) 
  
  GRL.2_1_data_adm1 <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    filter(grepl('GRL.2_1', GID_2)) %>% 
    left_join(GRL_data) %>% 
    select(-Country) %>% 
    pivot_longer(-c('GID_nmbr','iso3', 'NAME_2', 'GID_2'), names_to = 'year', values_to = 'gdp_adm2corr') %>% 
    mutate(gdp_adm2corr = as.numeric(gdp_adm2corr)) %>% 
    mutate(GID_nmbr = as.factor(GID_nmbr)) %>% 
    mutate(year = as.character(year)) %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    #mutate(rowNumb = as.numeric(substr(GID_2, 5,5 ))) %>% 
    mutate(adm2ID = 2*10^7 + cntry_id*10^4 + 5) %>% 
    select(iso3, GID_2, NAME_2, year, GID_nmbr, gdp_adm2corr,cntry_id, adm2ID)
  
  
  # and part of Bahamas (new provinces; GDP close to 101801)
  
  BHS_data <- read.csv('results/tabulated_gdp_perCapita.csv') %>% 
    as_tibble() %>% 
    filter(iso3 == 'BHS') %>% 
    select(-c(Subnat, slope)) %>% 
    set_names('GID_nmbr','iso3', 'Country', paste0(1990:2022) ) %>% 
    filter(GID_nmbr == 1018001) %>% 
    pivot_longer(-c('GID_nmbr', 'iso3',  'Country'), values_to = 'gdp_pc', names_to = 'year') %>% 
    select(year, gdp_pc) %>% 
    mutate(year = as.character(year))
  
  
  BHS_data_adm1 <- adm2_gdp %>% 
    #st_drop_geometry() %>% 
    as_tibble() %>% 
    mutate(year = as.character(year)) %>% 
    filter(grepl('BHS', GID_2) & is.na(gdp_adm2corr)) %>% 
    left_join(BHS_data) %>% 
    mutate(gdp_adm2corr = gdp_pc) %>% 
    mutate(GID_nmbr = as.factor(GID_nmbr)) %>% 
    select(-gdp_pc)
  
  
  adm2_gdp <- adm2_gdp %>% 
    mutate(GID_nmbr = as.factor(GID_nmbr)) %>% 
    mutate(year = as.character(year)) %>% 
    # remove BHS that we just filled
    filter(!GID_2 %in% unique(BHS_data_adm1$GID_2)) %>% 
    # join back in
    bind_rows(BHS_data_adm1)%>% 
    bind_rows(SJM_data_adm1) %>% 
    bind_rows(GRL.2_1_data_adm1) 
    
    
    
    temp2 <- adm2_gdp %>% 
    filter(grepl('BHS.2_1', GID_2))
  temp2
  
  temp2 <- adm2_gdp %>% 
    filter(grepl('MYT', GID_2))
  temp2
  
  data.table::fwrite(adm2_gdp, 'results/adm2_gdp.csv')
  
  #rm(adm2_polyg_comb_Matlab_downScaled,downscaledGDPratio, adm2_polyg_comb_gini, adm2_polyg_comb_Matlab, adm2_polyg_comb_wData)
}




# adm2_gdp_afg <- adm2_gdp %>% 
#   filter(iso3 == 'FRA')

##### to grid -----


if (file.exists('data_gis/gdp_Adm2_raster_5arcmin.tif')){
  # load it
  r_gdp_adm2_polyg_5arcmin <- rast('data_gis/gdp_Adm2_raster_5arcmin.tif')
  #r_gdp_adm2_polyg_1arcmin <- rast('data_gis/gdp_Adm2_raster_1arcmin.tif')
} else { 
  # create it
  
  #create ref raster
  #ref_rast_5arcmin <- rast(ncol=360*12, nrow=180*12)
  ref_rast_1arcmin <- rast(ncol=360*60, nrow=180*60)
  
  ref_raster_1arcmin <- raster::raster(ncol=360*60, nrow=180*60)
  # rasterise to 1 arc min resolutions
  
  adm2_gdp_temp2 <- adm2_gdp %>% 
    select(GID_2,adm2ID) %>% distinct(GID_2, .keep_all=T)
  
  
  
  adm2_polyg_comb_ID <- adm2_polyg_comb %>% 
    left_join(adm2_gdp_temp2) %>% 
    mutate(rowNmb = row_number()) %>% 
    # remove caspian sea
    filter(!iso3 == 'XCA') 
  
  temp2 <- adm2_polyg_comb_ID %>% 
    filter(grepl('GRL', GID_2))
  temp2
  
  
  sf::write_sf(adm2_polyg_comb_ID, 'data_gis/adm2_polyg_comb_ID.gpkg' )
  
  
  v_adm2_polyg_comb <- vect('data_gis/adm2_polyg_comb_ID.gpkg') 
  
  r_gdp_adm2_polyg_1arcmin <-  rasterize(v_adm2_polyg_comb,ref_rast_1arcmin,field="rowNmb")
  
  # unique(r_gdp_adm2_polyg_1arcmin)
  
  
  # aggregate to 5 arc-min
  r_gdp_adm2_polyg_5arcmin <- terra::aggregate(r_gdp_adm2_polyg_1arcmin,fact=5,fun=modal,na.rm=T)
  # unique(r_gdp_adm2_polyg_5arcmin)
  
  # write raster
  
  terra::writeRaster(r_gdp_adm2_polyg_5arcmin,'data_gis/gdp_Adm2_raster_5arcmin.tif', 
                     gdal="COMPRESS=LZW",overwrite=TRUE)
  
}



### put data to raster -----------------------------------------------------


myFun_gdp_data2raster <- function(inYears = 1990:1991, 
                                  IndexName = 'gdp_pc', 
                                  inDataAdm2 = adm2_gdp) {
  
  coll_raster = rast()
  
  rowNmb_adm2ID <- sf::read_sf('data_gis/adm2_polyg_comb_ID.gpkg' ) %>% 
    st_drop_geometry() %>% 
    select(adm2ID, rowNmb) %>% 
    distinct(adm2ID, .keep_all = T)
  
  
  tempDataAdm2 <- inDataAdm2 %>% 
    select(-c(cntry_id, rowNumb,gdp_adm2, corrRatio)) %>% 
    left_join(rowNmb_adm2ID) %>% 
    rename(!!IndexName := gdp_adm2corr)# %>% 
  #filter(!is.na(adm2ID)) 
  
  temp2 <- tempDataAdm2 %>% 
    filter(grepl('SJM',GID_2))
  temp2
  
  # get all ids
  all_ids <- unique(r_gdp_adm2_polyg_5arcmin) %>% 
    as_tibble() %>% 
    drop_na()
  
  for (iYear in inYears) {
    
    tempDataAdm2_selYear <- tempDataAdm2 %>% 
      filter(year == iYear)
    
    # check adm0 areas for which we do not have data, and put those to NA in the raster
    idWithData <- tempDataAdm2_selYear %>% 
      as_tibble() %>% 
      filter(!is.na(gdp_pc)) %>% 
      dplyr::select(rowNmb) %>% 
      distinct()
    
    # get those ids we do not have data for
    idNoData <- all_ids %>% 
      filter(!rowNmb %in% idWithData$rowNmb)
    
    r_gdp_adm2_polyg_5arcmin_mod <- r_gdp_adm2_polyg_5arcmin
    
    # those areas that do not have data --> NA 
    r_gdp_adm2_polyg_5arcmin_mod[r_gdp_adm2_polyg_5arcmin_mod %in% as.numeric(as.matrix(idNoData))] <- NA
    
    
    temp_id <-  as.numeric(tempDataAdm2_selYear$rowNmb)
    temp_v <- as.numeric(tempDataAdm2_selYear[[IndexName]])
    
    # reclassify
    temp_raster <- classify(r_gdp_adm2_polyg_5arcmin_mod,
                            cbind(temp_id, temp_v))
    
    # plot(temp_raster)
    
    terra::add(coll_raster) <- temp_raster
  }
  
  names(coll_raster) <- paste0(IndexName,'_',inYears[1]:inYears[length(inYears)])
  
  terra::writeRaster(coll_raster,paste0('results/rast_adm2_gdpPerCapita_',inYears[1],'_',inYears[length(inYears)],'.tif'), 
                     gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(coll_raster)
}

varNames <- c('gdp_pc' )

for (iVar in 1:length(varNames)) {
  
  rast_varName <- myFun_gdp_data2raster(inYears = 1990:2022, 
                                        IndexName = varNames[iVar], 
                                        inDataAdm2 = adm2_gdp) 
  
}


#### for GBR, the admin 1 data is more accurate; let's use that ----

rast_adm2 <- rast("results/rast_adm2_gdpPerCapita_1990_2022.tif")
rast_adm1 <- rast("results/rast_adm1_gdp_perCapita_1990_2022.tif")
rast_adm0_admin <- rast("data_gis/gdp_adm0_raster_5arcmin.tif")

rast_adm2[rast_adm0_admin == 67] <- rast_adm1

writeRaster(rast_adm2,"results/rast_adm2_gdpPerCapita_1990_2022.tif",gdal="COMPRESS=LZW",overwrite=TRUE)

### calculate adm2 - cntry ratio

adm0gdp_pc <- rast('results/rast_gdp_pc_adm0_1990_2022.tif')
adm2gdp_pc <- rast('results/rast_adm2_gdpPerCapita_1990_2022.tif')

ratioGdp_pc <- adm2gdp_pc/adm0gdp_pc

terra::writeRaster(ratioGdp_pc,'results/rast_gdp_ratio_Adm2Adm0_5arcmin.tif', 
                   gdal="COMPRESS=LZW",overwrite=TRUE)


## difference between gdp and gnic


ratioGnic <- rast('/Users/mkummu/R/hdi_subnat/results/rast_gni_ratio_Adm2Adm0_5arcmin.tif')
ratioGdp_pc <- rast('/Users/mkummu/R/subnat_gdp_2023/results/rast_gdp_ratio_Adm2Adm0_5arcmin.tif')

diffGnicGdp <- ratioGnic-ratioGdp_pc

terra::writeRaster(diffGnicGdp,'/Users/mkummu/R/subnat_gdp_2023/results/rast_diff_GnicGdp_ratio_5arcmin.tif', 
                   gdal="COMPRESS=LZW",overwrite=TRUE)





### simplify polygon layer ----

if (file.exists('data_gis/adm2_polyg_comb_5arcmin_simple.gpkg')){
  # load it
  gdp_adm2_polyg_simpl <- st_read('data_gis/adm2_polyg_comb_5arcmin_simple.gpkg') 
} else { 
  # create it
  
  p_5arcmin <- as.polygons(r_gdp_adm2_polyg_5arcmin)
  # p_1arcmin <- as.polygons(rast(r_gdp_adm2_polyg_1arcmin))
  # as.data.frame(p)
  
  writeVector(p_5arcmin, 'data_gis/adm2_polyg_comb_5arcmin_simple.gpkg', overwrite=T)
  
  gdp_adm2_polyg_simpl <- st_read('data_gis/adm2_polyg_comb_5arcmin_simple.gpkg') 
  
  # writeVector(p_1arcmin, 'data_gis/adm2_polyg_comb_1arcmoin_simple.gpkg', overwrite=T)
  
  # gdp_adm2_polyg_simpl <- st_as_sf(raster::raster(r_gdp_adm2_polyg_5arcmin))  %>% 
  # 
  #   sf::st_simplify(., preserveTopology = T, dTolerance = 0.1)
  # 
  # 
  # st_write(gdp_adm2_polyg_simpl, 'data_gis/GDL_regions_v7_simpl.gpkg', delete_dsn=T)
  
}



#### put data to gpkg (and slope to raster) -----

adm2_count <- adm2_gdp %>% 
  distinct(adm2ID)


myFun_gdp_data2gpkg <- function(inYears = 1990:2022, IndexName = 'gdp_pc', 
                                inDataAdm2 = adm2_gdp) {
  
  
  rowNmb_adm2ID <- sf::read_sf('data_gis/adm2_polyg_comb_ID.gpkg' ) %>% 
    st_drop_geometry() %>% 
    select(adm2ID, rowNmb) %>% 
    distinct(adm2ID, .keep_all = T)
  
  
  tempDataAdm2 <- inDataAdm2 %>% 
    select(-c(cntry_id, rowNumb,gdp_adm2, corrRatio)) %>% 
    left_join(rowNmb_adm2ID) %>% 
    rename(!!IndexName := gdp_adm2corr) 
  #filter(!is.na(adm2ID)) 
  
  
  # calculate trend
  
  # https://stackoverflow.com/questions/72922288/group-wise-linear-models-function-nest-by
  
  
  test_tempDataAdm2_trend <- tempDataAdm2 %>% 
    #filter(GID_nmbr == 226)  %>% 
    as_tibble() %>% 
    group_by(adm2ID) %>% 
    mutate(time = row_number()) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(gdp_pc < 0)
  
  
  
  tempDataAdm2_trend <- tempDataAdm2 %>% 
    drop_na() %>% 
    #filter(GID_nmbr == 226)  %>% 
    as_tibble() %>% 
    group_by(adm2ID) %>% 
    mutate(time = row_number()) %>% 
    ungroup() %>% 
    select(-year) %>% 
    mutate(log10_gdp_pc = log10(gdp_pc)) %>% 
    nest(data = -adm2ID) %>% 
    mutate(
      model = map(data,  ~ mblm::mblm(log10_gdp_pc ~ time, data = .))
    ) %>% 
    mutate(
      tidy_summary = map(model, tidy)
    ) %>% 
    unnest(tidy_summary) %>% 
    filter(term == 'time') %>% 
    select(adm2ID, estimate, p.value)
  
  
  
  
  # # https://stackoverflow.com/questions/32274779/extracting-p-values-from-multiple-linear-regression-lm-inside-of-a-ddply-funct
  # 
  # tempDataadm2_trend <- tempDataadm2 %>% 
  #   group_by(GID_nmbr) %>% 
  #   #nest() %>% 
  #   do({model = lm(gnic~year, data=.)    # create your model
  #   data.frame(tidy(model),              # get coefficient info
  #              glance(model))})   %>% 
  #   filter(term == 'year')
  
  gdp_adm2_polyg_noGeom <- adm2_polyg_comb %>%
    st_drop_geometry() %>% 
    select(iso3, GID_2, NAME_2)
  
  tempDataadm2_dublicates <- tempDataAdm2 %>% 
    select(GID_2, year, as.name(!!IndexName), adm2ID, rowNmb) %>% 

    dplyr::group_by(GID_2, adm2ID, rowNmb, year) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L) 
  
  tempDataadm2_wTrend <- tempDataAdm2 %>% 
    select(GID_2, year, as.name(!!IndexName), adm2ID, rowNmb) %>% 
    
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm2_trend) %>% 
    mutate(p.value = p.value < 0.05) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(gdp_adm2_polyg_simpl) %>% 
    left_join(gdp_adm2_polyg_noGeom) %>% 
    select(GID_2, adm2ID, iso3, NAME_2, slope, everything()) %>% 
    select(-c(estimate, p.value))
  
  st_write(tempDataadm2_wTrend,
           paste0('results/polyg_adm2_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.gpkg'), 
           delete_dsn=T)
  
  # only csv
  temp <- tempDataadm2_wTrend %>% 
    st_drop_geometry() %>% 
    select(-geom)
  
  write_csv(tempDataadm2_wTrend %>% st_drop_geometry(), "results/tabulated_adm2_gdp_perCapita.csv")
  
  # slope to raster
  
  
  # check adm0 areas for which we do not have data, and put those to NA in the raster
  idNoData <- tempDataAdm2 %>% 
    as_tibble() %>% 
    filter(is.na(gdp_pc)) %>% 
    dplyr::select(adm2ID)
  
  r_gdp_adm2_polyg_5arcmin[r_gdp_adm2_polyg_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  
  temp_id <-  as.numeric(tempDataadm2_wTrend$rowNmb)
  temp_v <- as.numeric(tempDataadm2_wTrend$slope)
  
  # reclassify
  slope_raster <- classify(r_gdp_adm2_polyg_5arcmin,
                           cbind(temp_id, temp_v))
  
  names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )
  
  #plot(slope_raster)
  
  terra::writeRaster(slope_raster,paste0('results/rast_adm2_slope_log10', IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'), 
                     gdal="COMPRESS=LZW",overwrite=TRUE)
  
  
}


poly_gdp <- myFun_gdp_data2gpkg(inYears = 1990:2022, 
                                IndexName = 'gdp_pc', 
                                inDataAdm2 = adm2_gdp) 




#### for GBR, the admin 1 data is more accurate; let's use that ----

v_adm2 <- read_sf("results/polyg_adm2_gdp_pc_1990_2022.gpkg")
v_adm1 <- read_sf("results/polyg_adm1_gdp_perCapita_1990_2022.gpkg")

v_adm1_uk <- v_adm1 %>% 
  filter(iso3 == "GBR") %>% 
  rename(NAME_2 = Subnat) %>% 
  rename(adm2ID = GID_nmbr) %>% 
  select(-Country)

v_adm2_uk <- v_adm2 %>% 
  filter(!iso3 == "GBR") %>% 
  bind_rows(v_adm1_uk) %>% 
  arrange(iso3, adm2ID)

st_write(v_adm2_uk,"results/polyg_adm2_gdp_pc_1990_2022.gpkg",delete_dsn=T)



v_adm1_adm0 <- v_adm1 %>% 
  filter(GID_nmbr > 1000) %>% 
  distinct(iso3)

v_adm1_adm1 <- v_adm1 %>% 
  filter(GID_nmbr > 1000) %>% 
  distinct(GID_nmbr)

