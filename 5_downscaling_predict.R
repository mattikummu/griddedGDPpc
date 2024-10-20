

### downscaling prediction

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

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

terra::gdalCache(10000)

#### 1. load data -----

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


#### 2. create admin2 polygon layer -----

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



##### 3. get data for each adm2 unit -----




# 3.1 extract data to adm2 boundaries

if (file.exists('results/ext_data.RData')){
  # load it
  load('results/ext_data.RData') 
} else { #create it
  
  ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)
  
  r_popCount_mod <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')
  
  
  r_urbanisation <- subset(rast('/Users/mkummu/R/misc/percentileNormalisation/output/urbanisationCntryWise_GHS2023a.tif'),1:33)
  
  r_urbanisation_ext <- extend(r_urbanisation,ref_raster_5arcmin)
  ext(r_urbanisation_ext) <- ext(ref_raster_5arcmin)
  
  
  r_gini <- rast('/Users/mkummu/R/gini/results/rast_gini_disp_1990_2021.tif')
  
  r_travelTime <- rast('/Users/mkummu/R/GIS_data_common/travel_time_cities/travel_time_to_cities_11.tif')
  r_travelTime_crop <- aggregate(crop(r_travelTime,ext(subset(r_popCount_mod,1))), fact=10, fun='mean', na.rm=T)
  r_travelTime_crop <- extend(r_travelTime_crop, subset(r_popCount_mod,1))
  
  
  sf_gdpUnits <- st_read('results/polyg_adm1_gdp_perCapita_1990_2022.gpkg') %>% 
    mutate(GID_nmbr = as.character(GID_nmbr))
  
  v_gdpUnits <- vect('results/polyg_adm1_gdp_perCapita_1990_2022.gpkg')
  
  r_adm1adm0comb_1arcmin <- rasterize(v_gdpUnits, rast(ncol=360*60, nrow=180*60), field = 'GID_nmbr')
  r_adm1adm0comb_5arcmin <- terra::aggregate(r_adm1adm0comb_1arcmin,fact=5,fun=modal,na.rm=T)
  
  
  v_adm2_polyg_comb <- read_sf('results/adm2_polyg_comb.gpkg')
  
  ## extract data to admin 2 units
  
  ext_pop_adm2 <- exactextractr::exact_extract(r_popCount_mod,v_adm2_polyg_comb, 'sum')
  
  ext_urb_x_pop_adm2 <- exactextractr::exact_extract(r_popCount_mod*r_urbanisation_ext,
                                                     v_adm2_polyg_comb, 'sum')  
  ext_gini_x_pop_adm2 <- exactextractr::exact_extract(r_popCount_mod*
                                                        c(r_gini, subset(r_gini,32)),
                                                      v_adm2_polyg_comb, 'sum')
  ext_travelTime_x_pop_adm2 <- exactextractr::exact_extract(r_popCount_mod*r_travelTime_crop,v_adm2_polyg_comb, 'sum')  
  
  ## extract data to admin 1 units
  
  ext_adm1units <- exactextractr::exact_extract(r_adm1adm0comb_5arcmin,v_adm2_polyg_comb, 'mode')  
  head(ext_adm1units)
  
  ext_adm1units_modal <- ext_adm1units %>% 
    as_tibble() %>% 
    mutate(ID = row_number()) %>% 
    rename(GID_nmbr = value) %>% 
    group_by(ID) %>% 
    count(ID,GID_nmbr) %>%
    slice(which.max(n))
  
  head(ext_adm1units_modal)
  
  # save
  
  save(ext_pop_adm2, ext_urb_x_pop_adm2, ext_gini_x_pop_adm2,
       ext_travelTime_x_pop_adm2, ext_adm1units_modal, 
       file = "results/ext_data.RData")
}


## 3.2 to long format

if (file.exists('results/adm2_polyg_comb_wData.csv')){
  # load it
  adm2_polyg_comb_wData_final <- as_tibble(data.table::fread('results/adm2_polyg_comb_wData.csv') )
} else { #create it
  
  
  adm2_polyg_comb_pop <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    bind_cols(ext_pop_adm2) %>% 
    select(-c(iso3, NAME_2)) %>% 
    set_names('GID_2',  paste0(1990:2022) ) %>% 
    pivot_longer(-c('GID_2'), names_to = 'year', values_to = 'pop') %>% 
    drop_na()
  
  
  temp2 <- adm2_polyg_comb_pop %>% 
    filter(grepl('SJM',GID_2))
  temp2
  
  
  
  # urbanisation
  
  adm2_polyg_comb_urb <- adm2_polyg_comb %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    bind_cols(ext_urb_x_pop_adm2 / ext_pop_adm2) %>% 
    select(-c(iso3, NAME_2)) %>% 
    # for year 2021, let's use year 2020
    #mutate(yr2021 = ppp_2020) %>% 
    set_names('GID_2',  paste0(1990:2022) ) %>% 
    pivot_longer(-c('GID_2'), names_to = 'year', values_to = 'urb') #%>% 
  
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
    select(-c( iso3, NAME_2)) %>% 
    set_names('GID_2', paste0(1990:2022) ) %>% 
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
    bind_cols(ext_travelTime_x_pop_adm2 / ext_pop_adm2) %>% 
    # for year 2021, let's use year 2020
    #mutate(yr2021 = ppp_2020) %>% 
    select(-c(iso3, NAME_2)) %>% 
    set_names('GID_2', paste0(1990:2022) )%>% 
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
  
  ## if gini == 0, put 0.5
  
  adm2_polyg_comb_wData <- adm2_polyg_comb_wData %>% 
    mutate(gini = ifelse(gini == 0, 0.5, gini))
  
  # check some admin units
  
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
  
  adm2_polyg_comb_wData_final <- adm2_polyg_comb_wData_adm1gdp
  
  data.table::fwrite(adm2_polyg_comb_wData_final, 'results/adm2_polyg_comb_wData.csv')
  
  
  
}



## 4. prepare file for downscaling  ----


if (file.exists('downscalingMatlab/adm2DataForDownscaling.csv')){
  
} else { #create it
  
  
  adm1urb <- adm2_polyg_comb_wData_final %>% 
    mutate(urb_x_pop = urb*pop) %>% 
    group_by(GID_nmbr, year) %>% 
    summarise(urbAdm1 = sum(urb_x_pop) / sum(pop) )
  
  adm1travelTime <- adm2_polyg_comb_wData_final %>% 
    mutate(TT_x_pop = travelTime*pop) %>% 
    group_by(GID_nmbr, year) %>% 
    summarise(travelTimeAdm1 = sum(TT_x_pop) / sum(pop))
  
  
  # 'urb','travelTime','adm0GDP','adm0urb','adm0gini'
  adm2_polyg_comb_Matlab <- adm2_polyg_comb_wData_final %>% 
    rename(adm0gini = gini) %>% 
    rename(adm0GDP = gdp_adm1) %>% 
    left_join(adm1urb) %>% 
    left_join(adm1travelTime) %>% 
    rename(adm0urb = urbAdm1) %>% 
    rename(adm0travelTime = travelTimeAdm1) %>% 
    select('iso3', 'GID_2', 'GID_nmbr', 'year', 'urb','travelTime','adm0GDP','adm0urb','adm0gini', 'adm0travelTime')
  
  temp2 <- adm2_polyg_comb_Matlab %>% 
    filter(grepl('FIN',GID_2))
  temp2
  
  test_na <- adm2_polyg_comb_Matlab %>% 
    filter(is.na(GID_nmbr))
  
  # add regions
  
  cntryID_reg <- read_csv("data_in/countries_codes_and_coordinates_reg.csv") %>% 
    dplyr::select(-cntry_code) %>% 
    rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
    #select(cntry_code,iso2,iso3,Country) %>% 
    mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
    distinct(iso3, .keep_all = T) %>% 
    select(iso3, RegionID, RegName)
  
  
  adm2_polyg_comb_Matlab_reg <- adm2_polyg_comb_Matlab %>% 
    left_join(cntryID_reg)
  
  data.table::fwrite(adm2_polyg_comb_Matlab, 'downscalingMatlab/adm2DataForDownscaling.csv')
  
}



### 5. run matlab script -----

# here the matlab scripts are executed


#### 6. load downscaled GDP ratio -----

# 6.1 load the data resulted from matlab

if (file.exists('results/adm2_gdp.csv')){
  # load it
  adm2_gdp <- as_tibble(data.table::fread('results/adm2_gdp.csv') )
} else { #create it
  
  downscaledGDPratio <- as_tibble(data.table::fread('downscalingMatlab/downsclaling_output.csv')) %>% 
    rename(Estimated.gdpratio = 'ENS_Prediction') %>% 
    # no bias correctpion, use estimated gdp ratio for corrected_prediction
    mutate(Corrected_Prediction = Estimated.gdpratio) %>% 
    rename(Corrected.Estimated.gdpratio = 'Corrected_Prediction') %>% 
    # na to 1
    mutate(Corrected.Estimated.gdpratio = ifelse(is.na(Corrected.Estimated.gdpratio), 1,
                                                 Corrected.Estimated.gdpratio)) # %>% 
  # mutate(year = as.character(year)) #%>% 
  # some negative values, let's add 10 to each
  # mutate(Corrected.Estimated.gdpratio = Corrected.Estimated.gdpratio + 10)
  
  
  adm2_polyg_comb_Matlab_downScaled_noYear2022 <- adm2_polyg_comb_wData_final %>% 
    select(iso3, GID_2, NAME_2, year, gdp_adm1, pop, GID_nmbr) %>% 
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
  
  adm2_polyg_comb_Matlab_downScaled_2022 <- adm2_polyg_comb_wData_final %>% 
    select(iso3, GID_2, NAME_2, year, gdp_adm1, pop, GID_nmbr) %>% 
    left_join(downscaledGDPratio) %>% 
    select(iso3,  GID_2, NAME_2, year, gdp_adm1, GID_nmbr, Corrected.Estimated.gdpratio, pop) %>% 
    rename(gdpRatio = Corrected.Estimated.gdpratio) %>% 
    mutate(gdp_adm1 = as.numeric(gdp_adm1)) %>% 
    filter(year == 2021) %>% 
    mutate(year = 2022) %>% 
    select(-gdp_adm1) %>% 
    left_join(adm1_gdp_pc %>% mutate(year = as.numeric(year)))
  
  # add to other data
  
  adm2_polyg_comb_Matlab_downScaled <- adm2_polyg_comb_Matlab_downScaled_noYear2022 %>% 
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

##### 7. to grid -----

# 7.1 create raster

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
  
  
  # write raster
  terra::writeRaster(r_gdp_adm2_polyg_5arcmin,'data_gis/gdp_Adm2_raster_5arcmin.tif', 
                     gdal="COMPRESS=LZW",overwrite=TRUE)
  
}



### 7.2 put data to raster 

source('functions/f_gdp_data2raster_adm2.R')

yearsIn = 1990:2022
rast_gdp <- f_gdp_data2raster_adm2(inYears = yearsIn, 
                                   IndexName = 'gdp_pc', 
                                   inDataAdm2 = adm2_gdp) 



### 8 check admin areas where number of admin units is higher in admin 1 data ----

# we use the reported data for those countries where adm2 level admin division
# is equal or close to the one of reported

count_adm1_polyg <- st_read('results/polyg_adm1_gdp_perCapita_1990_2022.gpkg') %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(iso3) %>%
  summarise(iso3count = n()) %>% 
  rename(iso3count_1 = iso3count)


count_adm2_polyg <- sf::read_sf('data_gis/adm2_polyg_comb_ID.gpkg' ) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(iso3) %>%
  summarise(iso3count = n()) %>% 
  rename(iso3count_2 = iso3count)


comp_count <- count_adm1_polyg %>% 
  left_join(count_adm2_polyg) %>% 
  mutate(countRatio = iso3count_2/iso3count_1) %>% 
  filter(!iso3count_2 == 1) %>% 
  filter(countRatio < 1.5) %>% 
  left_join(cntry_info)

# for those countries, where ratio of number of admin areas between adm 1 and adm2
# is less than 1.5, let's use the admin 1 data for the final results

rast_gdpAdm2 <- rast('results/rast_adm2_gdpPerCapita_1990_2022_unHarm.tif')
rast_adm1 <- rast("results/rast_adm1_gdp_perCapita_1990_2022.tif")
rast_adm0_admin <- rast("data_gis/gdp_adm0_raster_5arcmin.tif")

rast_gdpAdm2[rast_adm0_admin %in% comp_count$cntry_id] <- rast_adm1

#writeRaster(rast_adm2,"results/rast_adm2_gdpPerCapita_1990_2022.tif",gdal="COMPRESS=LZW",overwrite=TRUE)




#### 9. harmonise against reported adm1 level values ----


sf_adm0 <- read_sf('data_gis/gdp_Adm0Adm1_polyg_simple.gpkg')
sf_adm1_info <- read_sf('results/polyg_adm1_gdp_perCapita_1990_2022.gpkg') %>% 
  st_drop_geometry()

r_popCount_mod_ext <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')


#### 9.1 admin 1 level data, reported (from 1_gdp_prepare_adm0.R)

adm0_comb_interpExtrap <- read_csv('results/tabulated_gdp_perCapita.csv') %>% 
  select(-Country, -Subnat, -slope) %>% 
  pivot_longer(-c(GID_nmbr, iso3), names_to = 'year', values_to = 'gdp')
#rename(gdp = gdp_pc)

### 9.2 national data, from downscaled raster

adm0_polyg_final <- sf_adm0 %>% 
  left_join(sf_adm1_info %>% 
              select(Country, GID_nmbr, iso3) ) %>% 
  # rename(GID_nmbr = cntry_id) %>% 
  # rename(Country = country_name)) %>% 
  filter(!is.na(GID_nmbr)) %>% 
  select(Country, GID_nmbr, iso3)



# rast_adm0_polyg_final <- terra::rasterize(adm0_polyg_final, y = subset(rast_gdpAdm2,1), field = 'GID_nmbr')



## 9.3 extract

# adm2
ext_gdp_x_pop_adm1 <- exactextractr::exact_extract(rast_gdpAdm2*r_popCount_mod_ext,adm0_polyg_final, fun='sum' )
dim(ext_gdp_x_pop_adm1)

# pop
ext_pop <- exactextractr::exact_extract(x= r_popCount_mod_ext,y=adm0_polyg_final, fun='sum')
dim(ext_pop)


# 9.4 weighted average from adm1 raster data, and then calculate ratio between that and reported GDP

sf_adm0_comb_adm1_ratio <- adm0_polyg_final %>%
  bind_cols(ext_gdp_x_pop_adm1 / ext_pop) %>%
  st_drop_geometry() %>% 
  #select(-c(ID)) %>%
  set_names('Country', 'GID_nmbr', 'iso3', paste0(yearsIn) ) %>%
  as_tibble() %>%
  pivot_longer(-c('Country', 'GID_nmbr', 'iso3'), names_to = 'year', values_to = 'gdp_pc_raster') %>%
  mutate(year = as.numeric(year)) %>% 
  distinct(GID_nmbr, year, .keep_all = T) %>%
  mutate(GID_nmbr = as.numeric(GID_nmbr)) %>% 
  left_join(adm0_comb_interpExtrap %>% mutate(year = as.numeric(year)) %>% 
              distinct(GID_nmbr, year, .keep_all = T)  %>% 
              mutate(GID_nmbr = as.numeric(GID_nmbr))) %>%
  # calculate ratio
  mutate(ratio = gdp_pc_raster / gdp) %>% 
  # if ratio is 0 or NA, let's use 1
  mutate(ratio = ifelse(is.na(ratio), 1, ratio )) %>% 
  mutate(ratio = ifelse(ratio == 0, 1, ratio )) 

## 9.5 to map

r_gdp_adm0_polyg_5arcmin <- rast('data_gis/gdp_Adm0Adm1_raster_5arcmin_feb2024.tif')

ratioRaster = rast()

for (iYear in yearsIn) {
  
  tempData_selYear <- sf_adm0_comb_adm1_ratio %>% 
    filter(year == iYear)
  
  temp_id <-  as.numeric(tempData_selYear$GID_nmbr)
  temp_v <- as.numeric(tempData_selYear$ratio)
  
  # reclassify
  temp_raster <- classify(r_gdp_adm0_polyg_5arcmin,
                          cbind(temp_id, temp_v))
  
  #plot(temp_raster)
  
  terra::add(ratioRaster) <- temp_raster
}

# 9.6 calculate final raster

finalRaster <- (rast_gdpAdm2 / ratioRaster)

### harmonisation did not work, for some reason, for Bahamas - let's use national values for it

rast_gdpAdm0 <- rast("results/rast_adm0_gdp_perCapita_1990_2022.tif")
rast_adm0_admin <- rast("data_gis/gdp_adm0_raster_5arcmin.tif")

finalRaster[rast_adm0_admin == 21] <- rast_gdpAdm0



terra::writeRaster(finalRaster,paste0('results/rast_adm2_','gdp','_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)


## xxx. some optional code ----

# 
# 
# 
# 
# ### calculate adm2 - cntry ratio
# 
# adm0gdp_pc <- rast('results/rast_adm0_gdp_perCapita_1990_2022.tif')
# adm2gdp_pc <- rast('results/rast_adm2_gdp_PerCapita_1990_2022.tif')
# 
# ratioGdp_pc <- adm2gdp_pc/adm0gdp_pc
# 
# terra::writeRaster(ratioGdp_pc,'results/rast_gdp_ratio_Adm2Adm0_5arcmin.tif', 
#                    gdal="COMPRESS=LZW",overwrite=TRUE)
# 
# # 
# # ## difference between gdp and gnic
# # 
# # 
# # ratioGnic <- rast('/Users/mkummu/R/hdi_subnat/results/rast_gni_ratio_Adm2Adm0_5arcmin.tif')
# # ratioGdp_pc <- rast('/Users/mkummu/R/subnat_gdp_2023/results/rast_gdp_ratio_Adm2Adm0_5arcmin.tif')
# # 
# # diffGnicGdp <- ratioGnic-ratioGdp_pc
# # 
# # terra::writeRaster(diffGnicGdp,'/Users/mkummu/R/subnat_gdp_2023/results/rast_diff_GnicGdp_ratio_5arcmin.tif', 
# #                    gdal="COMPRESS=LZW",overwrite=TRUE)
# 




### 10. put data to polygons  ----

# 10.1 create simplified polygon layer

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



#### 10.2 put data to gpkg (and slope to raster) 

adm2_count <- adm2_gdp %>% 
  distinct(adm2ID)


# 10.2.1 harmonise adm 2 data

# 
# adm2_gdp_harm <- adm2_gdp %>% 
#   left_join(sf_adm0_comb_adm1_ratio %>% select(GID_nmbr,year, ratio) %>% 
#               rename(ratio_harm = ratio)) %>% 
#   mutate(gdp_adm2corr = gdp_adm2corr/ratio_harm) %>% 
#   select(-ratio_harm)
  
ext_gdp_adm2 <- exactextractr::exact_extract( finalRaster,gdp_adm2_polyg_simpl,'mode')



# 10.2.2 apply the function to

source('functions/f_gdp_data2gpkg_adm2.R')

poly_gdp <- f_gdp_data2gpkg_adm2(inYears = 1990:2022, 
                                 IndexName = 'gdp_pc', 
                                 inDataAdm2 = adm2_gdp) 





### 10.3 check admin areas where number of admin units is equal or higher in admin 1 data ----

# done above (comp_count )

# for those countries, where ratio of number of admin areas between adm 1 and adm2
# is less than 1.5, let's use the admin 1 data for the final results


v_adm2 <- read_sf("results/polyg_adm2_gdp_perCapita_1990_2022.gpkg")
v_adm1 <- read_sf("results/polyg_adm1_gdp_perCapita_1990_2022.gpkg")

v_adm1_sel <- v_adm1 %>% 
  filter(iso3 %in% comp_count$iso3 | iso3 == "BHS") %>% 
  rename(NAME_2 = Subnat) %>% 
  rename(adm2ID = GID_nmbr) %>% 
  select(-Country)

omit_iso3 <- unique(v_adm1_sel$iso3)

v_adm2_final <- v_adm2 %>% 
  filter(!iso3 %in% omit_iso3) %>% 
  bind_rows(v_adm1_sel) %>% 
  arrange(iso3, adm2ID)



temp <- v_adm2_final %>% 
  st_drop_geometry() 

write_csv(temp, "results/tabulated_adm2_gdp_perCapita.csv")


st_write(v_adm2_final,"results/polyg_adm2_gdp_perCapita_1990_2022.gpkg",delete_dsn=T)



