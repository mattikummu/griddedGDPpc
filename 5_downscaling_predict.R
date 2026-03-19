### 5_downscaling_predict.R
# Apply the trained MATLAB neural-network model to downscale admin-1 GDP per capita
# to a 30 arc-sec pixel grid for 1990–2024 and save raster outputs.
# Subnational GDP per capita dataset — Matti Kummu, Aalto University (matti.kummu@aalto.fi)

library(sf)
library(terra)
library(tidyterra)
library(openxlsx) #
library(zoo)

library(broom)
library(tidyr)
library(tidyverse)
library(dplyr)


#
# # set working directory the path that this script is located in
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

v_subnat_gis_combined <- vect("results/polyg_adm1_gdp_perCapita_1990_2024.gpkg")
sf_subnat_gis_combined <- read_sf("results/polyg_adm1_gdp_perCapita_1990_2024.gpkg")
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

  r_popCount_mod <- rast('data_gis/r_pop_GHS_1989_2024_5arcmin.tif') %>%
    subset(2:36)


  r_urbanisation <- subset(rast('../misc/percentileNormalisation/output//urbanisationCntryWise_GHS2023a.tif'), 6:40)

  r_urbanisation_ext <- extend(r_urbanisation,ref_raster_5arcmin)
  ext(r_urbanisation_ext) <- ext(ref_raster_5arcmin)


  r_gini_org <- rast('/Users/mkummu/R/subnatGini/results/rast_gini_disp_1990_2023.tif')
  # for year 2024, let's use 2023
  r_gini <- c(r_gini_org, subset(r_gini_org, 34))

  r_travelTime <- rast('/Users/mkummu/R/GIS_data_common/travel_time_cities/travel_time_to_cities_11.tif')
  # sea to NA
  valueSea <- r_travelTime[1,1]
  r_travelTime[r_travelTime == as.numeric(valueSea)] <- NA
  r_travelTime_crop <- aggregate(crop(r_travelTime,ext(subset(r_popCount_mod,1))), fact=10, fun='mean', na.rm=T)
  r_travelTime_crop <- extend(r_travelTime_crop, subset(r_popCount_mod,1))


  sf_gdpUnits <- st_read('results/polyg_adm1_gdp_perCapita_1990_2024.gpkg') %>%
    mutate(GID_nmbr = as.character(GID_nmbr))

  v_gdpUnits <- vect('results/polyg_adm1_gdp_perCapita_1990_2024.gpkg')

  r_adm1adm0comb_1arcmin <- rasterize(v_gdpUnits, rast(ncol=360*60, nrow=180*60), field = 'GID_nmbr')
  r_adm1adm0comb_5arcmin <- terra::aggregate(r_adm1adm0comb_1arcmin,fact=5,fun='modal',na.rm=T)


  v_adm2_polyg_comb <- read_sf('results/adm2_polyg_comb.gpkg')

  ## extract data to admin 2 units

  ext_pop_adm2 <- exactextractr::exact_extract(r_popCount_mod,v_adm2_polyg_comb, 'sum')

  ext_urb_x_pop_adm2 <- exactextractr::exact_extract(r_popCount_mod*r_urbanisation_ext,
                                                     v_adm2_polyg_comb, 'sum')
  ext_gini_x_pop_adm2 <- exactextractr::exact_extract(r_popCount_mod*r_gini,
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
    set_names('GID_2',  paste0(1990:2024) ) %>%
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
    set_names('GID_2',  paste0(1990:2024) ) %>%
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
    set_names('GID_2', paste0(1990:2024) ) %>%
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
    set_names('GID_2', paste0(1990:2024) )%>%
    pivot_longer(-c('GID_2'), names_to = 'year', values_to = 'travelTime')%>%
    drop_na()

  temp2 <- adm2_polyg_comb_traveTime %>%
    filter(grepl('BRB.8',GID_2))
  temp2

  adm2_polyg_comb_adm1unit <- adm2_polyg_comb %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    bind_cols(ext_adm1units_modal ) %>%
    left_join(cntry_info %>% select(iso3, cntry_id)) %>%
    mutate(GID_nmbr = ifelse(GID_nmbr < 1000, cntry_id, GID_nmbr)) %>%
    select(-ID, -n, -iso3, -NAME_2, -cntry_id)  %>%
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
    set_names('GID_nmbr', paste0(1990:2024) ) %>%
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
  adm2_polyg_comb_Matlab_reg_ratio <- read_csv( 'downscalingMatlab/adm2DataForDownscaling_ratio.csv')
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

  data.table::fwrite(adm2_polyg_comb_Matlab_reg, 'downscalingMatlab/adm2DataForDownscaling.csv')
  ### 4.x prediction data with ratios for urb and travel time


  adm2_polyg_comb_Matlab_reg_ratio <- adm2_polyg_comb_Matlab_reg %>%
    mutate(urbRatio = urb / adm0urb) %>%
    mutate(travelTimeRatio = travelTime/adm0travelTime) %>%
    select(-urb, -travelTime) %>%
    select(iso3, GID_2, GID_nmbr, year, urbRatio, travelTimeRatio , everything())

  # write file

  data.table::fwrite(adm2_polyg_comb_Matlab_reg_ratio, 'downscalingMatlab/adm2DataForDownscaling_ratio.csv')

}




### 5. run Python script -----

# here the Python scripts are executed


#### 6. load downscaled GDP ratio -----

# 6.1 load the data resulted from matlab



# downscaledGDPratio <- as_tibble(data.table::fread('downscalingMatlab/xgboost_downscaled_output.csv',
#                                                   fill = TRUE))
downscaledGDPratio <- as_tibble(data.table::fread('downscalingMatlab/GDP Uncertainty Results_v2/adm2DataForDownscaling_predictions_with_uncertainty.csv')) %>%
  rename('predicted.gdp_pc.adm2' = 'adm2_gdp_pc_mean') %>%
  # no bias correctpion, use estimated gdp ratio for corrected_prediction
  # mutate(Corrected_Prediction = Estimated.gdpratio) %>%
  # rename(predicted.gdp_pc.adm2 = 'Corrected_Prediction') %>%
  # # na to 1
  # mutate(predicted.gdp_pc.adm2 = ifelse(is.na(predicted.gdp_pc.adm2), 1,
  #                                              predicted.gdp_pc.adm2)) # %>%
  ## some border cases, GID_nmbr is wrong (Angola, for example) - let's fix that

  left_join(cntry_info %>% select(iso3, cntry_id)) %>%
  mutate(GID_nmbr = ifelse(GID_nmbr < 1000, cntry_id, GID_nmbr)) %>%
  select(-cntry_id)


nrow(downscaledGDPratio)
downscaledGDPratio[nrow(downscaledGDPratio),]

temp333 <- downscaledGDPratio %>%
  filter(GID_nmbr == 3) %>%
  distinct(iso3)



# # few negative values; find those, calculate mean ratio
# df <- tibble(
#   year = 2000:2010,
#   value = c(1.1, 1.3, NA, 1.6, NA, NA, 2.0, 2.1, NA, 2.4, 2.5)
# )
#
# downscaledGDPratio <- downscaledGDPratio %>%
#   select(iso3, GID_2, GID_nmbr, year, predicted.gdp_pc.adm2) %>%
#   mutate(predicted.gdp_pc.adm2 = as.numeric(predicted.gdp_pc.adm2),
#          year = as.character(year),
#          GID_nmbr = as.character(GID_nmbr))
#
# test_negative <- downscaledGDPratio %>%
#   filter(predicted.gdp_pc.adm2 < 0) %>%
#   distinct(GID_2)
#
# mean_GID_2_negative <- downscaledGDPratio %>%
#   filter(GID_2 %in% test_negative$GID_2) %>%
#   # negative to NA
#   mutate(predicted.gdp_pc.adm2 = ifelse(predicted.gdp_pc.adm2 < 0, NA, predicted.gdp_pc.adm2) ) %>%
#   # interpolate missing values
#   group_by(GID_2) %>%
#   mutate(
#     # First interpolate inner NAs (keep edge NAs)
#     predicted.gdp_pc.adm2_interp = na.approx(predicted.gdp_pc.adm2, x = year, na.rm = FALSE),
#     # Then fill edge NAs using last/next observation
#     predicted.gdp_pc.adm2_interp = na.locf(predicted.gdp_pc.adm2_interp, na.rm = FALSE),  # forward fill
#     predicted.gdp_pc.adm2_interp = na.locf(predicted.gdp_pc.adm2_interp, fromLast = TRUE, na.rm = FALSE)  # backward fill
#   ) %>%
#   ungroup() %>%
#   mutate(predicted.gdp_pc.adm2 = predicted.gdp_pc.adm2_interp) %>%
#   select(-predicted.gdp_pc.adm2_interp)
#
# if (nrow(mean_GID_2_negative) > 0) {
#   # If mean_GID_2_negative has rows, perform the filtering AND binding
#   downscaledGDPratio_noNeg <- downscaledGDPratio %>%
#     filter(!GID_2 %in% test_negative$GID_2) %>%
#     bind_rows(mean_GID_2_negative) %>%
#     arrange(GID_2, year)
# } else {
#   # If mean_GID_2_negative is empty, only perform the filtering
#   downscaledGDPratio_noNeg <- downscaledGDPratio %>%
#     filter(!GID_2 %in% test_negative$GID_2) %>%
#     arrange(GID_2, year)
# }
#
# ## there are some outliers in the ratios; let's use moving median to get rid of those
#
# downscaledGDPratio_noNeg_smoothed <- downscaledGDPratio_noNeg %>%
#   group_by(GID_2) %>%
#   mutate(
#     rolling_median = rollmedian(predicted.gdp_pc.adm2, k = 3, fill = NA, align = "center"),
#     # Manually define edge medians (use average of two neighbors)
#     rolling_median = case_when(
#       year == 1 ~ median(c(lead(predicted.gdp_pc.adm2)), na.rm = TRUE),
#       year == n() ~ median(c(lag(predicted.gdp_pc.adm2)), na.rm = TRUE),
#       TRUE ~ rolling_median
#     ),
#     deviation = abs(predicted.gdp_pc.adm2 - rolling_median) / rolling_median,
#     is_outlier = if_else(!is.na(deviation) & deviation > 0.2, TRUE, FALSE),
#     smoothed = if_else(is_outlier, rolling_median, predicted.gdp_pc.adm2)
#   ) %>%
#   ungroup() %>%
#   mutate(predicted.gdp_pc.adm2 = smoothed) %>%
#   select(-c(rolling_median,deviation,is_outlier, smoothed) )  %>%
#   arrange(GID_2, year)
#
# test_fin <- downscaledGDPratio_noNeg_smoothed %>%
#   filter(iso3 == "FIN")


## continue from there

adm2_polyg_comb_Matlab_downScaled <- adm2_polyg_comb_wData_final %>%
  select(iso3, GID_2, NAME_2, year, gdp_adm1, pop, GID_nmbr) %>%
  mutate(year = as.character(year),
         GID_nmbr = as.character(GID_nmbr)) %>%
  left_join(downscaledGDPratio %>%
              mutate(year = as.character(year), GID_nmbr = as.character(GID_nmbr))) %>%
  select(iso3,  GID_2, NAME_2, year, gdp_adm1, GID_nmbr, predicted.gdp_pc.adm2, pop) %>%
  rename(gdp_adm2 = predicted.gdp_pc.adm2) %>%
  mutate(gdp_adm1 = as.numeric(gdp_adm1)) %>%
  mutate(gdp_adm2 = ifelse(is.na(gdp_adm2), gdp_adm1, gdp_adm2)) %>%
  mutate(gdpRatio = gdp_adm2/gdp_adm1) %>%
  mutate(gdp_adm2_x_pop = gdp_adm2 * pop)


temp <- adm2_polyg_comb_Matlab_downScaled %>%
  filter(iso3 == "AGO")
#
# temp4 <- adm2_polyg_comb_Matlab_downScaled %>%
#   filter(grepl('CHN.26.1_1', GID_2))
#
# write_csv(temp4, 'results/temp/downscaled_GDP_AUT.5.csv')
#
# temp5 <- downscaledGDPratio %>%
#   filter(grepl('CHN.5.11_1', GID_2))
#
# write_csv(temp5, 'results/temp/downscaling_results_AUT.5.csv')
# #
# temp2 <- adm2_polyg_comb_Matlab_downScaled %>%
#   filter(GID_2 == 'CHN.26.1_1')
#
# temp2 <- adm2_polyg_comb_Matlab_downScaled %>%
#   filter(grepl('BRB.8',GID_2))
# temp2

adm1gdp_from_adm2gdp <- adm2_polyg_comb_Matlab_downScaled %>%
  group_by(GID_nmbr, year) %>%
  summarise(gdp_adm1_fromAdm2 = sum(gdp_adm2_x_pop, na.rm=T) / sum(pop, na.rm=T)) %>%
  ungroup() %>%
  #distinct(GID_nmbr, year, .keep_all=T) %>%
  left_join(adm2_polyg_comb_Matlab_downScaled %>% select(GID_nmbr,gdp_adm1, year) %>% distinct()) %>%
  mutate(corrRatio = gdp_adm1 / gdp_adm1_fromAdm2) %>%
  select(GID_nmbr, year, corrRatio) #%>%
#distinct(.keep_all=T)


adm2_gdp_temp <- adm2_polyg_comb_Matlab_downScaled %>%
  select(iso3,  GID_2,     NAME_2,  year,  GID_nmbr, gdp_adm2) %>%
  left_join(adm1gdp_from_adm2gdp) %>%
  mutate(gdp_adm2corr = corrRatio*gdp_adm2)


temp <- adm2_gdp_temp %>%
  filter(is.na(gdp_adm2corr))

temp2 <- adm2_polyg_comb_Matlab_downScaled %>%
  filter(GID_2 %in% unique(temp$GID_2))

temp3 <- adm1gdp_from_adm2gdp %>%
  filter(GID_nmbr %in% unique(temp$GID_nmbr))

temp4 <- adm2_polyg_comb_Matlab_downScaled %>%
  filter(GID_nmbr==3) %>%
  filter(if_any(everything(), is.na))

#### 6.1 replace downscaled CHN and BRA with reported data (see codes 2_2 for BRA and CHN) ----

adm2_CHN <- data.table::fread('results/adm2_CHN_gdp_per_capita_1990_2024.csv') %>%
  as_tibble() %>%
  mutate(year = as.integer(year), GID_nmbr = as.integer(GID_nmbr)) %>%
  rename(gdp_adm2corr = adm2_gdp_pc) %>%
  select(-c(NAME_1, GID_1, adm1_gdp_pc, adm2_gdp_ratio, Subnat))
adm2_BRA <- data.table::fread('results/adm2_BRA_gdp_per_capita_1990_2024.csv') %>%
  as_tibble() %>%
  mutate(year = as.integer(year), GID_nmbr = as.integer(GID_nmbr)) %>%
  rename(gdp_adm2corr = adm2_gdp_pc) %>%
  select(-c(adm1_gdp_pc, GID_1, adm2_gdp_ratio, Subnat))


adm2_gdp_temp_w_BRA_CHN <- adm2_gdp_temp %>%
  filter(!GID_2 %in% unique(c(adm2_CHN$GID_2, adm2_BRA$GID_2))) %>%
  mutate(year = as.integer(year), GID_nmbr = as.integer(GID_nmbr)) %>%
  # mutate(year = as.integer(year)) %>%
  bind_rows(adm2_BRA) %>%
  bind_rows(adm2_CHN) %>%
  filter(!GID_2 == "") %>%
  arrange(GID_2, year)

test <- adm2_gdp_temp_w_BRA_CHN %>%
  filter(iso3 == "CHN")
# filter(is.na(gdp_adm2corr))


##### 6.3 add Svalbard (not all data for downscaling available, so not in the datalist) ------
SJM_data <- read.csv('results/tabulated_gdp_perCapita.csv') %>%
  as_tibble() %>%
  filter(iso3 == 'SJM') %>%
  select(-c(Subnat, slope)) %>%
  set_names('GID_nmbr','iso3', 'Country', paste0(1990:2024) )

SJM_data_adm1 <- adm2_polyg_comb %>%
  st_drop_geometry() %>%
  filter(grepl('SJM', GID_2)) %>%
  left_join(SJM_data) %>%
  select(-Country) %>%
  pivot_longer(-c('GID_nmbr','iso3', 'NAME_2', 'GID_2'), names_to = 'year', values_to = 'gdp_adm2corr') %>%
  mutate(gdp_adm2corr = as.numeric(gdp_adm2corr)) %>%
  mutate(GID_nmbr = as.factor(GID_nmbr)) %>%
  mutate(year = as.character(year)) %>%
  # left_join(cntry_info[,c(2,4)]) %>%
  # mutate(rowNumb = as.numeric(substr(GID_2, 5,5 ))) %>%
  # mutate(adm2ID = 2*10^7 + cntry_id*10^4 + rowNumb) %>%
  select(iso3, GID_2, NAME_2, year, GID_nmbr, gdp_adm2corr)

# add also part of Greenland

GRL_data <- read.csv('results/tabulated_gdp_perCapita.csv') %>%
  as_tibble() %>%
  filter(iso3 == 'GRL') %>%
  select(-c(Subnat, slope)) %>%
  set_names('GID_nmbr','iso3', 'Country', paste0(1990:2024) )

GRL.2_1_data_adm1 <- adm2_polyg_comb %>%
  st_drop_geometry() %>%
  filter(grepl('GRL.2_1', GID_2)) %>%
  left_join(GRL_data) %>%
  select(-Country) %>%
  pivot_longer(-c('GID_nmbr','iso3', 'NAME_2', 'GID_2'), names_to = 'year', values_to = 'gdp_adm2corr') %>%
  mutate(gdp_adm2corr = as.numeric(gdp_adm2corr)) %>%
  mutate(GID_nmbr = as.factor(GID_nmbr)) %>%
  mutate(year = as.character(year)) %>%
  # left_join(cntry_info[,c(2,4)]) %>%
  # #mutate(rowNumb = as.numeric(substr(GID_2, 5,5 ))) %>%
  # mutate(adm2ID = 2*10^7 + cntry_id*10^4 + 5) %>%
  select(iso3, GID_2, NAME_2, year, GID_nmbr, gdp_adm2corr)


### 6.4 and part of Bahamas (new provinces; GDP close to 101801)

BHS_data <- read.csv('results/tabulated_gdp_perCapita.csv') %>%
  as_tibble() %>%
  filter(iso3 == 'BHS') %>%
  select(-c(Subnat, slope)) %>%
  set_names('GID_nmbr','iso3', 'Country', paste0(1990:2024) ) %>%
  filter(GID_nmbr == 1018001) %>%
  pivot_longer(-c('GID_nmbr', 'iso3',  'Country'), values_to = 'gdp_pc', names_to = 'year') %>%
  select(year, gdp_pc) %>%
  mutate(year = as.character(year))


BHS_data_adm1 <- adm2_gdp_temp %>%
  #st_drop_geometry() %>%
  as_tibble() %>%
  mutate(year = as.character(year)) %>%
  filter(grepl('BHS', GID_2) & is.na(gdp_adm2corr)) %>%
  left_join(BHS_data) %>%
  mutate(gdp_adm2corr = gdp_pc) %>%
  mutate(GID_nmbr = as.factor(GID_nmbr)) %>%
  select(-gdp_pc)




#### join back ---


adm2_gdp_temp_w_BRA_CHN_GRL_BHS_SJM <- adm2_gdp_temp_w_BRA_CHN %>%
  mutate(GID_nmbr = as.factor(GID_nmbr)) %>%
  mutate(year = as.character(year)) %>%
  # remove BHS that we just filled
  filter(!GID_2 %in% unique(c(BHS_data_adm1$GID_2, SJM_data_adm1$GID_2, GRL.2_1_data_adm1$GID_2))) %>%
  # join back in
  bind_rows(BHS_data_adm1)%>%
  bind_rows(SJM_data_adm1) %>%
  bind_rows(GRL.2_1_data_adm1) %>%
  arrange(str_rank(GID_2, numeric = TRUE))






### add adm2ID

adm2_gdp_rowNbr <- adm2_gdp_temp_w_BRA_CHN_GRL_BHS_SJM %>%
  left_join(cntry_info[,c(2,4)]) %>%
  select(iso3, GID_2, cntry_id) %>%
  distinct() %>%
  group_by(iso3) %>%
  mutate(rowNumb = row_number()) %>%
  ungroup() %>%
  mutate(adm2ID = 2*10^7 + cntry_id*10^4 + rowNumb)



adm2_gdp <- adm2_gdp_temp_w_BRA_CHN_GRL_BHS_SJM %>%
  left_join(adm2_gdp_rowNbr)



##### 6.2 check potential missing areas ------

missing_adm2_polyg_comb <- adm2_polyg_comb %>%
  filter(!GID_2 %in% adm2_gdp$GID_2) %>%
  st_drop_geometry()

write_csv(missing_adm2_polyg_comb, "results/missing_adm2_polyg_comb.csv")


temp_valueNA <- adm2_gdp %>%
  filter(is.na(gdp_adm2corr))



### write final ---

data.table::fwrite(adm2_gdp, 'results/adm2_gdp.csv')

#rm(adm2_polyg_comb_Matlab_downScaled,downscaledGDPratio, adm2_polyg_comb_gini, adm2_polyg_comb_Matlab, adm2_polyg_comb_wData)



##### 7. harmonise against admin 1 level -----


DataAdm1 = read.csv('results/tabulated_gdp_perCapita.csv')  %>% as_tibble() %>%
  select(GID_nmbr, X1990:X2024) %>%
  set_names(c('GID_nmbr', paste0(1990:2024))) %>%
  pivot_longer(names_to = 'year', values_to = 'gdp_adm1', -c(GID_nmbr))
DataAdm2 = adm2_gdp
# pop

adm2_polyg_comb <- read_sf('results/adm2_polyg_comb.gpkg')
adm2_gdp_temp4 <- adm2_gdp %>%
  select(GID_2,adm2ID) %>% distinct(GID_2, .keep_all=T)

adm2_polyg_comb_ID <- adm2_polyg_comb %>%
  left_join(adm2_gdp_temp4) %>%
  mutate(rowNmb = row_number()) %>%
  # remove caspian sea
  filter(!iso3 == 'XCA')




#adm2_polyg_comb_ID <- read_sf('data_gis/adm2_polyg_comb_ID.gpkg' )
r_popCount_mod_ext <- subset(rast('data_gis/r_pop_GHS_1989_2024_5arcmin.tif'), 2:36)
ext_pop <- exactextractr::exact_extract(x= r_popCount_mod_ext,y=adm2_polyg_comb_ID, fun='sum')



ext_pop_long <- adm2_polyg_comb_ID %>%
  st_drop_geometry() %>%
  bind_cols(ext_pop) %>%
  select(adm2ID, sum.pop1990:sum.pop2024) %>%
  set_names(c('adm2ID', paste0(1990:2024))) %>%
  pivot_longer(names_to = 'year', values_to = 'popCount', -c(adm2ID)) %>%
  distinct(adm2ID, year, .keep_all = T)

DataAdm2_pop <- DataAdm2 %>%
  mutate(GID_nmbr = as.character(GID_nmbr)) %>%
  mutate(year = as.character(year)) %>%
  left_join(DataAdm1 %>%   mutate(GID_nmbr = as.character(GID_nmbr))) %>%
  filter(year > 1989) %>%
  #mutate(gdpAdm1 = gdp*gdpRatio) %>%
  mutate(year = as.character(year)) %>%
  distinct(adm2ID, year, .keep_all = T)%>%
  left_join(ext_pop_long %>% mutate(year = as.character(year)) )

adm1gdp_DataAdm2 <- DataAdm2_pop %>%
  mutate(gdpTot = gdp_adm2corr * popCount) %>%
  group_by(year, GID_nmbr) %>%
  summarise(gdpAdm1_fromAdm2 = sum(gdpTot, na.rm = TRUE), popAdm1_fromAdm2 = sum(popCount, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gdp1_fromAdm2Gdp = gdpAdm1_fromAdm2 / popAdm1_fromAdm2) %>%
  mutate(GID_nmbr = as.character(GID_nmbr)) %>%
  left_join(DataAdm1 %>% mutate(year = as.character(year))  %>% mutate(GID_nmbr = as.character(GID_nmbr)) )%>%
  mutate(harmon_ratio = gdp1_fromAdm2Gdp / gdp_adm1)



adm2_gdp_harm <- adm2_gdp %>%
  mutate(GID_nmbr = as.character(GID_nmbr)) %>%
  mutate(year = as.character(year)) %>%
  left_join(adm1gdp_DataAdm2 %>%
              select(year, GID_nmbr, harmon_ratio, gdp1_fromAdm2Gdp, gdp_adm1) %>%
              mutate(year = as.character(year))) %>%
  mutate(gdp_adm2corr_harm = gdp_adm2corr / harmon_ratio) %>%
  select(-gdp_adm2corr, -harmon_ratio) %>%
  rename(gdp_adm2corr = gdp_adm2corr_harm)

temp <- adm2_gdp_harm %>%
  filter(grepl('AUT.5.', GID_2))
#write_csv(temp, 'results/temp/downscaled_harmonised_GDP_AUT.5.csv')

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

  # rasterise to 1 arc min resolutions

  adm2_gdp_temp2 <- adm2_gdp_harm %>%
    select(GID_2,adm2ID) %>% distinct(GID_2, .keep_all=T)


  adm2_polyg_comb_ID <- adm2_polyg_comb %>%
    left_join(adm2_gdp_temp2) %>%
    mutate(rowNmb = row_number()) %>%
    # remove caspian sea
    filter(!iso3 == 'XCA')

  # temp2 <- adm2_polyg_comb_ID %>%
  #   filter(grepl('GRL', GID_2))
  # temp2


  sf::write_sf(adm2_polyg_comb_ID, 'data_gis/adm2_polyg_comb_ID.gpkg' )


  #v_adm2_polyg_comb <- vect('data_gis/adm2_polyg_comb_ID.gpkg')

  source('functions/f_simplify_polyg.R')

  # simplify polygon
  v_adm2_polyg_comb_simpl <-  f_simplify_polyg(input_gpkg_file = 'data_gis/adm2_polyg_comb_ID.gpkg',
                                               output_gpkg_file = 'data_gis/adm2_polyg_comb_ID_simpl.gpkg',
                                               simplify_percentage = 1)

  v_adm2_polyg_comb_simpl <- vect(v_adm2_polyg_comb_simpl)

  r_gdp_adm2_polyg_1arcmin <-  rasterize(v_adm2_polyg_comb_simpl,ref_rast_1arcmin,field="rowNmb")
  # write raster
  terra::writeRaster(r_gdp_adm2_polyg_1arcmin,'data_gis/gdp_Adm2_raster_1arcmin.tif',
                     gdal="COMPRESS=LZW",overwrite=TRUE)
  # unique(r_gdp_adm2_polyg_1arcmin)


  # aggregate to 5 arc-min
  r_gdp_adm2_polyg_5arcmin <- terra::aggregate(r_gdp_adm2_polyg_1arcmin,fact=5,fun=modal,na.rm=T)


  # write raster
  terra::writeRaster(r_gdp_adm2_polyg_5arcmin,'data_gis/gdp_Adm2_raster_5arcmin.tif',
                     gdal="COMPRESS=LZW",overwrite=TRUE)

}



### 7.2 put data to raster

source('functions/f_gdp_data2raster_adm2.R')

yearsIn = 1990:2024
rast_gdp <- f_gdp_data2raster_adm2(inYears = yearsIn,
                                   IndexName = 'gdp_pc',
                                   inDataAdm2 = adm2_gdp_harm)



### 8 check admin areas where number of admin units is higher in admin 1 data ----

# we use the reported data for those countries where adm2 level admin division
# is equal or close to the one of reported

count_adm1_polyg <- st_read('results/polyg_adm1_gdp_perCapita_1990_2024.gpkg') %>%
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

rast_gdpAdm2 <- rast('results/rast_adm2_gdpPerCapita_1990_2024_unHarm.tif')
rast_adm1 <- rast("results/rast_adm1_gdp_perCapita_1990_2024.tif")
rast_adm0_admin <- rast("data_gis/gdp_adm0_raster_5arcmin.tif")

rast_gdpAdm2[rast_adm0_admin %in% comp_count$cntry_id] <- rast_adm1

#writeRaster(rast_adm2,"results/rast_adm2_gdpPerCapita_1990_2022.tif",gdal="COMPRESS=LZW",overwrite=TRUE)

terra::writeRaster(rast_gdpAdm2,paste0('results/rast_adm2_','gdp','_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                       '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)



### 10. put data to polygons  ----

# 10.1 load simplified polygon layer

gdp_adm2_polyg_simpl <-  st_read('data_gis/adm2_polyg_comb_ID_simpl.gpkg')

# if (file.exists('data_gis/adm2_polyg_comb_5arcmin_simple.gpkg')){
#   # load it
#   gdp_adm2_polyg_simpl <- st_read('data_gis/adm2_polyg_comb_5arcmin_simple.gpkg')
#   gdp_adm2_polyg_simpl_1arcmin <- st_read('data_gis/adm2_polyg_comb_1arcmin_simple.gpkg')
# } else {
#   # create it
#
#   p_5arcmin <- as.polygons(r_gdp_adm2_polyg_5arcmin)
#   p_1arcmin <- as.polygons(r_gdp_adm2_polyg_1arcmin)
#   # as.data.frame(p)
#
#   writeVector(p_5arcmin, 'data_gis/adm2_polyg_comb_5arcmin_simple.gpkg', overwrite=T)
#
#   writeVector(p_1arcmin, 'data_gis/adm2_polyg_comb_1arcmin_simple.gpkg', overwrite=T)
#
#   gdp_adm2_polyg_simpl <- st_read('data_gis/adm2_polyg_comb_5arcmin_simple.gpkg')
#   gdp_adm2_polyg_simpl_1arcmin <- st_read('data_gis/adm2_polyg_comb_1arcmin_simple.gpkg')
#
#   # writeVector(p_1arcmin, 'data_gis/adm2_polyg_comb_1arcmoin_simple.gpkg', overwrite=T)
#
#   # gdp_adm2_polyg_simpl <- st_as_sf(raster::raster(r_gdp_adm2_polyg_5arcmin))  %>%
#   #
#   #   sf::st_simplify(., preserveTopology = T, dTolerance = 0.1)
#   #
#   #
#   # st_write(gdp_adm2_polyg_simpl, 'data_gis/GDL_regions_v7_simpl.gpkg', delete_dsn=T)
#
# }



#### 10.2 put data to gpkg (and slope to raster)

adm2_count <- adm2_gdp_harm %>%
  distinct(adm2ID)



# 10.2.2 apply the function to

source('functions/f_gdp_data2gpkg_adm2.R')

poly_gdp <- f_gdp_data2gpkg_adm2(inYears = 1990:2024,
                                 IndexName = 'gdp_pc',
                                 inDataAdm2 = adm2_gdp_harm,
                                 gdp_adm2_polyg = gdp_adm2_polyg_simpl,
                                 nameFile = 'polyg_adm2_gdp_perCapita_')



### 10.3 check admin areas where number of admin units is equal or higher in admin 1 data ----

# done above (comp_count )

# for those countries, where ratio of number of admin areas between adm 1 and adm2
# is less than 1.5, let's use the admin 1 data for the final results


v_adm2 <- read_sf("results/polyg_adm2_gdp_perCapita_1990_2024.gpkg")
v_adm1 <- read_sf("results/polyg_adm1_gdp_perCapita_1990_2024.gpkg")

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

write_csv(temp, "results/polyg_adm2_gdp_perCapita_1990_2024.csv")


st_write(v_adm2_final,"results/polyg_adm2_gdp_perCapita_1990_2024.gpkg",delete_dsn=T)

#slope to raster

# rasterise
slope_raster_1arcmin <- terra::rasterize(v_adm2_final, rast(ncol=360*60, nrow=180*60), field='slope')

# aggregate to 5 arc-min
slope_raster <- terra::aggregate(slope_raster_1arcmin,fact=5,fun=modal,na.rm=T)


inYears    <- 1990:2024
IndexName  <- 'gdp_pc'
names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )

#plot(slope_raster)

terra::writeRaster(slope_raster,paste0('results/rast_adm2_slope_log10', IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'),
                   gdal="COMPRESS=LZW",overwrite=TRUE)






#### for 1990-2020

v_adm2 <- read_sf("results/polyg_adm2_gdp_perCapita_2000_2020.gpkg")
v_adm1 <- read_sf("results/polyg_adm1_gdp_perCapita_1990_2022.gpkg") %>%
  select(-paste0(1990:1999)) %>%
  select(-paste0(2021:2022))

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

st_write(v_adm2_final,"results/polyg_adm2_gdp_perCapita_2000_2020.gpkg",delete_dsn=T)


### slope to raster



# rasterise
slope_raster_1arcmin <- terra::rasterize(v_adm2_final, rast(ncol=360*60, nrow=180*60), field='slope')

# aggregate to 5 arc-min
slope_raster <- terra::aggregate(slope_raster_1arcmin,fact=5,fun=modal,na.rm=T)

inYears    <- 2000:2020
IndexName  <- 'gdp_pc'
names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )

#plot(slope_raster)

terra::writeRaster(slope_raster,paste0('results/rast_adm2_slope_log10', IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'),
                   gdal="COMPRESS=LZW",overwrite=TRUE)
