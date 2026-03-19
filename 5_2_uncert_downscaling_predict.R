

### downscaling prediction

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

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


#### 2. load admin2 polygon layer -----

adm2_polyg_comb <- st_read('results/adm2_polyg_comb.gpkg') 


##### 3. get data for each adm2 unit -----


load('results/ext_data.RData') 


## 3.2 load long format

adm2_polyg_comb_wData_final <- as_tibble(data.table::fread('results/adm2_polyg_comb_wData.csv') )



## 4. load file for downscaling  ----

adm2_polyg_comb_Matlab_reg_ratio <- read_csv( 'downscalingMatlab/adm2DataForDownscaling_ratio.csv')




### 5. run Python script -----

# here the Python scripts are executed


#### 6. load downscaled GDP  -----

# 6.1 load the data resulted from matlab

variable_in <- 'adm2_gdp_pc_cv'

# downscaledGDPratio <- as_tibble(data.table::fread('downscalingMatlab/xgboost_downscaled_output.csv',
#                                                   fill = TRUE)) 
downscaledGDPratio <- as_tibble(data.table::fread('downscalingMatlab/GDP Uncertainty Results_v2/adm2DataForDownscaling_predictions_with_uncertainty.csv')) %>% 
  rename(predicted.gdp_sd.adm2 = !!as.name(variable_in)) 

nrow(downscaledGDPratio)

test <- downscaledGDPratio %>% 
  filter(is.na(predicted.gdp_sd.adm2)) 

summary(downscaledGDPratio)
# few negative values; find those, calculate mean ratio
df <- tibble(
  year = 2000:2010,
  value = c(1.1, 1.3, NA, 1.6, NA, NA, 2.0, 2.1, NA, 2.4, 2.5)
)

downscaledGDPratio <- downscaledGDPratio %>% 
  select(iso3, GID_2, GID_nmbr, year, predicted.gdp_sd.adm2) %>% 
  mutate(predicted.gdp_sd.adm2 = as.numeric(predicted.gdp_sd.adm2),
         year = as.character(year),
         GID_nmbr = as.character(GID_nmbr)) 
# 
# test_negative <- downscaledGDPratio %>% 
#   filter(predicted.gdp_sd.adm2 < 0) %>% 
#   distinct(GID_2) 
# 
# mean_GID_2_negative <- downscaledGDPratio %>% 
#   filter(GID_2 %in% test_negative$GID_2) %>% 
#   # negative to NA
#   mutate(predicted.gdp_sd.adm2 = ifelse(predicted.gdp_sd.adm2 < 0, NA, predicted.gdp_sd.adm2) ) %>% 
#   # interpolate missing values
#   group_by(GID_2) %>% 
#   mutate(
#     # First interpolate inner NAs (keep edge NAs)
#     predicted.gdp_sd.adm2_interp = na.approx(predicted.gdp_sd.adm2, x = year, na.rm = FALSE),
#     # Then fill edge NAs using last/next observation
#     predicted.gdp_sd.adm2_interp = na.locf(predicted.gdp_sd.adm2_interp, na.rm = FALSE),  # forward fill
#     predicted.gdp_sd.adm2_interp = na.locf(predicted.gdp_sd.adm2_interp, fromLast = TRUE, na.rm = FALSE)  # backward fill
#   ) %>% 
#   ungroup() %>% 
#   mutate(predicted.gdp_sd.adm2 = predicted.gdp_sd.adm2_interp) %>% 
#   select(-predicted.gdp_sd.adm2_interp)
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
#     rolling_median = rollmedian(predicted.gdp_sd.adm2, k = 3, fill = NA, align = "center"),
#     # Manually define edge medians (use average of two neighbors)
#     rolling_median = case_when(
#       year == 1 ~ median(c(lead(predicted.gdp_sd.adm2)), na.rm = TRUE),
#       year == n() ~ median(c(lag(predicted.gdp_sd.adm2)), na.rm = TRUE),
#       TRUE ~ rolling_median
#     ),
#     deviation = abs(predicted.gdp_sd.adm2 - rolling_median) / rolling_median,
#     is_outlier = if_else(!is.na(deviation) & deviation > 0.2, TRUE, FALSE),
#     smoothed = if_else(is_outlier, rolling_median, predicted.gdp_sd.adm2)
#   ) %>%
#   ungroup() %>%
#   mutate(predicted.gdp_sd.adm2 = smoothed) %>%
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
  select(iso3,  GID_2, NAME_2, year, gdp_adm1, GID_nmbr, predicted.gdp_sd.adm2, pop) %>% 
  rename(gdp_adm2 = predicted.gdp_sd.adm2) %>% 
  mutate(gdp_adm1 = as.numeric(gdp_adm1)) %>% 
  mutate(gdpRatio = gdp_adm2/gdp_adm1) %>% 
  mutate(gdp_adm2_x_pop = gdp_adm2 * pop)
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


adm2_gdp_rowNbr <- read_csv('results/adm2_gdp.csv') %>% 
  select(GID_2, adm2ID, rowNumb) %>% 
  distinct()

adm2_gdp <- adm2_gdp_temp %>% 
  left_join(adm2_gdp_rowNbr) %>% 
  mutate(GID_nmbr = as.factor(GID_nmbr)) %>% 
  mutate(year = as.character(year)) #%>% 


temp_valueNA <- adm2_gdp %>% 
  filter(is.na(gdp_adm2)) 


missing_adm2_polyg_comb <- adm2_polyg_comb %>% 
  filter(!GID_2 %in% adm2_gdp$GID_2) %>% 
  st_drop_geometry()



data.table::fwrite(adm2_gdp, 'results/adm2_gdp_uncert.csv')





##### 7. harmonise against admin 1 level -----

# 
# DataAdm1 = read.csv('results/tabulated_gdp_perCapita.csv')  %>% as_tibble() %>% 
#   select(GID_nmbr, X1990:X2024) %>% 
#   set_names(c('GID_nmbr', paste0(1990:2024))) %>% 
#   pivot_longer(names_to = 'year', values_to = 'gdp_adm1', -c(GID_nmbr))
# DataAdm2 = adm2_gdp
# # pop
# 
# adm2_polyg_comb <- read_sf('results/adm2_polyg_comb.gpkg')
# adm2_gdp_temp4 <- adm2_gdp %>% 
#   select(GID_2,adm2ID) %>% distinct(GID_2, .keep_all=T)
# 
# adm2_polyg_comb_ID <- adm2_polyg_comb %>% 
#   left_join(adm2_gdp_temp4) %>% 
#   mutate(rowNmb = row_number()) %>% 
#   # remove caspian sea
#   filter(!iso3 == 'XCA') 
# 
# 
# 
# 
# #adm2_polyg_comb_ID <- read_sf('data_gis/adm2_polyg_comb_ID.gpkg' )
# r_popCount_mod_ext <- subset(rast('data_gis/r_pop_GHS_1989_2024_5arcmin.tif'), 2:36)
# ext_pop <- exactextractr::exact_extract(x= r_popCount_mod_ext,y=adm2_polyg_comb_ID, fun='sum')
# 
# 
# 
# ext_pop_long <- adm2_polyg_comb_ID %>% 
#   st_drop_geometry() %>% 
#   bind_cols(ext_pop) %>% 
#   select(adm2ID, sum.pop1990:sum.pop2024) %>% 
#   set_names(c('adm2ID', paste0(1990:2024))) %>% 
#   pivot_longer(names_to = 'year', values_to = 'popCount', -c(adm2ID)) %>% 
#   distinct(adm2ID, year, .keep_all = T)
# 
# DataAdm2_pop <- DataAdm2 %>% 
#   mutate(GID_nmbr = as.character(GID_nmbr)) %>% 
#   mutate(year = as.character(year)) %>% 
#   left_join(DataAdm1 %>%   mutate(GID_nmbr = as.character(GID_nmbr))) %>% 
#   filter(year > 1989) %>% 
#   #mutate(gdpAdm1 = gdp*gdpRatio) %>% 
#   mutate(year = as.character(year)) %>% 
#   distinct(adm2ID, year, .keep_all = T)%>% 
#   left_join(ext_pop_long %>% mutate(year = as.character(year)) )
# 
# adm1gdp_DataAdm2 <- DataAdm2_pop %>% 
#   mutate(gdpTot = gdp_adm2corr * popCount) %>% 
#   group_by(year, GID_nmbr) %>%
#   summarise(gdpAdm1_fromAdm2 = sum(gdpTot, na.rm = TRUE), popAdm1_fromAdm2 = sum(popCount, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(gdp1_fromAdm2Gdp = gdpAdm1_fromAdm2 / popAdm1_fromAdm2) %>% 
#   mutate(GID_nmbr = as.character(GID_nmbr)) %>% 
#   left_join(DataAdm1 %>% mutate(year = as.character(year))  %>% mutate(GID_nmbr = as.character(GID_nmbr)) )%>% 
#   mutate(harmon_ratio = gdp1_fromAdm2Gdp / gdp_adm1) 
# 
# 
# 
# adm2_gdp_harm <- adm2_gdp %>% 
#   mutate(GID_nmbr = as.character(GID_nmbr)) %>% 
#   mutate(year = as.character(year)) %>% 
#   left_join(adm1gdp_DataAdm2 %>% select(year, GID_nmbr, harmon_ratio, gdp1_fromAdm2Gdp, gdp_adm1) %>% mutate(year = as.character(year))) %>% 
#   mutate(gdp_adm2corr_harm = gdp_adm2corr / harmon_ratio) %>% 
#   select(-gdp_adm2corr, -harmon_ratio) %>% 
#   rename(gdp_adm2corr = gdp_adm2corr_harm) 
# 
# temp <- adm2_gdp_harm %>%
#   filter(grepl('AUT.5.', GID_2))
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
  
  temp2 <- adm2_polyg_comb_ID %>% 
    filter(grepl('GRL', GID_2))
  temp2
  
  
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

adm2_gdp_unc <- adm2_gdp %>% 
  mutate(gdp_adm2corr = gdp_adm2) 

source('functions/f_gdp_data2raster_adm2.R')

yearsIn = 1990:2024
rast_gdp <- f_gdp_data2raster_adm2(inYears = yearsIn, 
                                   IndexName = 'gdp_pc_unc', 
                                   inDataAdm2 = adm2_gdp_unc) 

### 8 check admin areas where number of admin units is higher in admin 1 data ----


## CV

rast_gdpAdm2_cv <- rast('results/rast_adm2_gdpPerCapita_1990_2024_unHarm.tif')

rast_gdpAdm2 <- terra::rast('results/rast_adm2_gdp_perCapita_1990_2024.tif')

rast_gdpAdm2_sd <- rast_gdpAdm2_cv * rast_gdpAdm2

terra::writeRaster(rast_gdpAdm2_cv,paste0('results/rast_adm2_','gdp','_CV','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                       '.tif'), overwrite=TRUE)

terra::writeRaster(rast_gdpAdm2_sd,paste0('results/rast_adm2_','gdp','_sd','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                          '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)



# Output:
# 1 AFG.1.1_1 
# 2 AFG.1.2_1 
# 3 AFG.1.10_1

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

poly_gdp <- f_gdp_data2gpkg_adm2(inYears = 2000:2024, 
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


inYears = 1990:2024
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

inYears = 2000:2020
names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )

#plot(slope_raster)

terra::writeRaster(slope_raster,paste0('results/rast_adm2_slope_log10', IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'), 
                   gdal="COMPRESS=LZW",overwrite=TRUE)


