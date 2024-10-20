
## prepare spatial data and rasterise the values


library(sf)
library(terra)

library(zoo)
library(purrr)
library(broom)
library(mblm)

library(fasterize)

library(tidyverse)
library(dplyr) 

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set time steps
timestep <- c(seq(1990, 2022))
step <- c(seq(1,length(timestep)))


#### 1. load  data ----

# 1.1 read cntry metadata
cntry_info <- read_csv("data_in/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso3 = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso3 = ifelse(iso3 == 'XKX','KSV',iso3)) %>% 
  # northern cyprus
  mutate(iso3 = ifelse(iso3 == 'XNC','ZNC',iso3)) %>%  
  distinct(iso3, .keep_all = T)

cntry_info_temp <- cntry_info %>% 
  rename(country = country_name) %>% 
  select(country,iso3)

GADM_data <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
  st_drop_geometry() %>% 
  mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0))  # kosovo to correct iso3

# many border areas with non-recognisable iso3 code
GADM_data <- GADM_data %>% 
  #mutate(GID_0 = ifelse(GID_0 == 'XNC','ZNC',GID_0)) %>% 
  mutate(GID_0 = ifelse(GID_0 %in% c('Z02', 'Z03', 'Z08'),'CHN',
                        ifelse(GID_0 %in% c('Z06'), 'PAK', 
                               ifelse(GID_0 %in% c('Z01', 'Z04','Z05','Z07','Z09'), 'IND', 
                                      GID_0)) ) )


# 1.2 load adm0 and adm1 data

adm0_comb_interpExtrap <- read_csv('results/adm0_gdp_pc_long_interpExtrap.csv') %>% 
  rename(gdp = gdp_pc)
adm1_ratioAdm1Adm0_interp <- read_csv('results/adm1_gdp_ratio_interp_feb2024.csv') %>% 
  # replace NA ratio with 1; i.e. we use national value for those
  mutate(gdpRatio = ifelse(is.na(gdpRatio), 1, gdpRatio))



##### 2. create / load polygon data ------


if (file.exists('data_gis/gdp_adm0adm1_polyg_feb2024.gpkg')){
  # load it
  gdp_adm0adm1_polyg <- read_sf('data_gis/gdp_adm0adm1_polyg_feb2024.gpkg') 
} else { 
  # create it
  adm1_polyg <- read_sf('results/gisData_GDP_pc_combined_feb2024.gpkg') %>% 
    mutate(iso_code = ifelse(iso3 == 'XKO', 'KSV', iso3)) # kosovo to correct iso3
  
  
  adm1_nogeom <- adm1_polyg %>% 
    st_drop_geometry()
  
  sf_adm0_polyg_diss <- read_sf('results/polyg_gdp_pc_adm0_1990_2021.gpkg')
  
  
 
  # check which countries are not in adm1_polyg
  
  amd1_data_uniqueISO3 <- unique(adm1_ratioAdm1Adm0_interp$iso3) %>% 
    as_tibble() %>% 
    set_names('iso3') %>% 
    mutate(iso3_adm1data = iso3)
  
  adm1_polyg_check <- adm1_polyg %>% 
    st_drop_geometry() %>% 
    select(iso3) %>% 
    distinct(iso3)%>% 
    #rename(iso3 = iso_code) %>% 
    full_join(amd1_data_uniqueISO3) %>% 
    full_join(cntry_info_temp %>% mutate(iso3_adm0Data = iso3),by='iso3') 
  
  iso3_onlyInAdm0 <- adm1_polyg_check %>% 
    
    filter(is.na(iso3_adm1data)) %>% 
    select(iso3) %>% 
    drop_na()
  
  
  
  adm0_polyg_sel <- sf_adm0_polyg_diss %>% 
    #st_drop_geometry() %>% 
    rename(Country = Country) %>% 
    mutate(iso3 = ifelse(iso3 == 'XKO','KSV',iso3)) %>% 
    filter(iso3 %in% iso3_onlyInAdm0$iso3) %>% # join the missing countries in HDI polyg; so that only those are selected
    filter(iso3 != 'ALA' & iso3 != 'XCA' & iso3 != 'ATA') %>%  # remove Åland (part of Finland); Caspian Sea (not needed), Antarctica
    mutate(GID_nmbr = paste0(iso3,'t')) %>% 
    mutate(continent = 'GADM') %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    mutate(GID_nmbr = cntry_id) %>% 
    rename(cntry_code = cntry_id) %>% 
    mutate(Subnat = 'Only adm0 data') %>% 
    select(Country, GID_nmbr, iso3,GID_nmbr, geom) 
  
  
  
  # adm1_polyg_updated_check <- adm1_polyg_updated %>% 
  #   st_drop_geometry()
  # combine sHDI and GADM datasets
  adm1_polyg_comb <- adm1_polyg %>% 
    filter(GID_nmbr %in% unique(adm1_ratioAdm1Adm0_interp$GID_nmbr)) #%>%  # choose only those regions we have data for
  #rename(geom = geometry, iso3 = iso_code)
  
  gdp_adm0adm1_polyg <- bind_rows(adm1_polyg_comb,adm0_polyg_sel) 
  
  adm1ids_cntry <- unique(adm1_ratioAdm1Adm0_interp$iso3) %>% 
    as_tibble() %>% 
    rename(iso3 = value) %>% 
    left_join(.,cntry_info)
  

  test_gdp_adm0adm1_polyg <- gdp_adm0adm1_polyg  %>% 
    st_drop_geometry()
  
  write_sf(gdp_adm0adm1_polyg,'data_gis/gdp_adm0adm1_polyg_feb2024.gpkg')
}


#### 3. create adm0, adm1 raster -----------------------------------------------------

if (file.exists('data_gis/gdp_Adm0Adm1_raster_5arcmin_feb2024.tif')){
  # load it
  r_gdp_adm0adm1_polyg_5arcmin <- rast('data_gis/gdp_Adm0Adm1_raster_5arcmin_feb2024.tif')
} else { 
  # create it
  
  #create ref raster
  ref_raster_5arcmin <- raster::raster(ncol=360*12, nrow=180*12)
  ref_raster_1arcmin <- raster::raster(ncol=360*60, nrow=180*60)
  
  # rasterise to 1 arc min resolutions
  
  # make valid
  # gdp_adm0adm1_polyg_cast <- st_cast(gdp_adm0adm1_polyg, "MULTIPOLYGON")
  # gdp_adm0adm1_polyg_cast_pol <- st_cast(gdp_adm0adm1_polyg, "POLYGON")
  
  #df_adm_valid <- st_make_valid(gdp_adm0adm1_polyg)
  
  v_gdp_adm0adm1_polyg <- vect('data_gis/gdp_adm0adm1_polyg_feb2024.gpkg')
  
  v_df_adm_valid <- makeValid(v_gdp_adm0adm1_polyg)
  
  #v_cast_v_df_adm_valid <- terra::disagg(v_df_adm_valid)
  
  #adm2_polyg_comb <- st_as_sf(v_df_adm_valid)
  
  #adm2_polyg_comb_cast <- st_cast(adm2_polyg_comb, "MULTIPOLYGON")
  
  #adm2_polyg_comb_cast_noGeom <- adm2_polyg_comb_cast %>% st_drop_geometry()
  
  r_gdp_adm0adm1_polyg_1arcmin_terra <- terra::rasterize(x = v_df_adm_valid, y = rast(ref_raster_1arcmin), field = "GID_nmbr")
  
  # r_gdp_adm0adm1_polyg_1arcmin <-  rast(fasterize(adm2_polyg_comb,ref_raster_1arcmin,field="GID_nmbr"))
  
  
  # aggregate to 5 arc-min
  r_gdp_adm0adm1_polyg_5arcmin <- terra::aggregate(r_gdp_adm0adm1_polyg_1arcmin_terra,fact=5,fun=modal,na.rm=T)
  
  
  # write raster
  terra::writeRaster(r_gdp_adm0adm1_polyg_5arcmin,'data_gis/gdp_Adm0Adm1_raster_5arcmin_feb2024.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
}




### 4. put data to raster -----------------------------------------------------

source('functions/f_gdp_data2raster_adm1.R')


yearsIn = 1990:2022

rast_gdp <- f_gdp_data2raster_adm1(inYears = yearsIn, 
                                  IndexName = 'gdp', 
                                  inDataAdm0 = adm0_comb_interpExtrap , 
                                  inDataAdm1 = adm1_ratioAdm1Adm0_interp) 

terra::writeRaster(rast_gdp,paste0('results/rast_adm1_','gdp','_perCapita_unharm_','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)


#### 5. harmonise against reported national values ----

# 5.1 load data

sf_adm0 <- read_sf('data_gis/gdp_adm0_polyg_simple.gpkg')

rast_gdpAdm1 <- rast_gdp

r_popCount_mod_ext <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')


#### national data, reported (from 1_gdp_prepare_adm0.R)

adm0_reported <- adm0_comb_interpExtrap 

### national data, from downscaled raster

adm0_polyg_final <- sf_adm0 %>% 
  left_join(cntry_info %>% 
              select(country_name, iso3, cntry_id)  %>% 
              rename(GID_nmbr = cntry_id) %>% 
              rename(Country = country_name)) %>% 
  filter(!is.na(GID_nmbr)) %>% 
  select(Country, GID_nmbr, iso3)

# adm0 raster
r_gdp_adm0_polyg_5arcmin <- rast('data_gis/gdp_Adm0_raster_5arcmin.tif')


## 5.2 extract

# increase gdal memory size
terra::gdalCache(20000)

rast_gdpAdm1[rast_gdpAdm1 < 0] = NA
r_popCount_mod_ext[r_popCount_mod_ext < 0] = NA

# adm1
ext_gdp_x_pop_adm1 <- exactextractr::exact_extract(rast_gdpAdm1*r_popCount_mod_ext,adm0_polyg_final, fun='sum' )
dim(ext_gdp_x_pop_adm1)

# pop
ext_pop <- exactextractr::exact_extract(x= r_popCount_mod_ext,y=adm0_polyg_final, fun='sum')
dim(ext_pop)

# 5.3 weighted average from adm1 raster data, and then calculate ratio between that and reported GDP

sf_adm0_comb_adm1_ratio <- adm0_polyg_final %>%
  bind_cols(ext_gdp_x_pop_adm1 / ext_pop) %>%
  st_drop_geometry() %>% 
  #select(-c(ID)) %>%
  set_names('Country', 'GID_nmbr', 'iso3', paste0(1990:2022) ) %>%
  as_tibble() %>%
  pivot_longer(-c('Country', 'GID_nmbr', 'iso3'), names_to = 'year', values_to = 'gdp_pc_raster') %>%
  mutate(year = as.numeric(year)) %>% 
  distinct(GID_nmbr, year, .keep_all = T) %>%
  left_join(adm0_reported %>% mutate(year = as.numeric(year)) %>% distinct(iso3, year, .keep_all = T)  ) %>%
  # calculate ratio
  mutate(ratio = gdp_pc_raster / gdp) %>% 
  # if ratio is 0 or NA, let's use 1
  mutate(ratio = ifelse(is.na(ratio), 1, ratio )) %>% 
  mutate(ratio = ifelse(ratio == 0, 1, ratio )) 

## 5.4 back to map


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

finalRaster <- rast_gdp / ratioRaster


### harmonisation did not work, for some reason, for Bahamas - let's use nationa values for it

rast_gdpAdm0 <- rast("results/rast_adm0_gdp_perCapita_1990_2022.tif")
rast_adm0_admin <- rast("data_gis/gdp_adm0_raster_5arcmin.tif")

finalRaster[rast_adm0_admin == 21] <- rast_gdpAdm0


# save raster

terra::writeRaster(finalRaster,paste0('results/rast_adm1_','gdp','_perCapita','_',yearsIn[1],'_',yearsIn[length(yearsIn)],
                                      '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)


### 6. prepare simplified  polygon layer ----

if (file.exists('data_gis/gdp_Adm0Adm1_polyg_simple.gpkg')){
  # load it
  gdp_adm0adm1_polyg_simpl <- st_read('data_gis/gdp_Adm0Adm1_polyg_simple.gpkg')
} else { 
  # create it
  
  p <- as.polygons(r_gdp_adm0adm1_polyg_5arcmin)
  as.data.frame(p)
  
  writeVector(p, 'data_gis/gdp_Adm0Adm1_polyg_simple.gpkg', overwrite=T)
  
  gdp_adm0adm1_polyg_simpl <- st_read('data_gis/gdp_Adm0Adm1_polyg_simple.gpkg') #%>% 

  
}



#### put data to gpkg (and slope to raster) -----
  
source('functions/f_gdp_data2gpkg_adm1.R')

varNames <- c('gdp')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- f_gdp_data2gpkg_adm1(inYears = 1990:2022, 
                                      IndexName = varNames[iVar], 
                                      inDataAdm0 = adm0_comb_interpExtrap, 
                                      inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}


## for bahamas, national data

v_adm0 <- read_sf("results/polyg_adm0_gdp_perCapita_1990_2022.gpkg")
v_adm1 <- read_sf("results/polyg_adm1_gdp_perCapita_1990_2022.gpkg")

v_adm0_sel <- v_adm0 %>% 
  filter(iso3 == "BHS" ) %>% 
  select(-cntry_id)

v_adm1_final <- v_adm1 %>% 
  filter(!iso3 == "BHS") %>% 
  bind_rows(v_adm0_sel) %>% 
  arrange(iso3, GID_nmbr) 



temp <- v_adm1_final %>% 
  st_drop_geometry() 

write_csv(temp, "results/tabulated_gdp_perCapita.csv")
st_write(v_adm1_final,"results/polyg_adm1_gdp_perCapita_1990_2022.gpkg",delete_dsn=T)
