
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


#### load  data ----

# read cntry metadata
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


# adm0 and adm1 data

adm0_comb_interpExtrap <- read_csv('results/adm0_gdp_pc_long_interpExtrap.csv') %>% 
  rename(gdp = gdp_pc)
adm1_ratioAdm1Adm0_interp <- read_csv('results/adm1_gdp_ratio_interp_feb2024.csv')


# calculate the number of adm0 and adm1 regions
adm0_count <- adm0_comb_interpExtrap %>% 
  distinct(iso3)

adm1_count_adm0 <- adm1_ratioAdm1Adm0_interp %>% 
  filter(GID_nmbr > 1000) %>% 
  distinct(iso3)

adm1_count_adm1 <- adm1_ratioAdm1Adm0_interp %>% 
  filter(GID_nmbr > 1000) %>% 
  distinct(GID_nmbr)

# check missing data in adm1 data
NAtemp <- adm1_ratioAdm1Adm0_interp %>% 
  drop_na()

adm1Missing <- adm1_ratioAdm1Adm0_interp %>% 
  filter(!GID_nmbr %in% NAtemp$GID_nmbr)

# lifexp missing for some admin areas in TUV

# replace NA ratio with 1; i.e. we use national value for those
adm1_ratioAdm1Adm0_interp[is.na(adm1_ratioAdm1Adm0_interp)] <- 1


# create / load polygon data


if (file.exists('data_gis/gdp_adm0adm1_polyg_feb2024.gpkg')){
  # load it
  gdp_adm0adm1_polyg <- read_sf('data_gis/gdp_adm0adm1_polyg_feb2024.gpkg') 
} else { 
  # create it
  adm1_polyg <- read_sf('results/gisData_GDP_pc_combined_feb2024.gpkg') %>% 
    mutate(iso_code = ifelse(iso3 == 'XKO', 'KSV', iso3)) # kosovo to correct iso3
  
  
  adm1_nogeom <- adm1_polyg %>% 
    st_drop_geometry()
  
  sf_adm0_polyg_diss <- read_sf('results/polyg_gdp_pc_adm0_1990_2022.gpkg')
  
  
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
    #filter(iso3 != 'ESH') %>%  # drop Western Sahara; in GDL part of Morocco
    mutate(GID_nmbr = paste0(iso3,'t')) %>% 
    mutate(continent = 'GADM') %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    mutate(GID_nmbr = cntry_id) %>% 
    rename(cntry_code = cntry_id) %>% 
    mutate(Subnat = 'Only adm0 data') %>% 
    select(Country, GID_nmbr, iso3,GID_nmbr, geom) #%>% 
  #set_names(c('GID_nmbr', 'continent', 'iso_code', 'geometry'))
  
  
  
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
  
  
  # create unique identified for each admin area in gdp_adm0adm1_polyg
  
  # gdp_adm0adm1_polyg <-  gdp_adm0adm1_polyg %>% 
  #   #st_drop_geometry() %>% 
  #   left_join(cntry_info[,c(2,4)]) %>% 
  #   mutate(GID_nmbr = ifelse(iso3 %in% adm1ids_cntry$iso3, 
  #                         cntry_id * 10000 + as.numeric( str_split(GID_nmbr, "r", simplify = TRUE)[ , 2] ),
  #                         cntry_id))
  test_gdp_adm0adm1_polyg <-gdp_adm0adm1_polyg  %>% 
    st_drop_geometry()
  
  write_sf(gdp_adm0adm1_polyg,'data_gis/gdp_adm0adm1_polyg_feb2024.gpkg')
}


# create adm0, adm1 raster -----------------------------------------------------

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




### put data to raster -----------------------------------------------------


myFun_gdp_data2raster <- function(inYears = 1990:2022, IndexName = 'gdp', 
                                  inDataAdm0 = adm0_comb_interpExtrap , 
                                  inDataAdm1 = adm1_ratioAdm1Adm0_interp) {
  
  coll_raster = rast()
  
  ratioName <- paste0(IndexName,'Ratio')
  
  tempDataAdm0 <- inDataAdm0 %>% 
    filter(year %in% inYears) %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    mutate(GID_nmbr = cntry_id) %>% 
    dplyr::select(c(!!IndexName,year, cntry_id, GID_nmbr, iso3))
  
  tempDataAdm1_Ratio <- inDataAdm1 %>% 
    filter(year %in% inYears) %>% 
    select(iso3, year, GID_nmbr, !!ratioName) %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    #mutate(GID_nmbr = cntry_id * 10000 + as.numeric( str_split(GID_nmbr, "r", simplify = TRUE)[ , 2] ) ) %>% 
    left_join(tempDataAdm0[,1:3]) %>% 
    rename(adm0Value = !!IndexName) %>% 
    mutate(!!as.name(IndexName) := !!as.name(ratioName) * adm0Value) %>% 
    dplyr::select(c(!!IndexName,year, 'GID_nmbr', iso3))
  
  tempDataAdm0Adm1 <- tempDataAdm0 %>% 
    filter(!iso3 %in% unique(tempDataAdm1_Ratio$iso3)) %>% 
    select(-cntry_id) %>% 
    bind_rows(tempDataAdm1_Ratio) %>% 
    select(-iso3) %>% 
    drop_na()
  
  # check adm0 areas for which we do not have data, and put those to NA in the raster
  idNoData <- gdp_adm0adm1_polyg %>% 
    st_drop_geometry() %>% 
    select(GID_nmbr) %>% 
    filter(!GID_nmbr %in% unique(tempDataAdm0Adm1$GID_nmbr)) %>% 
    # ## REMOVE WST SAHARA as data exist from subnational ata
    # filter(!GID_nmbr == 912) %>% 
    drop_na()
  
  if(dim(idNoData)[1] == 0) {
    # no nothing
  } else {
    r_gdp_adm0adm1_polyg_5arcmin[r_gdp_adm0adm1_polyg_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  }
  
  for (iYear in inYears) {
    
    tempDataAdm0Adm1_selYear <- tempDataAdm0Adm1 %>% 
      filter(year == iYear)
    
    temp_id <-  as.numeric(tempDataAdm0Adm1_selYear$GID_nmbr)
    temp_v <- as.numeric(tempDataAdm0Adm1_selYear[[IndexName]])
    
    # reclassify
    temp_raster <- classify(r_gdp_adm0adm1_polyg_5arcmin,
                            cbind(temp_id, temp_v))
    
    terra::add(coll_raster) <- temp_raster
  }
  
  names(coll_raster) <- paste0(IndexName,'_',inYears[1]:inYears[length(inYears)])
  
  terra::writeRaster(coll_raster,paste0('results/rast_adm1_',IndexName,'_pc_',inYears[1],'_',inYears[length(inYears)],
                                        '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(coll_raster)
}

varNames <- c('gdp' )

for (iVar in 1:length(varNames)) {
  
  rast_varName <- myFun_gdp_data2raster(inYears = 1990:2022, 
                                        IndexName = varNames[iVar], 
                                        inDataAdm0 = adm0_comb_interpExtrap , 
                                        inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}




### simplify polygon layer ----

if (file.exists('data_gis/gdp_Adm0Adm1_polyg_simple.gpkg')){
  # load it
  gdp_adm0adm1_polyg_simpl <- st_read('data_gis/gdp_Adm0Adm1_polyg_simple.gpkg')
} else { 
  # create it
  
  p <- as.polygons(r_gdp_adm0adm1_polyg_5arcmin)
  as.data.frame(p)
  
  writeVector(p, 'data_gis/gdp_Adm0Adm1_polyg_simple.gpkg', overwrite=T)
  
  gdp_adm0adm1_polyg_simpl <- st_read('data_gis/gdp_Adm0Adm1_polyg_simple.gpkg') #%>% 
  #rename(GID_nmbr = layer)
  
  # gdp_adm0adm1_polyg_simpl <- st_as_sf(raster::raster(r_gdp_adm0adm1_polyg_5arcmin))  %>% 
  # 
  #   sf::st_simplify(., preserveTopology = T, dTolerance = 0.1)
  # 
  # 
  # st_write(gdp_adm0adm1_polyg_simpl, 'data_gis/GDL_regions_v7_simpl.gpkg', delete_dsn=T)
  
}



#### put data to gpkg (and slope to raster) -----




myFun_gdp_data2gpkg <- function(inYears = 1990:2022, IndexName = 'gdp', 
                                inDataAdm0 = adm0_comb_interpExtrap , 
                                inDataAdm1 = adm1_ratioAdm1Adm0_interp) {
  
  ratioName <- paste0(IndexName,'Ratio')
  
  tempDataAdm0 <- inDataAdm0 %>% 
    filter(year %in% inYears) %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    mutate(GID_nmbr = cntry_id) %>% 
    dplyr::select(c(!!IndexName,year, cntry_id, GID_nmbr, iso3))
  
  tempDataAdm1_Ratio <- inDataAdm1 %>% 
    filter(year %in% inYears) %>% 
    select(iso3, year, GID_nmbr, !!ratioName) %>% 
    left_join(cntry_info[,c(2,4)]) %>% 
    #mutate(GID_nmbr = cntry_id * 10000 + as.numeric( str_split(GID_nmbr, "r", simplify = TRUE)[ , 2] ) ) %>% 
    left_join(tempDataAdm0[,1:3]) %>% 
    rename(adm0Value = !!IndexName) %>% 
    mutate(!!as.name(IndexName) := !!as.name(ratioName) * adm0Value) %>% 
    dplyr::select(c(!!IndexName,year, 'GID_nmbr', iso3))
  
  tempDataAdm0Adm1 <- tempDataAdm0 %>% 
    filter(!iso3 %in% unique(tempDataAdm1_Ratio$iso3)) %>% 
    select(-cntry_id) %>% 
    bind_rows(tempDataAdm1_Ratio) %>% 
    select(-iso3) %>% 
    drop_na()
  
  # calculate trend
  
  # https://stackoverflow.com/questions/72922288/group-wise-linear-models-function-nest-by
  
  tempDataAdm0Adm1_trend <- tempDataAdm0Adm1 %>% 
    group_by(GID_nmbr) %>% 
    mutate(time = row_number()) %>% 
    ungroup() %>% 
    select(-year) %>% 
    nest(data = -GID_nmbr) %>% 
    mutate(
      model = map(data,  ~ lm(log10( !!as.name(IndexName) ) ~ time, data = .))
    ) %>% 
    mutate(
      tidy_summary = map(model, tidy)
    ) %>% 
    unnest(tidy_summary) %>% 
    filter(term == 'time') %>% 
    select(GID_nmbr, estimate, p.value)
  
  
  
  tempDataAdm0Adm1_trend_mblm <- tempDataAdm0Adm1 %>% 
    #filter(GID_nmbr == 226)  %>% 
    as_tibble() %>% 
    group_by(GID_nmbr) %>% 
    mutate(time = row_number()) %>% 
    ungroup() %>% 
    select(-year) %>% 
    mutate(log10_gdp_pc = log10(!!as.name(IndexName))) %>% 
    nest(data = -GID_nmbr) %>% 
    mutate(
      model = map(data,  ~ mblm(log10_gdp_pc ~ time, data = .))
    ) %>% 
    mutate(
      tidy_summary = map(model, tidy)
    ) %>% 
    unnest(tidy_summary) %>% 
    filter(term == 'time') %>% 
    select(GID_nmbr, estimate, p.value)
  
  
  
  # # https://stackoverflow.com/questions/32274779/extracting-p-values-from-multiple-linear-regression-lm-inside-of-a-ddply-funct
  # 
  # tempDataAdm0Adm1_trend <- tempDataAdm0Adm1 %>% 
  #   group_by(GID_nmbr) %>% 
  #   #nest() %>% 
  #   do({model = lm(gnic~year, data=.)    # create your model
  #   data.frame(tidy(model),              # get coefficient info
  #              glance(model))})   %>% 
  #   filter(term == 'year')
  
  gdp_adm0adm1_polyg_noGeom <- gdp_adm0adm1_polyg %>%
    st_drop_geometry() %>% 
    select(iso3, GID_nmbr, Country, Subnat)
  
  tempDataAdm0Adm1_wTrend <- tempDataAdm0Adm1 %>% 
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm0Adm1_trend_mblm) %>% 
    mutate(p.value = p.value < 0.05) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(gdp_adm0adm1_polyg_simpl) %>% 
    left_join(gdp_adm0adm1_polyg_noGeom) %>% 
    select(GID_nmbr, iso3,  Country, Subnat, slope, everything()) %>% 
    select(-c(estimate, p.value))%>% 
    mutate(across(paste0('X',inYears[1]):paste0('X',inYears[length(inYears)]), round, 0))
  
  # temp <- tempDataAdm0Adm1_wTrend %>%
  #   dplyr::group_by(GID_nmbr, year) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L) 
  
  st_write(tempDataAdm0Adm1_wTrend,paste0('results/polyg_adm1_',IndexName,'_pc_',inYears[1],'_',inYears[length(inYears)],'.gpkg'), delete_dsn=T)
  
  # tempDataAdm0Adm1_wTrend <- st_read("results/polyg_adm1_gdp_pc_1990_2022.gpkg") %>%
  #   as_tibble() %>%
  #   mutate(across(paste0('X',inYears[1]):paste0('X',inYears[length(inYears)]), round, 0))
  # 
  # only csv
  temp <- tempDataAdm0Adm1_wTrend %>% 
    st_drop_geometry() %>% 
    select(-geom) %>% 
    mutate(across(paste0('X',inYears[1]):paste0('X',inYears[length(inYears)]), round, 0))
  
  
  write_csv(temp, paste0('results/tabulated_adm1_',IndexName,'_pc_',inYears[1],'_',inYears[length(inYears)],'.csv'))
  
  # slope to raster
  
  
  idNoData <- gdp_adm0adm1_polyg %>% 
    st_drop_geometry() %>% 
    select(GID_nmbr) %>% 
    filter(!GID_nmbr %in% unique(tempDataAdm0Adm1$GID_nmbr)) %>% 
    # ## REMOVE WST SAHARA as data exist from subnational ata
    # filter(!GID_nmbr == 912) %>% 
    drop_na()
  
  if(dim(idNoData)[1] == 0) {
    # no nothing
  } else {
    r_gdp_adm0adm1_polyg_5arcmin[r_gdp_adm0adm1_polyg_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  }
  
  temp_id <-  as.numeric(tempDataAdm0Adm1_trend_mblm$GID_nmbr)
  temp_v <- as.numeric(tempDataAdm0Adm1_trend_mblm$estimate)
  
  # reclassify
  slope_raster <- classify(r_gdp_adm0adm1_polyg_5arcmin,
                           cbind(temp_id, temp_v))
  
  names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )
  
  
  
  terra::writeRaster(slope_raster,paste0('results/rast_slope_log10',IndexName,'_',
                                         inYears[1],'_',inYears[length(inYears)],'.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  
}


varNames <- c('gdp')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- myFun_gdp_data2gpkg(inYears = 1990:2022, 
                                      IndexName = varNames[iVar], 
                                      inDataAdm0 = adm0_comb_interpExtrap, 
                                      inDataAdm1 = adm1_ratioAdm1Adm0_interp) 
  
}




