
# prepare national data for GDP per cap

# creator: matti.kummu@aalto.fi


library(openxlsx)
library(zoo)
library(broom)
library(mblm)

library(data.table)
library(sf)
library(terra)

library(tidyverse)
library(dplyr) #

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set time steps
timestep <- c(seq(1990, 2022))
step <- c(seq(1,length(timestep)))


##### read data -----

# load cntry_info
cntry_info <- read_csv("data_in/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso_code = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso_code = ifelse(iso_code == 'XKX','KSV',iso_code)) %>%  
  # northern cyprus
  mutate(iso_code = ifelse(iso_code == 'XNC','ZNC',iso_code)) %>%  
  distinct(iso_code, .keep_all = T)


# WB national database
# https://hdr.undp.org/data-center/documentation-and-downloads

# WB regional info ()
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


#### combine data -----

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


if (!dir.exists('results/')) {
  dir.create('results/')
}
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

#### interpolate function -----

adm0_comb_long <- adm0_comb %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'gdp_pc')


myFun_interpAdm0 <- function(GDPadm0Comb = adm0_comb_long,
                             nameIndicator = 'gdp_pc') {
  #nameIndicator = 'gdp_pc'
  temp_indic <- GDPadm0Comb %>% 
    select(iso3, year, !!nameIndicator) %>% 
    rename(value := !!nameIndicator) %>% 
    # remove countries without data
    #filter(!iso3 == temp_indicNoData$iso3) %>% 
    # interpolate
    group_by(iso3) %>% 
    #https://stackoverflow.com/questions/70155104/interpolate-na-values-when-column-ends-on-na
    mutate(value = na.approx(value, rule = 1, na.rm=F)) %>% 
    ungroup() %>%
    rename(!!nameIndicator := value)# %>% 
  # pivot_wider( names_from = 'year', values_from = 'value')
  
}


##### function for extrapolation ----

myFunExtrapol <- function(dataAll, dataFull, dataToBeExtrap, iCntry) {
  
  # swiidNotFull <- dataAll %>% 
  #   filter(!iso3 == unique(dataFull$iso3)) %>% 
  #   set_names(c('iso3', paste0('n',1990:2021))) 
  
  
  swiidTest <- dataFull %>% 
    pivot_longer(-iso3, names_to = 'year', values_to = 'value') %>% 
    left_join(pivot_longer(dataToBeExtrap[iCntry,], -iso3, names_to = 'year', values_to = 'value'), by = 'year') %>% 
    drop_na()
  
  # example of regression in dplyr
  # https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr
  
  lm_test <- swiidTest %>% 
    nest_by(iso3.x) %>%
    mutate(mod = list(lm(value.y ~ value.x, data = data)))
  
  lm_bestModels <- lm_test %>% 
    reframe(broom::glance(mod)) %>% 
    arrange(-r.squared) %>% 
    filter(row_number() < 8) # select 7 best models
  
  # calculate the disance between these best fitting countries and the target country
  
  v_bestFits <- subset(p_adm0_centroids, p_adm0_centroids$GID_0 %in% lm_bestModels$iso3.x)
  v_target <- subset(p_adm0_centroids, p_adm0_centroids$GID_0 %in% dataToBeExtrap[1,]$iso3)
  
  distTemp <- distance(v_target, v_bestFits)
  
  # select the one from the top fit countries with smallest distance
  
  testList <- lm_test[which(lm_test$iso3.x==lm_bestModels[which(distTemp == min(distTemp)),]$iso3.x),]$mod 
  # names(testList) <- "bestModel"
  
  df.x <- dataFull %>% 
    pivot_longer(-iso3, names_to = 'year', values_to = 'value')%>% 
    filter(iso3 == lm_bestModels[which(distTemp == min(distTemp)),]$iso3.x) %>% 
    rename(value.x = value)
  
  modelledData <- predict(testList[[1]], df.x) %>%
    as_tibble()
  
  
  filledData <- pivot_longer(dataToBeExtrap[iCntry,], -iso3, names_to = 'year', values_to = 'valueOrg') %>% 
    bind_cols(modelledData) %>% 
    # calculate ratio of the first and last non_NA value over modelled data
    #mutate(modDataSel = ifelse(is.na(gini_mkt), NA, value)) %>% 
    mutate(ratioFirst = first(na.omit(valueOrg / value))) %>% 
    mutate(ratioLast = last(na.omit(valueOrg / value))) %>% 
    # identify where first and last NAs are located
    mutate(valueTEMP = ifelse(is.na(valueOrg), 0, valueOrg )) %>% 
    mutate(isLeadingNA = cumsum(valueTEMP) == 0,
           isTrailingNA = rev(cumsum(rev(valueTEMP))) ==0 ) %>% 
    # fill NA values with the scaled value of the target data, based on the trend in modelled data
    mutate(valueOrg_filled = valueOrg, 
           valueOrg_filled = case_when(
             isLeadingNA ~ value * ratioFirst,
             isTrailingNA ~ value * ratioLast,
             TRUE ~ valueOrg_filled
           )) %>% 
    select(-c(ratioFirst, ratioLast, valueTEMP, isLeadingNA, isTrailingNA))
  
  return(filledData)
  
  
}


### apply functions ----

# sf_cntryGIS <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer = 'ADM_0') %>% 
#   st_simplify(dTolerance = 0.05, preserveTopology = T)

if (file.exists('data_GIS/p_adm0_centroids.gpkg')) {
  p_adm0_centroids <- vect('data_GIS/p_adm0_centroids.gpkg')
  
} else { # create it
  # gadm_410-levels.gpkg can be downloaded from 
  # https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip
  v_cntryGIS <- terra::simplifyGeom(vect('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer = 'ADM_0'))
  v_cntryGIS_EE <- project(v_cntryGIS, '+proj=eqearth')
  terra::writeVector(v_cntryGIS, 'data_GIS/v_cntryGISsimplif.gpkg')
  terra::writeVector(v_cntryGIS_EE, 'data_GIS/v_cntryGISsimplif_equalEarthProj.gpkg')
  p_adm0_centroids <- terra::centroids(v_cntryGIS_EE)
  terra::writeVector(p_adm0_centroids, 'data_GIS/p_adm0_centroids.gpkg')
}



myFun_interpExtrap_adm0 <- function(nameIndic) {
  
  # define indicator
  #nameIndic = 'gdp_pc'
  
  # interpolate
  indicInterp <-   myFun_interpAdm0(GDPadm0Comb = adm0_comb_long, 
                                    nameIndicator = nameIndic)
  

  
  indicWider <- indicInterp %>% 
    pivot_wider(names_from = 'year', values_from = !!nameIndic) 
  
  
  
  # full timeseries
  dataFull <- indicWider %>% 
    set_names(c('iso3', paste0('n',1990:2022))) %>% 
    drop_na()
  
  # data where no data
  data0 <- indicInterp %>% 
    select(iso3, year, !!nameIndic) %>% 
    rename(value := !!nameIndic) %>% 
    group_by(iso3) %>% 
    summarise(across(where(is.numeric), ~sum(!is.na(.)))) %>% 
    filter(value == 0)
  
  # data where data for less than five years
  dataLess5 <- indicInterp %>% 
    select(iso3, year, !!nameIndic) %>% 
    rename(value := !!nameIndic) %>% 
    group_by(iso3) %>% 
    summarise(across(where(is.numeric), ~sum(!is.na(.)))) %>% 
    filter(!value == 0) %>% 
    filter(value < 5)
  
  # select data that is missing max three years from beginning and end of the time period (1990-2021)
  dataNearlyFull <- indicWider %>% 
    set_names(c('iso3', paste0('n',1990:2022))) %>% 
    filter(!iso3 %in% dataFull$iso3) %>% 
    mutate(nearFullTimeser = ifelse(!is.na(n1993)&!is.na(n2018), 1, 0 )) %>% 
    filter(nearFullTimeser == 1) %>% 
    select(-nearFullTimeser)
  
  # missing between dataLess5 and dataNearlyFull
  dataNotFull <- indicWider %>% 
    filter(!iso3 %in% c(unique(dataFull$iso3), unique(dataLess5$iso3), unique(dataNearlyFull$iso3), unique(data0$iso3))) %>% 
    set_names(c('iso3', paste0('n',1990:2022))) 
  
  
  # for countries with just few entries missing
  
  if (exists('collectFilledData_nearlyFull')) {
    remove('collectFilledData_nearlyFull') # remove if exist
  } else {
    # do nothing
  }
  
  if (nrow(dataNearlyFull) == 0) {
    dataFull_NearlyFull <- dataFull
  } else{
    for (i in 1:nrow(dataNearlyFull)) { # 
      
      # extrapolate a country in question
      tempExtrapolCntry <- myFunExtrapol(dataAll = indicWider, 
                                         dataFull = dataFull, 
                                         dataToBeExtrap = dataNearlyFull, 
                                         iCntry = i)
      
      # collect to new data frame
      if (exists('collectFilledData_nearlyFull')) {
        collectFilledData_nearlyFull <- collectFilledData_nearlyFull %>% 
          bind_rows(tempExtrapolCntry)
      } else {
        collectFilledData_nearlyFull <- tempExtrapolCntry
      }
      
    }
    collectFilledData_nearlyFullWide <- collectFilledData_nearlyFull %>% 
      select(iso3, year, valueOrg_filled) %>% 
      pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')
    
    dataFull_NearlyFull <- dataFull %>% 
      bind_rows(collectFilledData_nearlyFullWide )
  }
  
  
  
  
  # #### for countries with less than 5 observations, we'll use trend from the closest country with full or nearly full data ----
  
  
  if (exists('collectFilledData_less5')) {
    remove('collectFilledData_less5') # remove if exist
  } else {
    # do nothing
  }
  
  
  if (nrow(dataLess5) == 0) {
    collectFilledData_less5Wide <- NULL
  } else {
    for (i in 1:nrow(dataLess5)) {
      
      v_fullData <- subset(p_adm0_centroids, p_adm0_centroids$GID_0 %in% dataFull_NearlyFull$iso3)
      v_target <- subset(p_adm0_centroids, p_adm0_centroids$GID_0 %in% dataLess5[i,]$iso3)
      
      distTemp <- distance(v_target, v_fullData) %>% 
        as_tibble() 
      
      distTempCntry <- t(distTemp) %>% 
        bind_cols(as_tibble(v_fullData$GID_0))
      
      dataClosest <- distTempCntry[which(distTempCntry$...1 == min(distTempCntry$...1)),]
      
      refData <- dataFull_NearlyFull %>% 
        filter(iso3 == dataClosest$value) %>% 
        pivot_longer(-iso3, names_to = 'year', values_to = 'value') %>% 
        select(value)
      
      dataToBeExtrap <- indicWider  %>% 
        set_names(c('iso3', paste0('n',1990:2022))) %>% 
        filter(iso3 == dataLess5[i,]$iso3)
      
      filledData <- pivot_longer(dataToBeExtrap, -iso3, names_to = 'year', values_to = 'valueOrg') %>% 
        bind_cols(refData) %>% 
        # calculate ratio of the first and last non_NA value over modelled data
        #mutate(modDataSel = ifelse(is.na(gini_mkt), NA, value)) %>% 
        mutate(ratioFirst = first(na.omit(valueOrg / value))) %>% 
        mutate(ratioLast = last(na.omit(valueOrg / value))) %>% 
        #mutate(nrow = row_number()) %>% 
        # identify where first and last NAs are located
        mutate(valueTEMP = ifelse(is.na(valueOrg), 0, valueOrg )) %>% 
        mutate(isLeadingNA = cumsum(valueTEMP) == 0,
               isTrailingNA = rev(cumsum(rev(valueTEMP))) ==0 ) %>% 
        
        # mutate(#isTrailingNA_1 <- is.na(lead(valueOrg)) & is.na(valueOrg) & lag(is.na(lead(valueOrg)) & is.na(valueOrg)),
        #        isLeadingNA = is.na(lag(valueOrg, n=1:nrow)) & is.na(valueOrg),
        #        isTrailingNA = is.na(lead(valueOrg)) & is.na(valueOrg) & !isLeadingNA) %>%
        # fill NA values with the scaled value of the target data, based on the trend in modelled data
        mutate(valueOrg_filled = valueOrg, 
               valueOrg_filled = case_when(
                 isLeadingNA ~ value * ratioFirst,
                 isTrailingNA ~ value * ratioLast,
                 TRUE ~ valueOrg_filled
               )) %>% 
        select(-c(ratioFirst, ratioLast, valueTEMP, isLeadingNA, isTrailingNA))
      
      
      
      # collect to new data frame
      if (exists('collectFilledData_less5')) {
        collectFilledData_less5 <- collectFilledData_less5 %>% 
          bind_rows(filledData)
      } else {
        collectFilledData_less5 <- filledData
      }
      
    }
    collectFilledData_less5Wide <- collectFilledData_less5 %>% 
      select(iso3, year, valueOrg_filled) %>% 
      pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')
  }
  
  
  #### then extrapolate the other countries, using the full and extrapolated nearlyFull data ----
  
  if (exists('collectFilledData_rest')) {
    remove('collectFilledData_rest') # remove if exist
  } else {
    # do nothing
  }
  
  
  for (i in 1:nrow(dataNotFull)) { # 
    
    # extrapolate a country in question
    tempExtrapolCntry <- myFunExtrapol(dataAll = indicWider, 
                                       dataFull = dataFull_NearlyFull, 
                                       dataToBeExtrap = dataNotFull, 
                                       iCntry = i)
    
    # collect to new data frame
    if (exists('collectFilledData_rest')) {
      collectFilledData_rest <- collectFilledData_rest %>% 
        bind_rows(tempExtrapolCntry)
    } else {
      collectFilledData_rest <- tempExtrapolCntry
    }
    
  }
  collectFilledData_restWide <- collectFilledData_rest %>% 
    select(iso3, year, valueOrg_filled) %>% 
    pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')
  
  
  
  # put all together
  dataFull_NearlyFull_Rest_Less5 <- dataFull_NearlyFull %>% 
    bind_rows(collectFilledData_restWide) %>% 
    bind_rows(collectFilledData_less5Wide) %>% 
    set_names('iso3', paste0(1990:2022)) %>% 
    pivot_longer(-iso3, names_to = 'year', values_to = nameIndic)
  
  return(dataFull_NearlyFull_Rest_Less5)
  
}

# indicators:  gdp_pc

gdp_pc <- myFun_interpExtrap_adm0(nameIndic = 'gdp_pc')

gdp_pc_wide <- gdp_pc %>% 
  pivot_wider(names_from = 'year', values_from = 'gdp_pc')

fwrite(gdp_pc, 'results/adm0_gdp_pc_long_interpExtrap.csv')
fwrite(gdp_pc_wide, 'results/adm0_gdp_pc_wide_interpExtrap.csv')





#### put data to gpkg (and slope to raster) -----

adm0_comb_interpExtrap <- read.csv( 'results/adm0_gdp_pc_long_interpExtrap.csv')

n_cntry <- adm0_comb_interpExtrap %>% 
  distinct(iso3)

## adm0 polyg

# some of the adm1 levels are divided to those that are officially in a country and those that are 
# on conflict zones (between CHN, IND and PAK)
# let's unite them with those that we have data for



adm0_polyg <- read_sf('/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg', layer ='ADM_0') %>% 
  mutate(GID_0 = ifelse(GID_0 == 'XKO', 'KSV', GID_0)) %>% # kosovo to correct iso3
  rename(iso3 = GID_0) 



# for China, Pakistan, India let's use older version of GADM so that these Z areas
# will be correctly represented. Also Hong Kong and Macao will this way be there as
# individual countries

# source: https://gadm.org/download_world36.html

adm0_gadm_old <-read_sf('/Users/mkummu/R/GIS_data_common/gadm36_levels_gpkg/gadm36_levels.gpkg', layer = 'level0') %>% 
  rename(iso3 = GID_0) %>% 
  rename(COUNTRY = NAME_0) %>% 
  filter(iso3 %in% c('CHN', 'PAK', 'IND', 'HKG', 'MAC'))

sf_adm0_polyg_diss <- adm0_polyg %>% 
  filter(!iso3 %in% c('CHN', 'PAK', 'IND', 'HKG', 'MAC')) %>% 
  bind_rows(adm0_gadm_old)


# join cntry info
adm0_polyg_final <- sf_adm0_polyg_diss %>% 
  #st_drop_geometry() %>% 
  rename(Country = COUNTRY) %>% 
  mutate(iso3 = ifelse(iso3 == 'XKO','KSV',iso3)) %>% 
  filter(iso3 != 'ALA' & iso3 != 'XCA' & iso3 != 'ATA') %>%  # remove Åland (part of Finland); Caspian Sea (not needed), Antarctica
  #filter(iso3 != 'ESH') %>%  # drop Western Sahara; in GDL part of Morocco
  mutate(GID_nmbr = paste0(iso3,'t')) %>% 
  left_join(cntry_info[,c(2,4)] %>%  rename(iso3 = iso_code)) %>% 
  mutate(GID_nmbr = cntry_id) %>% 
  filter(!is.na(GID_nmbr)) %>% 
  select(Country, GID_nmbr, iso3,GID_nmbr, geom)


temp <- adm0_polyg_final %>% 
  st_drop_geometry()

# create adm0  raster -----------------------------------------------------

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


### put data to raster -----------------------------------------------------


myFun_gdp_data2raster <- function(inYears = 1990:2022, IndexName = 'gdp_pc', 
                                  inDataAdm0 = adm0_comb_interpExtrap) {
  
  coll_raster = rast()
  
  ratioName <- paste0(IndexName,'Ratio')
  
  tempDataAdm0 <- inDataAdm0 %>% 
    filter(year %in% inYears) %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntry_info[,c(2,4)] %>%  rename(iso3 = iso_code)) %>% 
    mutate(GID_nmbr = cntry_id) %>% 
    dplyr::select(c(!!IndexName,year, cntry_id, GID_nmbr, iso3)) %>% 
    drop_na()
  
  
  
  # check adm0 areas for which we do not have data, and put those to NA in the raster
  idNoData <- adm0_polyg_final %>% 
    st_drop_geometry() %>% 
    select(GID_nmbr) %>% 
    filter(!GID_nmbr %in% unique(tempDataAdm0$GID_nmbr)) %>% 
    # ## REMOVE WST SAHARA as data exist from subnational ata
    # filter(!GID_nmbr == 912) %>% 
    drop_na()
  
  r_gdp_adm0_polyg_5arcmin[r_gdp_adm0_polyg_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  for (iYear in inYears) {
    
    tempDataAdm0_selYear <- tempDataAdm0 %>% 
      filter(year == iYear)
    
    temp_id <-  as.numeric(tempDataAdm0_selYear$GID_nmbr)
    temp_v <- as.numeric(tempDataAdm0_selYear[[IndexName]])
    
    # reclassify
    temp_raster <- classify(r_gdp_adm0_polyg_5arcmin,
                            cbind(temp_id, temp_v))
    
    terra::add(coll_raster) <- temp_raster
  }
  
  names(coll_raster) <- paste0(IndexName,'_',inYears[1]:inYears[length(inYears)])
  
  terra::writeRaster(coll_raster,paste0('results/rast_adm0_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],
                                        '.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(coll_raster)
}

varNames <- c('gdp_pc' )

for (iVar in 1:length(varNames)) {
  
  rast_varName <- myFun_gdp_data2raster(inYears = 1990:2022, 
                                        IndexName = varNames[iVar], 
                                        inDataAdm0 = adm0_comb_interpExtrap) 
  
}




### simplify polygon layer ----

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



#### put data to gpkg (and slope to raster) -----





myFun_gdp_data2gpkg <- function(inYears = 1990:2022, IndexName = 'gdp_pc', 
                                inDataAdm0 = adm0_comb_interpExtrap) {
  
  
  tempDataAdm0 <- inDataAdm0 %>% 
    filter(year %in% inYears) %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntry_info[,c(2,4)] %>%  rename(iso3 = iso_code)) %>% 
    mutate(GID_nmbr = cntry_id) %>% 
    dplyr::select(c(!!IndexName,year, cntry_id, GID_nmbr, iso3)) %>%
    pivot_wider(names_from = year, values_from = !!IndexName) %>% 
    pivot_longer(-c(cntry_id,GID_nmbr, iso3), names_to = 'year', values_to = 'gdp_pc') 
    # drop_na()
  
  
  # calculate trend
  
  # https://stackoverflow.com/questions/72922288/group-wise-linear-models-function-nest-by
  
  tempDataAdm0_trend <- tempDataAdm0 %>% 
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
  
  
  
  #####
  
  tempDataAdm0_trend_mblm <- tempDataAdm0 %>% 
    #filter(GID_nmbr == 226)  %>% 
    as_tibble() %>% 
    group_by(GID_nmbr) %>% 
    mutate(time = row_number()) %>% 
    ungroup() %>% 
    select(-year) %>% 
    mutate(log10_gdp_pc = log10(gdp_pc)) %>% 
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
  
  
  ####

  
  
  gdp_adm0_polyg_noGeom <- adm0_polyg_final %>%
    st_drop_geometry() %>% 
    select(iso3, GID_nmbr, Country)
  
  tempDataAdm0_wTrend <- tempDataAdm0 %>% 
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm0_trend_mblm) %>% 
    mutate(p.value = p.value < 0.05) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(gdp_adm0_polyg_simpl) %>% 
    left_join(gdp_adm0_polyg_noGeom) %>% 
    select(GID_nmbr, iso3,  Country, slope, everything()) %>% 
    select(-c(estimate, p.value)) %>% 
    mutate(across(paste0('X',inYears[1]):paste0('X',inYears[length(inYears)]), round, 0))
  
  
  
  st_write(tempDataAdm0_wTrend,paste0('results/polyg_adm0_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.gpkg'), delete_dsn=T)
  
  # tempDataAdm0_wTrend <- st_read("results/polyg_adm0_gdp_pc_1990_2022.gpkg") %>%
  #   as_tibble()  %>% 
  #   mutate(across(paste0('X',inYears[1]):paste0('X',inYears[length(inYears)]), round, 0))
  
  
  # only csv
  temp <- tempDataAdm0_wTrend %>% 
    st_drop_geometry() %>% 
    select(-geom) %>% 
    mutate(across(paste0('X',inYears[1]):paste0('X',inYears[length(inYears)]), round, 0))
    
  
  write_csv(temp, paste0('results/tabulated_adm0_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.csv'))
  
  # slope to raster
  
  
  idNoData <- adm0_polyg_final %>% 
    st_drop_geometry() %>% 
    select(GID_nmbr) %>% 
    filter(!GID_nmbr %in% unique(tempDataAdm0$GID_nmbr)) %>% 
    # ## REMOVE WST SAHARA as data exist from subnational ata
    # filter(!GID_nmbr == 912) %>% 
    drop_na()
  
  r_gdp_adm0_polyg_5arcmin[r_gdp_adm0_polyg_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  temp_id <-  as.numeric(tempDataAdm0_trend_mblm$GID_nmbr)
  temp_v <- as.numeric(tempDataAdm0_trend_mblm$estimate)
  
  # reclassify
  slope_raster <- classify(r_gdp_adm0_polyg_5arcmin,
                           cbind(temp_id, temp_v))
  
  names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )
  
  
  
  terra::writeRaster(slope_raster,paste0('results/rast_slope_log10',IndexName,'_adm0_',
                                         inYears[1],'_',inYears[length(inYears)],'.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  
}


varNames <- c('gdp_pc')

for (iVar in 1:length(varNames)) {
  
  vect_varName <- myFun_gdp_data2gpkg(inYears = 1990:2022, 
                                      IndexName = varNames[iVar], 
                                      inDataAdm0 = adm0_comb_interpExtrap) 
  
}





