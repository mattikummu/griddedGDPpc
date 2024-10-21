
f_gdp_data2raster_adm2 <- function(inYears = 1990:1991, 
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
  
  terra::writeRaster(coll_raster,paste0('results/rast_adm2_gdpPerCapita_',inYears[1],'_',inYears[length(inYears)],'_unHarm.tif'),
                     gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(coll_raster)
}