
f_gdp_data2raster_adm1 <- function(inYears = 1990:2022, IndexName = 'gdp', 
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
  
  
  return(coll_raster)
}