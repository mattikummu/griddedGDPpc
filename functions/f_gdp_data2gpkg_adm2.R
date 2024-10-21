
f_gdp_data2gpkg_adm2 <- function(inYears = 1990:2022, IndexName = 'gdp_pc', 
                                inDataAdm2) {
  
  
  rowNmb_adm2ID <- sf::read_sf('data_gis/adm2_polyg_comb_ID.gpkg' ) %>% 
    st_drop_geometry() %>% 
    select(adm2ID, rowNmb) %>% 
    distinct(adm2ID, .keep_all = T)
  
  
  tempDataAdm2 <- inDataAdm2 %>% 
    select(-c(cntry_id, rowNumb,gdp_adm2, corrRatio)) %>% 
    left_join(rowNmb_adm2ID) %>% 
    rename(!!IndexName := gdp_adm2corr) %>% 
    distinct(adm2ID, year, .keep_all = T)
  
  
  ### apply the harmonisation ratio
  
  tempDataAdm2_harm <- tempDataAdm2  %>% 
    mutate(year = as.numeric(year)) %>% 
    mutate(GID_nmbr = as.numeric(GID_nmbr)) %>% 
    left_join(sf_adm0_comb_adm1_ratio %>% 
                filter(iso3 %in% unique(tempDataAdm2$iso3)) %>% 
                select(GID_nmbr, year, ratio) %>% 
                distinct(GID_nmbr, year, .keep_all = T) %>% 
                mutate(year = as.numeric(year)) %>% 
                mutate(GID_nmbr = as.numeric(GID_nmbr))) %>% 
    mutate(ratio = ifelse(is.na(ratio), 1, ratio)) %>% 
    rename(gdp_org = gdp_pc) %>% 
    mutate(gdp_pc = gdp_org / ratio)
  
  # calculate trend
  
  tempDataAdm2_trend <- tempDataAdm2_harm %>% 
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
  
  
  gdp_adm2_polyg_noGeom <- adm2_polyg_comb %>%
    st_drop_geometry() %>% 
    select(iso3, GID_2, NAME_2)
  
  tempDataadm2_dublicates <- tempDataAdm2_harm %>% 
    select(GID_2, year, as.name(!!IndexName), adm2ID, rowNmb) %>% 
    
    dplyr::group_by(GID_2, adm2ID, rowNmb, year) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L) 
  
  tempDataadm2_wTrend <- tempDataAdm2_harm %>% 
    select(GID_2, year, as.name(!!IndexName), adm2ID, rowNmb) %>% 
    distinct(adm2ID, year, .keep_all = T) %>% 
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm2_trend) %>% 
    mutate(p.value = p.value < 0.05) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(gdp_adm2_polyg_simpl) %>% 
    left_join(gdp_adm2_polyg_noGeom) %>% 
    select(GID_2, adm2ID, iso3, NAME_2, slope, everything()) %>% 
    select(-c(estimate, p.value))
  
  st_write(tempDataadm2_wTrend,
           paste0('results/polyg_adm2_gdp_perCapita_',inYears[1],'_',inYears[length(inYears)],'.gpkg'), 
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
