

f_gdp_data2gpkg_adm1 <- function(inYears = yearsIn, IndexName = 'gdp', 
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
    #select(-iso3) %>% 
    drop_na() %>% 
    distinct(GID_nmbr, year, .keep_all = T)
  
  ### apply the harmonisation ratio
  
  tempDataAdm0Adm1_harm <- tempDataAdm0Adm1 %>% 
    left_join(sf_adm0_comb_adm1_ratio %>% 
                filter(iso3 %in% unique(tempDataAdm1_Ratio$iso3)) %>% 
                select(iso3, year, ratio)) %>% 
    mutate(ratio = ifelse(is.na(ratio), 1, ratio)) %>% 
    rename(gdp_org = gdp) %>% 
    mutate(gdp = gdp_org / ratio)
  
  # 
  
  # calculate trend 
  
  tempDataAdm0Adm1_trend_mblm <- tempDataAdm0Adm1_harm %>% 
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
  
  
  gdp_adm0adm1_polyg_noGeom <- gdp_adm0adm1_polyg %>%
    st_drop_geometry() %>% 
    select(iso3, GID_nmbr, Country, Subnat)
  
  tempDataAdm0Adm1_wTrend <- tempDataAdm0Adm1_harm %>% 
    select(-c(gdp_org, ratio)) %>% 
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm0Adm1_trend_mblm) %>% 
    mutate(p.value = p.value < 0.05) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(gdp_adm0adm1_polyg_simpl) %>% 
    left_join(gdp_adm0adm1_polyg_noGeom) %>% 
    select(GID_nmbr, iso3,  Country, Subnat, slope, everything()) %>% 
    select(-c(estimate, p.value))
  
  # temp <- tempDataAdm0Adm1_wTrend %>%
  #   dplyr::group_by(GID_nmbr, year) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L) 
  
  st_write(tempDataAdm0Adm1_wTrend,paste0('results/polyg_adm1_',IndexName,'_perCapita_',
                                          inYears[1],'_',inYears[length(inYears)],'.gpkg'), delete_dsn=T)
  
  # only csv
  temp <- tempDataAdm0Adm1_wTrend %>% 
    st_drop_geometry() %>% 
    select(-geom)
  
  write_csv(temp, "results/tabulated_gdp_perCapita.csv")
  
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
