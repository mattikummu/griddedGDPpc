f_interp_adm1 <- function(variableName = 'gdp') {
  
  adm0_data_gdp <- sf_adm0_gdp_long %>% 
    select(iso3,year,!!as.name(variableName)) %>% 
    rename(indicAdm0 = !!variableName) %>% 
    mutate(year = as.character(year))
  
  # calculate ratio between adm1 and adm0
  adm1_data_gdp_ratio <- sf_subnat_gis_combined_long %>% 
    mutate(year = as.character(year)) %>% 
    select(iso3,GID_nmbr,year,!!variableName) %>% 
    rename(indicAdm1 = !!variableName) %>% 
    left_join(adm0_data_gdp) %>% 
    mutate(ratioAdm1Adm0 := indicAdm1/indicAdm0)
  
  outVarName <- paste0(as.name(variableName),'Ratio')
  
  temp <- adm1_data_gdp_ratio %>% 
    filter(is.na(GID_nmbr))
  
  # interpolate ratio
  adm1_data_SHDI_interpRatio <- adm1_data_gdp_ratio %>% 
    select(iso3, GID_nmbr, year, ratioAdm1Adm0) %>% 
    # make sure that all years are included
    pivot_wider(names_from = 'year', values_from = 'ratioAdm1Adm0') %>% 
    pivot_longer(-c(iso3, GID_nmbr), names_to = 'year', values_to = 'ratioAdm1Adm0') %>% 
    # interpolate
    group_by(GID_nmbr) %>% 
    #https://stackoverflow.com/questions/70155104/interpolate-na-values-when-column-ends-on-na
    mutate(ratioAdm1Adm0 = na.approx(ratioAdm1Adm0, maxgap = Inf, rule = 2)) %>% 
    ungroup() %>% 
    rename(!!as.name(outVarName) := ratioAdm1Adm0) %>% 
    select(iso3, GID_nmbr, year, !!as.name(outVarName))
  
  
  
  return(adm1_data_SHDI_interpRatio)
}