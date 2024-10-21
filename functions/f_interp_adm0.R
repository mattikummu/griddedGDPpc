f_interp_adm0 <- function(GDPadm0Comb = adm0_comb_long,
                             nameIndicator = 'gdp_pc') {
  temp_indic <- GDPadm0Comb %>% 
    select(iso3, year, !!nameIndicator) %>% 
    rename(value := !!nameIndicator) %>% 
    # interpolate
    group_by(iso3) %>% 
    #https://stackoverflow.com/questions/70155104/interpolate-na-values-when-column-ends-on-na
    mutate(value = na.approx(value, rule = 1, na.rm=F)) %>% 
    ungroup() %>%
    rename(!!nameIndicator := value)
  
}
