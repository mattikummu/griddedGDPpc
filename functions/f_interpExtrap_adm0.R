
f_interpExtrap_adm0 <- function(nameIndic) {
  
  source('functions/f_interp_adm0.R')
  
  source('functions/f_extrapol_adm0.R')
  
  # interpolate
  indicInterp <-   f_interp_adm0(GDPadm0Comb = adm0_comb_long, 
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
      tempExtrapolCntry <- f_extrapol_adm0(dataAll = indicWider, 
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
    tempExtrapolCntry <- f_extrapol_adm0(dataAll = indicWider, 
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