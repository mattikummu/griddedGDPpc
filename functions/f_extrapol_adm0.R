
f_extrapol_adm0 <- function(dataAll, dataFull, dataToBeExtrap, iCntry) {
  
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
