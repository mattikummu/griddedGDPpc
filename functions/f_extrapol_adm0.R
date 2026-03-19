
f_extrapol_adm0 <- function(dataAll, dataFull, dataToBeExtrap, iCntry) {
  # p_adm0_centroids: SpatVector of country centroids (terra), with column GID_0

  target_row <- dataToBeExtrap[iCntry, ]

  swiidTest <- dataFull %>%
    pivot_longer(-iso3, names_to = 'year', values_to = 'value') %>%
    left_join(
      pivot_longer(target_row, -iso3, names_to = 'year', values_to = 'value_target'),
      by = 'year'
    ) %>%
    drop_na()

  # Fit linear models: target ~ each analogue country
  # https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr
  lm_test <- swiidTest %>%
    nest_by(iso3.x) %>%
    mutate(mod = list(lm(value_target ~ value, data = data)))

  # Select best analogue countries, relaxing R² threshold if needed
  lm_all <- lm_test %>%
    reframe(broom::glance(mod)) %>%
    arrange(-r.squared)

  r2_thresholds <- c(0.5, 0.25, 0)
  for (thresh in r2_thresholds) {
    lm_bestModels <- lm_all %>%
      filter(r.squared > thresh) %>%
      slice_head(n = 7)
    if (nrow(lm_bestModels) > 0) break
  }

  # Calculate distance between best-fitting countries and the target country
  v_bestFits <- subset(p_adm0_centroids, p_adm0_centroids$GID_0 %in% lm_bestModels$iso3.x)
  v_target   <- subset(p_adm0_centroids, p_adm0_centroids$GID_0 %in% target_row$iso3)

  if (nrow(v_target) == 0 || nrow(v_bestFits) == 0) {
    # Centroid not found for target or analogues — fall back to best R² country
    best_iso3 <- lm_bestModels$iso3.x[1]
  } else {
    distTemp <- as.vector(terra::distance(v_target, v_bestFits))
    # Select the closest country among the top-fitting analogues
    best_iso3 <- lm_bestModels$iso3.x[which.min(distTemp)]
  }

  testList <- lm_test[lm_test$iso3.x == best_iso3, ]$mod

  df.x <- dataFull %>%
    pivot_longer(-iso3, names_to = 'year', values_to = 'value') %>%
    filter(iso3 == best_iso3)

  modelledData <- predict(testList[[1]], df.x) %>%
    as_tibble()

  filledData <- pivot_longer(target_row, -iso3, names_to = 'year', values_to = 'valueOrg') %>%
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
