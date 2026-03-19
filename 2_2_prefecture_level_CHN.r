### 2_2_prefecture_level_CHN.r
# Process China prefecture-level GDP data, match to GADM admin-2 units,
# and produce a long-format GDP table for use in the admin-2 downscaling pipeline.
# Subnational GDP per capita dataset — Matti Kummu, Aalto University (matti.kummu@aalto.fi)

library(sf)
library(terra)
library(Rfast)
library(data.table)
library(zoo)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr)


## read in data


prefect_CHN <- read_excel('data_in/data_update_nov2025/A new dataset of province- and prefecture-level human development index in China_v2_20250604.xlsx',
                          sheet = 'prefecture-level HDI', skip = 0) %>%
  select(province_English, province_Chinese, prefecture_English, prefecture_Chinese, year, code, gnipc) %>%
  rename(NAME_2 = prefecture_English,
         NL_NAME_2 = prefecture_Chinese,
         NAME_1 = province_English,
         NL_NAME_1 = province_Chinese)

prefect_CHN_distinct <- prefect_CHN %>%
  distinct(NAME_1, NAME_2, .keep_all = T) %>%
  arrange(NAME_1, NAME_2) %>%
  mutate(NAME_1 = ifelse(NAME_1 == 'Tibet', 'Xizang', NAME_1)) %>%
  mutate(NAME_2 = case_when(
    NAME_2 == 'Xiangyang' ~ 'Xiangfan',
    NAME_2 == 'Xiangxi' ~ 'Xiangxi Tujia and Miao',
    NAME_2 == 'Nagqu' ~ 'Nagchu',
    NAME_2 == 'Nyingchi' ~ 'Nyingtri',
    NAME_2 == 'Qamdo' ~ 'Chamdo',
    NAME_2 == 'Xigazê' ~ 'Shigatse',
    NAME_2 == 'Bayingolin' ~ "Bayin'gholin Mongol",
    NAME_2 == 'Turpan' ~ 'Turfan',
    TRUE ~ NAME_2 # This is the "else" condition, returning the original value
  ))


# GADM 4.1 boundaries – download: https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip
adm1_CHN <- read_sf( '/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg' ,  layer = 'ADM_1') %>%
  filter(GID_0 == 'CHN')



adm2_CHN <- read_sf( '/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg' ,  layer = 'ADM_2') %>%
  st_drop_geometry() %>%
  filter(GID_0 == 'CHN') %>%
  select(GID_0, GID_1, NAME_1,NL_NAME_1, GID_2, NAME_2, NL_NAME_2) %>%
  arrange(NAME_1, NAME_2)

## with only one prefecture

adm2_CHN_only1pref <- adm2_CHN %>%
  # 1. Group the data by Province (adm1)
  group_by(NAME_1) %>%

  # 2. Filter the groups where the number of unique Prefectures (adm2) is 1
  filter(n_distinct(NAME_2) == 1) %>%

  # 3. Ungroup the data (optional, but good practice)
  ungroup()



# left join with Chinese name

adm2_CHN_pref_comb_NL <- adm2_CHN %>%
  left_join(prefect_CHN_distinct, by = c('NL_NAME_2'))

adm2_CHN_pref_comb_NL_notNA <- adm2_CHN_pref_comb_NL %>%
  filter(!is.na(code))

prefect_CHN_distinct_left_from_NA <- prefect_CHN_distinct %>%
  filter(!NL_NAME_2 %in% adm2_CHN_pref_comb_NL_notNA$NL_NAME_2) %>%
  arrange(NAME_1, NAME_2)

adm2_CHN_pref_comb_NL_NA <- adm2_CHN_pref_comb_NL %>%
  filter(is.na(code))%>%
  arrange(NAME_1.x, NAME_2.x)

# left join with English name

adm2_CHN_pref_comb_NL_NA_comb_EN <- adm2_CHN %>%
  filter(GID_2 %in% adm2_CHN_pref_comb_NL_NA$GID_2) %>%
  left_join(prefect_CHN_distinct, by = c('NAME_2'))

adm2_CHN_pref_comb_NL_NA_comb_EN_notNA <- adm2_CHN_pref_comb_NL_NA_comb_EN %>%
  filter(!is.na(code))


# left for which we do not have yet corresponding in GADM

adm2_CHN_pref_comb_NL_NA_comb_EN_from_NA <- prefect_CHN_distinct %>%
  filter(!NL_NAME_2 %in% adm2_CHN_pref_comb_NL_notNA$NL_NAME_2) %>%
  filter(!NAME_2 %in% adm2_CHN_pref_comb_NL_NA_comb_EN_notNA$NAME_2) %>%
  arrange(NAME_1, NAME_2)

# left from adm 2

adm2_CHN_pref_comb_NL_NA_EN_NA <- adm2_CHN_pref_comb_NL_NA_comb_EN %>%
  filter(is.na(code))%>%
  arrange(NAME_1.x, NAME_2) |>
  select(GID_0, GID_1, GID_2, NAME_1 = NAME_1.x, NL_NAME_1 = NL_NAME_1.x, NAME_2, NL_NAME_2 = NL_NAME_2.x, code) |>
  filter(!NAME_1 == 'Hong Kong') %>%
  filter(!NAME_1 == 'Macau') |>
  filter(!GID_2 %in% adm2_CHN_only1pref$GID_2)

# final combine all together

adm2_CHN_pref_comb <- adm2_CHN_pref_comb_NL_notNA |>
  bind_rows(adm2_CHN_pref_comb_NL_NA_comb_EN_notNA) |>
  select(GID_0, GID_1, GID_2, NAME_1 = NAME_1.x, NL_NAME_1 = NL_NAME_1.x, NAME_2 = NAME_2.x, NL_NAME_2, code) |>
  bind_rows(adm2_CHN_pref_comb_NL_NA_EN_NA)

### add hong kong??



##### ccalculate adm0 gdp based on the adm1 level data-----

prefect_CHN_wide <- prefect_CHN %>%
  pivot_wider(names_from = 'year', values_from = 'gnipc') %>%
  left_join(adm2_CHN_pref_comb %>% select(code, GID_1, GID_2))

prefect_CHN_long <- prefect_CHN_wide %>%
  pivot_longer(cols = -c(NAME_1, NL_NAME_1, NAME_2, NL_NAME_2, code, GID_2, GID_1), values_to = 'gdp', names_to = 'year') %>%
  arrange(GID_2)


## check missing GID_1 codes

GID1_missing <- prefect_CHN_long |>
  filter(is.na(GID_1)) |>
  distinct(NAME_1, .keep_all = T)

GID_code_for_missing <- prefect_CHN_long |>
  filter(!is.na(GID_1)) |>
  filter(NAME_1 %in% GID1_missing$NAME_1) |>
  distinct(GID_1, NAME_1)

prefect_CHN_long <- prefect_CHN_long |>
  left_join(GID_code_for_missing, by = 'NAME_1', suffix = c('', '_corrected')) |>
  mutate(GID_1 = ifelse(is.na(GID_1), GID_1_corrected, GID_1)) |>
  select(-GID_1_corrected)

test <- prefect_CHN_long |>
  filter(is.na(GID_1)) |>
  distinct(NAME_1, .keep_all = T)



ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)



if (file.exists('data_gis/r_pop_GHS_1989_2024_5arcmin.tif')){
  # load it
  r_pop <- rast('data_gis/r_pop_GHS_1989_2024_5arcmin.tif')
} else { # create it

  # GHS-POP population rasters – download: https://ghsl.jrc.ec.europa.eu/download.php?ds=pop
  r_popCount_GP <- rast("/Users/mkummu/R/GIS_data_common/GHS_POP/r_pop_GHS_1985_2025_5arcmin.tif")

  r_pop <- subset(r_popCount_GP,5:40)

  writeRaster(r_pop,'data_gis/r_pop_GHS_1989_2024_5arcmin.tif', overwrite=TRUE)



}

## adm 1 level data

# read in adm1 level gdp data from "data_in/gadm_level1_gdp_pc.xlsx"
yearsIn <- 1990:2024
v_adm1 <- vect(paste0('results/polyg_adm1_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

t_GID_nmbr_GID_1 <- vect("results/gisData_GDP_pc_combined_feb2024.gpkg") |>
  as_tibble() |>
  filter(iso3 == 'CHN') |>
  distinct(GID_nmbr, GID_1)

t_adm1 <- as_tibble(v_adm1) |>
  filter(iso3 == 'CHN') %>%
  select(iso3, GID_nmbr, Subnat, paste0(1990:2024) ) |>
  pivot_longer(cols = -c(iso3, GID_nmbr, Subnat), names_to = 'year', values_to = 'adm1_gdp_pc') %>%
  mutate(year = as.character(year)) |>
  left_join(t_GID_nmbr_GID_1, by = 'GID_nmbr')



### updated CHN prefecture data
# Yingkou GID missing

prefect_CHN_update <- read_excel('data_in/data_update_nov2025/China prefecture level GDP per capita 2010-2023.xlsx',
                          sheet = 'Sheet1', skip = 2) %>%
  rename(NAME_2 = prefecture_English,
         NL_NAME_2 = prefecture_Chinese,
         NAME_1 = province_English,
         NL_NAME_1 = province_Chinese) |>
  ## add code
  left_join(prefect_CHN %>%
    select(NAME_1, NAME_2, code) |>
    distinct(), by = c('NAME_1', 'NAME_2')) |>
  ## add GID_1 and GID_2
  left_join(adm2_CHN_pref_comb %>% select(code, GID_1, GID_2)) |>
  select(-c(NL_NAME_1, NL_NAME_2))

prefect_CHN_update_long <- prefect_CHN_update %>%
  pivot_longer(cols = -c(code, GID_1, GID_2, NAME_1, NAME_2), names_to = 'year', values_to = 'gdppc')

# prefect_CHN_update_names <- prefect_CHN_update |>
#   select(NAME_1, NL_NAME_1, NAME_2, NL_NAME_2) |>

# combine with reported adm1 data

prefect_CHN_update_long_comb_adm1 <- prefect_CHN_update_long |>
  left_join(t_adm1, by = c('GID_1', 'year'))

##### interpolate and extrapolate missing values ----

# interpolate within 2010-2023 (do not extrapolate)
prefect_CHN_update_long_comb_adm1 |>
  group_by(GID_2) %>%
  mutate(gdppc_interp = na.approx(gdppc, x = as.numeric(year), maxgap = Inf, rule =1, na.rm = FALSE)) %>%
  ungroup() -> prefect_CHN_update_long_comb_adm1_interp

#view(prefect_CHN_update_long_comb_adm1_interp)


# fill leading/trailing NAs by scaling the nearest observed gdppc to adm1_gdp_pc changes
prefect_CHN_update_long_comb_adm1_interp_extrap <- prefect_CHN_update_long_comb_adm1_interp %>%
  mutate(year_num = as.integer(year)) %>%
  mutate(gdppc_interp_org = gdppc_interp) %>%
  group_by(GID_2) %>%
  group_modify(~{
    df <- .x
    idx <- which(!is.na(df$gdppc_interp))
    if(length(idx) == 0) return(df)            # nothing to base on
    first_i <- idx[1]; last_i <- idx[length(idx)]
    val_first <- df$gdppc_interp[first_i]; adm1_first <- df$adm1_gdp_pc[first_i]
    val_last  <- df$gdppc_interp[last_i];  adm1_last  <- df$adm1_gdp_pc[last_i]

    # fill years after last observed value
    if(!is.na(val_last) && !is.na(adm1_last)){
      after_idx <- which(is.na(df$gdppc_interp) & df$year_num > df$year_num[last_i])
      if(length(after_idx) > 0){
        df$gdppc_interp[after_idx] <- val_last * (df$adm1_gdp_pc[after_idx] / adm1_last)
      }
    }

    # fill years before first observed value
    if(!is.na(val_first) && !is.na(adm1_first)){
      before_idx <- which(is.na(df$gdppc_interp) & df$year_num < df$year_num[first_i])
      if(length(before_idx) > 0){
        df$gdppc_interp[before_idx] <- val_first * (df$adm1_gdp_pc[before_idx] / adm1_first)
      }
    }

    df
  }) %>%
  ungroup() %>%
  select(-year_num) |>
  select(-gdppc_interp_org)

# view(prefect_CHN_update_long_comb_adm1_interp_extrap)





#### scale with reported adm1 data ------

# population for each subnat

timeSteps <- 2010:2023

adm2_CHN_geo <- read_sf( '/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg' ,  layer = 'ADM_2') %>%
  filter(GID_0 == 'CHN') %>%
  select(GID_0, GID_1, NAME_1,NL_NAME_1, GID_2, NAME_2, NL_NAME_2) %>%
  arrange(NAME_1, NAME_2)

v_subnatPop <- exactextractr::exact_extract(subset(r_pop,22:35), adm2_CHN_geo, fun = 'sum')

v_subnatPop_comb <- adm2_CHN_geo %>%
  st_drop_geometry() %>%
  select(GID_2) %>%
  bind_cols(v_subnatPop) %>%
  #select(-ID) %>%
  set_names(c('GID_2', paste0(timeSteps))) %>%
  pivot_longer(names_to = 'year', values_to = 'pop', -'GID_2')
  # # mutate(pop2021 = pop2020) %>%
  # mutate(pop1989 = pop1990) %>%
  # mutate(pop2022 = pop2020) %>%
  #select(GID_nmbr, cntry_id, paste0('pop',1989:2024))

# gdp per capita

prefect_CHN_long_combPop <- prefect_CHN_update_long_comb_adm1_interp_extrap |> #prefect_CHN_long %>%
  rename(gdp = gdppc_interp) |>
  left_join(v_subnatPop_comb) %>%
  mutate(gdp_x_pop = gdp * pop) %>%
  group_by(GID_1, year) %>%
  mutate(adm1_pop = sum(pop, na.rm = TRUE)) %>%
  mutate(adm1_gdp_x_pop = sum(gdp_x_pop, na.rm = TRUE)) %>%
  mutate(adm1_gdp_pc = adm1_gdp_x_pop / adm1_pop) %>%
  ungroup() %>%
  mutate(adm2_gdp_ratio = gdp / adm1_gdp_pc) %>%
  select(NAME_1, NAME_2, GID_1, GID_2, year, adm2_gdp_ratio) |>
  ## put ratio as 1, when NA
  mutate(adm2_gdp_ratio = ifelse(is.na(adm2_gdp_ratio), 1, adm2_gdp_ratio))

# --- Ensure full year coverage 1990:2024 for every admin (GID_2).
# Existing values are preserved; years with no data remain NA.
years_full <- as.character(1990:2024)

# meta of admins (use prefect_CHN_long to include admins even if some years missing)
meta_admin <- prefect_CHN_long %>%
  distinct(NAME_1, NAME_2, GID_1, GID_2)

# full grid admin x year
full_grid <- tidyr::crossing(meta_admin, year = years_full)


# merge to get all years
prefect_CHN_long_combPop_allYrs <- full_grid %>%
  left_join(prefect_CHN_long_combPop
    |> mutate(year = as.character(year)) |>
     select(GID_1, GID_2, year, adm2_gdp_ratio), by = c("GID_1", "GID_2", "year")) %>%
  arrange(GID_2, year)


print(prefect_CHN_long_combPop_allYrs |>
  filter(is.na(GID_2)), n = 36)

# interpolate ratio
adm1_data_GDP_interpRatio <- prefect_CHN_long_combPop_allYrs %>%
  # pivot_longer(-c(iso3, GID_nmbr), names_to = 'year', values_to = 'ratioAdm1Adm0') %>%
  # interpolate
  filter(!is.na(GID_2)) %>%
  group_by(GID_2) %>%
  #https://stackoverflow.com/questions/70155104/interpolate-na-values-when-column-ends-on-na
  mutate(adm2_gdp_ratio = na.approx(adm2_gdp_ratio, maxgap = Inf, rule = 2)) %>%
  ungroup()
  #rename(!!as.name(outVarName) := ratioAdm1Adm0) %>%
  #select(iso3, GID_nmbr, year, !!as.name(outVarName))



### calculate the GDP using adm1 level data and the ratio



# combine adm1 gdp pc and adm2 ratio

ad2_comb_adm1_data <- adm1_data_GDP_interpRatio |>
  left_join(t_adm1, by = c('GID_1', 'year')) |>
  mutate(adm2_gdp_pc = adm1_gdp_pc * adm2_gdp_ratio)

test <- ad2_comb_adm1_data |>
  filter(NAME_1 %in% GID_code_for_missing$NAME_1)


# write to file
fwrite(ad2_comb_adm1_data, 'results/adm2_CHN_gdp_per_capita_1990_2024.csv')


# plot histogram of ratio
library(ggplot2)
ggplot(data = ad2_comb_adm1_data, aes(x = adm2_gdp_ratio))  +
geom_histogram(binwidth = 0.1) +
  labs(title = 'Histogram of ADM2 GDP per capita ratio', x = 'ADM2 GDP per capita ratio', y = 'Count') +
  theme_minimal()


## histogram of the ratios by GID_1
library(ggplot2)
ggplot(data = ad2_comb_adm1_data, aes(x = adm2_gdp_ratio)) +
  geom_histogram(binwidth = 0.1) +
  # set xlim 0.4 to 1.5
  xlim(0.4, 1.5) +
  facet_wrap(~GID_1, scales = 'free_y') +
  labs(title = 'Histogram of ADM2 GDP per capita by ADM1', x = 'ADM2 GDP per capita', y = 'Count') +
  theme_minimal()
