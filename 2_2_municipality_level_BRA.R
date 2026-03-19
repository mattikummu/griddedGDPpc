#!/usr/bin/env Rscript
## Brazil municipality → GADM adm2 name matching (mapping only)
## Simplified: read input files, extract unique municipality names (no years),
## clean names, match to GADM ADM2 using exact and constrained fuzzy matching,
## and write a CSV + RDS of mappings for manual review.

library(readxl)
library(dplyr)
library(stringi)
library(stringr)
library(stringdist)
library(sf)

# Input files (adjust paths if needed)
file_2002_2009 <- 'data_in/data_update_nov2025/PIB dos Munic¡pios - base de dados 2002-2009.xls'
file_2010_2021 <- 'data_in/data_update_nov2025/PIB dos Munic¡pios - base de dados 2010-2021.xlsx'

# Read adm2 reference
adm2_path <- '/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg'
adm2_BRA <- read_sf(adm2_path, layer = 'ADM_2') %>%
  filter(GID_0 == 'BRA') %>%
  st_drop_geometry() %>%
  select(GID_0, GID_1, GID_2, NAME_1, NAME_2)

# Helper to read a GDP file and return a dataframe with columns: muni_orig, adm1_name (if present), muni_code (if present)
read_muni_only <- function(path){
  df <- readxl::read_excel(path)
  nm <- names(df)
  nm_clean <- stringi::stri_trans_general(nm, 'Latin-ASCII') |> tolower()

  # detect Portuguese long-format columns
  muni_col <- nm[which(str_detect(nm_clean, 'nome do municip') | str_detect(nm_clean, '^nome do municipio'))]
  if(length(muni_col) == 0) muni_col <- nm[which(str_detect(nm_clean, 'nome do mun'))]
  if(length(muni_col) == 0) muni_col <- nm[which(str_detect(nm_clean, 'nome'))][1]

  adm1_col <- nm[which(str_detect(nm_clean, 'nome da unidade da federa'))]
  if(length(adm1_col) == 0) adm1_col <- nm[which(str_detect(nm_clean, 'unidade da federa'))]

  muni_code_col <- nm[which(str_detect(nm_clean, 'codigo do municip'))]

  if(length(muni_col) == 0) stop(paste('Could not detect municipality column in', path))

  out <- df %>%
    rename(muni_orig = all_of(muni_col)) %>%
    select(any_of(c('muni_orig', adm1_col, muni_code_col)))

  if(!is.null(adm1_col) && length(adm1_col)>0) out <- out %>% rename(adm1_name = all_of(adm1_col))
  if(!is.null(muni_code_col) && length(muni_code_col)>0) out <- out %>% rename(muni_code = all_of(muni_code_col))

  out
}

# Read both files (safe if one is missing)
gdp_files <- list(file_2002_2009, file_2010_2021)
df_list <- list()
for(p in gdp_files){
  if(file.exists(p)){
    df_list[[p]] <- tryCatch(read_muni_only(p), error = function(e){
      message('Warning reading ', p, ': ', e$message)
      NULL
    })
  } else {
    message('File not found: ', p)
  }
}

gdp_muni <- bind_rows(df_list) %>%
  mutate(
    muni_orig = as.character(muni_orig),
    adm1_name = ifelse(exists('adm1_name') & !is.na(adm1_name), as.character(adm1_name), NA_character_),
    muni_code = ifelse(exists('muni_code') & !is.na(muni_code), as.character(muni_code), NA_character_)
  ) %>%
  distinct(muni_orig, adm1_name, muni_code)

# Clean municipality and adm1 names for matching
clean_name <- function(x){
  x2 <- stringi::stri_trans_general(x, 'Latin-ASCII')
  x2 <- tolower(as.character(x2))
  x2 <- str_replace_all(x2, "[[:punct:]]", ' ')
  x2 <- str_replace_all(x2, regex('(^municipio de |^munic de |^munic\\. de |^cidade de | municipio | cidade )', ignore_case = TRUE), ' ')
  x2 <- str_squish(x2)
  x2
}

gdp_muni <- gdp_muni %>%
  mutate(
    muni = clean_name(muni_orig),
    adm1_name_clean = ifelse(!is.na(adm1_name), clean_name(adm1_name), NA_character_)
  )

# Prepare adm2 reference cleaned
adm2_ref <- adm2_BRA %>%
  mutate(
    NAME_2_clean = clean_name(NAME_2),
    NAME_1_clean = clean_name(NAME_1)
  ) %>%
  arrange(GID_2) %>%
  distinct(NAME_1_clean, NAME_2_clean, .keep_all = TRUE)

# Initialize mapping table
muni_unique <- gdp_muni %>%
  mutate(GID_2 = NA_character_, match_method = NA_character_, match_dist = NA_real_)

# 1) exact match by adm1 + muni
# pull GID_2 from reference with a distinct name to avoid suffixing issues
muni_unique <- muni_unique %>%
  left_join(adm2_ref %>% select(GID_2_exact_adm1 = GID_2, NAME_1_clean, NAME_2_clean),
            by = c('muni' = 'NAME_2_clean', 'adm1_name_clean' = 'NAME_1_clean')) %>%
  mutate(
    GID_2 = coalesce(GID_2, GID_2_exact_adm1),
    match_method = if_else(!is.na(GID_2_exact_adm1) & is.na(match_method), 'exact_adm1_adm2', match_method)
  ) %>%
  select(-GID_2_exact_adm1)


## Sanity check: which remain unmatched after step 1

# -- Admin-area comparison diagnostics --
# Ensure we've attached ADM2 metadata (NAME_1) to the mapping table
if(!'NAME_1' %in% names(muni_unique)){
  muni_unique <- muni_unique %>% left_join(adm2_ref %>% select(GID_2, NAME_1, NAME_2), by = 'GID_2')
}

# Create cleaned adm1 from ADM2 for fair comparison
muni_unique <- muni_unique %>%
  mutate(NAME_1_clean_from_adm2 = ifelse(!is.na(NAME_1), clean_name(NAME_1), NA_character_))

# Compare adm1 coming from the GDP file (adm1_name_clean) with adm1 inferred from matched ADM2
compare_admin <- muni_unique %>%
  mutate(
    adm1_from_gdp = adm1_name_clean,
    adm1_from_adm2 = NAME_1_clean_from_adm2,
    adm1_match = case_when(
      is.na(adm1_from_gdp) & is.na(adm1_from_adm2) ~ TRUE,
      is.na(adm1_from_gdp) | is.na(adm1_from_adm2) ~ FALSE,
      TRUE ~ adm1_from_gdp == adm1_from_adm2
    )
  )

# Summarize comparison
admin_comp_counts <- compare_admin %>%
  summarize(
    total = n(),
    matched_adm1 = sum(adm1_match, na.rm = TRUE),
    unmatched_adm1 = sum(!adm1_match, na.rm = TRUE),
    missing_gdp_adm1 = sum(is.na(adm1_from_gdp)),
    missing_adm2_adm1 = sum(is.na(adm1_from_adm2))
  )




##### ccalculate adm0 gdp based on the adm1 level data-----

## can you read in municipality level gdp per capita data directly


# ## read in data

# ...existing code...
# ## read in data

read_gdp_file <- function(path){
  df <- readxl::read_excel(path)
  nm <- names(df)
  nm_clean <- stringi::stri_trans_general(nm, "Latin-ASCII") |> tolower()

  # detect the expected Portuguese columns (simple substring matches)
  year_col       <- nm[which(nm_clean == "ano" | nm_clean == "year")][1]
  adm1_col       <- nm[which(str_detect(nm_clean, "nome da unidade da federac") |
                             str_detect(nm_clean, "nome da unidade da federacao") |
                             str_detect(nm_clean, "nome da unidade"))][1]
  adm1_code_col  <- nm[which(str_detect(nm_clean, "codigo da unidade") |
                             str_detect(nm_clean, "codigo da unidade da federac") |
                             str_detect(nm_clean, "codigo da unidade da federacao"))][1]
  muni_col       <- nm[which(str_detect(nm_clean, "nome do municip") |
                             str_detect(nm_clean, "nome do municipio") |
                             str_detect(nm_clean, "nome do mun"))][1]
  muni_code_col  <- nm[which(str_detect(nm_clean, "codigo do municip") |
                             str_detect(nm_clean, "codigo do municipio"))][1]
  gdp_col        <- nm[which(str_detect(nm_clean, "produto interno bruto") & str_detect(nm_clean, "per capita"))][1]
  if (is.na(gdp_col)) gdp_col <- nm[which(str_detect(nm_clean, "pib") & str_detect(nm_clean, "per capita"))][1]

  if (is.na(muni_col)) stop("Could not find municipality column (Nome do Município) in: ", path)
  if (is.na(year_col)) stop("Could not find year column 'Ano' in: ", path)
  if (is.na(gdp_col)) stop("Could not find GDP per capita column in: ", path)

  # build tidy tibble using the detected columns (keeps as-character numeric conversion)
  out <- tibble::tibble(
    muni_orig  = as.character(df[[muni_col]]),
    adm1_name  = if (!is.na(adm1_col)) as.character(df[[adm1_col]]) else NA_character_,
    adm1_code  = if (!is.na(adm1_code_col)) as.character(df[[adm1_code_col]]) else NA_character_,
    muni_code  = if (!is.na(muni_code_col)) as.character(df[[muni_code_col]]) else NA_character_,
    year       = as.character(df[[year_col]]),
    gdp      = suppressWarnings(as.numeric(df[[gdp_col]]))
  )

  # dedupe identical rows (keeps first)
  out %>% distinct(muni_orig, adm1_name, muni_code, year, .keep_all = TRUE)
}

# read both files and combine
gdp_bra_full <- list(file_2002_2009, file_2010_2021) %>%
  keep(file.exists) %>%
  lapply(read_gdp_file) %>%
  bind_rows() %>%
  distinct(muni_orig, adm1_name, muni_code, year, .keep_all = TRUE)

gdp_bra_full_filter <- gdp_bra_full |> filter(muni_code == '1200450' )


prefect_BRA_long <- gdp_bra_full %>% 
  select(muni_code, year, gdp) %>% 
  left_join(muni_unique %>% select(muni_code, GID_2))


test <- prefect_BRA_long |> filter(GID_2 == 'BRA.1.20_2'  )


# prefect_BRA_long <- prefect_BRA_wide %>% 
#   pivot_longer(cols = -c(NAME_1, NL_NAME_1, NAME_2, NL_NAME_2, code, GID_2, GID_1), values_to = 'gdp', names_to = 'year') %>% 
#   arrange(GID_2)

ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)



if (file.exists('data_gis/r_pop_GHS_1989_2024_5arcmin.tif')){
  # load it
  r_pop <- rast('data_gis/r_pop_GHS_1989_2024_5arcmin.tif')
} else { # create it
  
  r_popCount_GP <- rast("/Users/mkummu/R/GIS_data_common/GHS_POP/r_pop_GHS_1985_2025_5arcmin.tif")
  
  r_pop <- subset(r_popCount_GP,5:40)
  
  writeRaster(r_pop,'data_gis/r_pop_GHS_1989_2024_5arcmin.tif', overwrite=TRUE)
  
  
  
}

# population for each subnat

timeSteps <- 2002:2021

adm2_BRA_geo <- read_sf( '/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg' ,  layer = 'ADM_2') %>% 
  filter(GID_0 == 'BRA') %>% 
  select(GID_0, GID_1, NAME_1,NL_NAME_1, GID_2, NAME_2, NL_NAME_2) %>% 
  arrange(NAME_1, NAME_2)

v_subnatPop <- exactextractr::exact_extract(subset(r_pop,14:33), adm2_BRA_geo, fun = 'sum')

v_subnatPop_comb <- adm2_BRA_geo %>% 
  st_drop_geometry() %>% 
  select(GID_1, GID_2) %>% 
  bind_cols(v_subnatPop) %>% 
  #select(-ID) %>% 
  set_names(c('GID_1', 'GID_2', paste0(timeSteps))) %>% 
  pivot_longer(names_to = 'year', values_to = 'pop', -c('GID_2', 'GID_1'))
  # # mutate(pop2021 = pop2020) %>%
  # mutate(pop1989 = pop1990) %>% 
  # mutate(pop2022 = pop2020) %>% 
  #select(GID_nmbr, cntry_id, paste0('pop',1989:2024))

# gdp per capita

prefect_BRA_long_combPop <- prefect_BRA_long %>% 
  left_join(v_subnatPop_comb) %>% 
  mutate(gdp_x_pop = gdp * pop) %>% 
  group_by(GID_1, year) %>% 
  mutate(adm1_pop = sum(pop, na.rm = TRUE)) %>% 
  mutate(adm1_gdp_x_pop = sum(gdp_x_pop, na.rm = TRUE)) %>% 
  mutate(adm1_gdp_pc = adm1_gdp_x_pop / adm1_pop) %>% 
  ungroup() %>% 
  mutate(adm2_gdp_ratio = gdp / adm1_gdp_pc) %>% 
  select(GID_1, GID_2, year, adm2_gdp_ratio) |> 
  ## put ratio as 1, when NA
  mutate(adm2_gdp_ratio = ifelse(is.na(adm2_gdp_ratio), 1, adm2_gdp_ratio))

# --- Ensure full year coverage 1990:2024 for every admin (GID_2).
# Existing values are preserved; years with no data remain NA.
years_full <- as.character(1990:2024)

# meta of admins (use prefect_BRA_long to include admins even if some years missing)
meta_admin <- prefect_BRA_long %>% 
  left_join(adm2_BRA_geo |> st_drop_geometry() |> select("GID_1", "GID_2")) %>%
  distinct(GID_1, GID_2)

meta_admin_adm2 <-  adm2_BRA_geo |> st_drop_geometry() |> select("GID_1", "GID_2") %>% 
  left_join(prefect_BRA_long) %>%
  distinct(GID_1, GID_2)

missing_ISO3 <- meta_admin_adm2 %>% filter(!GID_2 %in% prefect_BRA_long$GID_2)

# full grid admin x year
full_grid <- tidyr::crossing(meta_admin_adm2, year = years_full)

# make sure year formats match and join existing ratios onto the full grid
prefect_BRA_long_combPop <- prefect_BRA_long_combPop %>%
  mutate(year = as.character(year))

prefect_BRA_long_combPop_allYrs <- full_grid %>%
  left_join(prefect_BRA_long_combPop |> select(GID_2,year,adm2_gdp_ratio)) %>%
  # add value 1 for missing iso3s
  mutate(adm2_gdp_ratio = ifelse(is.na(adm2_gdp_ratio) & GID_2 %in% missing_ISO3$GID_2, 1, adm2_gdp_ratio)) %>%
  arrange(GID_2, year) 
  

# interpolate ratio
adm1_data_GDP_interpRatio <- prefect_BRA_long_combPop_allYrs %>% 
  # pivot_longer(-c(iso3, GID_nmbr), names_to = 'year', values_to = 'ratioAdm1Adm0') %>% 
  # interpolate
  group_by(GID_2) %>% 
  #https://stackoverflow.com/questions/70155104/interpolate-na-values-when-column-ends-on-na
  mutate(adm2_gdp_ratio = na.approx(adm2_gdp_ratio, maxgap = Inf, rule = 2)) %>% 
  ungroup() 
  #rename(!!as.name(outVarName) := ratioAdm1Adm0) %>% 
  #select(iso3, GID_nmbr, year, !!as.name(outVarName))

### calculate the GDP using adm1 level data and the ratio

# read in adm1 level gdp data from "data_in/gadm_level1_gdp_pc.xlsx"


### calculate the GDP using adm1 level data and the ratio

# read in adm1 level gdp data from "data_in/gadm_level1_gdp_pc.xlsx"
yearsIn <- 1990:2024
v_adm1 <- vect(paste0('results/polyg_adm1_gdp_perCapita_',yearsIn[1],'_',yearsIn[length(yearsIn)],'.gpkg'))

# t_GID_nmbr_GID_1 <- read_csv('downscalingMatlab/adm2DataForDownscaling.csv') |> 
#   filter(iso3 == 'BRA') |> 
#   distinct(GID_2, GID_nmbr) |> 
#   left_join(meta_admin) |> 
#   distinct(GID_nmbr, GID_1) |> 
#   arrange(GID_nmbr) |>
#   drop_na()

t_GID_nmbr_GID_1 <- vect("results/gisData_GDP_pc_combined_feb2024.gpkg") |> 
  as_tibble() |> 
  filter(iso3 == 'BRA') |> 
  distinct(GID_nmbr, GID_1) 

t_adm1 <- as_tibble(v_adm1) |> 
  filter(iso3 == 'BRA') %>%
  select(iso3, GID_nmbr, Subnat, paste0(1990:2024) ) |> 
  pivot_longer(cols = -c(iso3, GID_nmbr, Subnat), names_to = 'year', values_to = 'adm1_gdp_pc') %>% 
  mutate(year = as.character(year)) |> 
  left_join(t_GID_nmbr_GID_1, by = 'GID_nmbr')



# combine adm1 gdp pc and adm2 ratio

ad2_comb_adm1_data <- adm1_data_GDP_interpRatio |> 
  left_join(t_adm1, by = c('GID_1', 'year')) |> 
  mutate(adm2_gdp_pc = adm1_gdp_pc * adm2_gdp_ratio)

# write to file
fwrite(ad2_comb_adm1_data, 'results/adm2_BRA_gdp_per_capita_1990_2024.csv')



## histogram of the ratios by GID_1
library(ggplot2)
ggplot(data = ad2_comb_adm1_data, aes(x = adm2_gdp_ratio)) +
  geom_histogram(binwidth = 0.1) +
  # set xlim 0.4 to 1.5
  xlim(0.4, 1.5) +
  facet_wrap(~GID_1, scales = 'free_y') +
  labs(title = 'Histogram of ADM2 GDP per capita by ADM1', x = 'ADM2 GDP per capita', y = 'Count') +
  theme_minimal()
