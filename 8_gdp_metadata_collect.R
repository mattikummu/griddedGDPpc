

### metadata for the GDP dataset


#library(raster)
library(terra)
library(sf)

library(scico)
library(rnaturalearth)
library(rmapshaper)
library(tmap)

library(purrr)

library(openxlsx)
library(readxl)

library(dplyr)
library(tidyverse)



# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))




#  adm0 origin
adm0_source_comb <- read_csv('results/adm0_source_comb.csv')

count(adm0_source_comb, source)

# add to metadata

adm0_GADM <- read_sf( '/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg' ,  layer = 'ADM_0') %>% 
  st_drop_geometry() %>% 
  select(GID_0,COUNTRY) %>% 
  rename(iso3 = GID_0) %>% 
  mutate(iso3 = ifelse(iso3 == "XKO", 'KSV', iso3)) %>% 
  filter(!iso3 == 'ALA') 

# for China, Pakistan, India let's use older version of GADM so that these Z areas
# will be correctly represented. Also Hong Kong and Macao will this way be there as
# individual countries

adm0_gadm_old <- read_sf('data_gis/gadm_level0.gpkg') %>% 
  st_drop_geometry() %>% 
  rename(iso3 = GID_0) %>% 
  rename(COUNTRY = NAME_0) %>% 
  filter(iso3 %in% c('CHN', 'PAK', 'IND', 'HKG', 'MAC'))

adm0_GADM_corr <- adm0_GADM %>% 
  filter(!iso3 %in% c('CHN', 'PAK', 'IND', 'HKG', 'MAC')) %>% 
  bind_rows(adm0_gadm_old) %>% 
  left_join(adm0_source_comb) %>% 
  rename(adm0_source = source) %>% 
  filter(!is.na(adm0_source))



write_csv(adm0_GADM_corr, 'results/amd0_source.csv')


updGIS <- read_excel('data_in/updates_feb2024.xlsx', sheet = 'meta', skip = 2) %>% 
  select(-Country, - GIS) %>% 
  rename(Years_3 = time) %>% 
  rename(Source_3 = DataSource) %>% 
  rename(WWW_3 = Source) %>% 
  rename(Notes_3 = Notes)

mariaMeta <- read_excel('data_in/MARIA_metadata_updated_mk.xlsx', sheet = 'Metadata', skip = 3)  %>% 
  select(iso3, Years, Source, WWW, Notes) %>% 
  filter(!is.na(Years)) %>% 
  rename(Years_2 = Years) %>% 
  rename(Source_2 = Source) %>% 
  rename(WWW_2 = WWW) %>% 
  rename(Notes_2 = Notes)


# aavilable years for each country

adm0_reported_years <- read_csv('results/adm0_reported_data.csv') %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'gdp_pc') %>% 
  drop_na() %>% 
  distinct(iso3, year) %>% 
  group_by(iso3) %>%
  summarize(Years = paste(year, collapse = ", ")) 

# aavilable years for each subnat area


adm1_reported_years <- read_csv( "results/subnat_gis_combined_feb2024.csv") %>% 
  select(iso3, paste0(1990:2021)) %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'gdp_pc') %>% 
  drop_na() %>% 
  distinct(iso3, year) %>% 
  group_by(iso3) %>%
  summarize(Years = paste(year, collapse = ", ")) 

adm1_reported_data <- read_csv( "results/subnat_gis_combined_feb2024.csv") 

uniqueISO3 <- adm1_reported_data %>% 
  select(iso3) %>% 
  distinct()

uniqueSubnat <- adm1_reported_data %>% 
  select(GID_nmbr) %>% 
  distinct()

# Function to create year ranges
create_year_ranges_3 <- function(years) {
  if (is.na(years)) {
    return(NA)
  }
  
  years <- as.numeric(strsplit(years, ", ")[[1]])
  ranges <- character(0)
  
  if (length(years) == 1) {
    return(as.character(years))
  }
  
  start <- years[1]
  for (i in 2:length(years)) {
    if (years[i] != years[i - 1] + 1) {
      if (start != years[i - 1]) {
        ranges <- c(ranges, paste(start, years[i - 1], sep = "-"))
      } else {
        ranges <- c(ranges, as.character(start))
      }
      start <- years[i]
    }
  }
  
  if (start != years[length(years)]) {
    ranges <- c(ranges, paste(start, years[length(years)], sep = "-"))
  } else {
    ranges <- c(ranges, as.character(start))
  }
  
  return(paste(ranges, collapse = ", "))
}


# Apply the function using mutate



adm0_reported_years_ranges <- adm0_reported_years %>%
  mutate(YearRanges = sapply(Years, create_year_ranges_3)) %>% 
  select(-Years) %>% 
  mutate(iso3 = ifelse(iso3 == "WBG", 'PSE', iso3)) %>% 
  # source
  left_join(adm0_GADM_corr)  %>% 
  select(iso3, COUNTRY, YearRanges, adm0_source)




adm1_reported_years_ranges <- adm1_reported_years %>%
  mutate(YearRanges = sapply(Years, create_year_ranges_3)) %>% 
  select(-Years) %>% 
  # add Maria's metadata
  left_join(mariaMeta) %>% 
  left_join(updGIS) %>% 
  mutate(Years_3 = ifelse(Years_3 == 'whole timeseries', YearRanges, Years_3))


temp_sample <- adm1_reported_years_ranges %>% 
  select(iso3, YearRanges, Years_2, Years_3) 

names(temp_sample) <- c('iso3', 'years1', 'years2', 'years3')




# Function to expand year ranges
expand_year_range <- function(year_range) {
  ranges <- strsplit(year_range, ", ")
  expanded_years <- character(0)
  
  for (range in ranges) {
    range_parts <- strsplit(range, "-")
    if (length(range_parts[[1]]) == 2) {
      start <- as.numeric(range_parts[[1]][1])
      end <- as.numeric(range_parts[[1]][2])
      expanded_years <- c(expanded_years, as.character(start:end))
    } else {
      expanded_years <- c(expanded_years, as.character(range_parts[[1]]))
    }
  }
  
  return(expanded_years)
}

# Function to create year ranges from lists
create_year_ranges_from_lists <- function(years_list) {
  ranges_list <- list()
  
  for (years in years_list) {
    years <- as.numeric(years)
    ranges <- character(0)
    
    if (length(years) == 1) {
      ranges <- as.character(years)
    } else {
      start <- years[1]
      for (i in 2:length(years)) {
        if (years[i] != years[i - 1] + 1) {
          if (start != years[i - 1]) {
            ranges <- c(ranges, paste(start, years[i - 1], sep = "-"))
          } else {
            ranges <- c(ranges, as.character(start))
          }
          start <- years[i]
        }
      }
      
      if (start != years[length(years)]) {
        ranges <- c(ranges, paste(start, years[length(years)], sep = "-"))
      } else {
        ranges <- c(ranges, as.character(start))
      }
    }
    
    ranges_list <- append(ranges_list, list(paste(ranges, collapse = ", ")))
  }
  
  return(ranges_list)
}
# Expand year ranges into individual years for each column
expanded_data <- temp_sample %>%
  mutate(
    years1_expanded = lapply(str_split(years1, ", "), expand_year_range),
    years2_expanded = lapply(str_split(years2, ", "), expand_year_range),
    years3_expanded = lapply(str_split(years3, ", "), expand_year_range)
  ) %>%
  select(-years1, -years2, -years3) #%>%
# mutate(
#   years1_expanded = sapply(years1_expanded, paste, collapse = ", "),
#   years2_expanded = sapply(years2_expanded, paste, collapse = ", "),
#   years3_expanded = sapply(years3_expanded, paste, collapse = ", ")
# )

# Print the result
print(expanded_data)


split_expanded_data <- expanded_data %>% 
  rowwise() %>%
  mutate(
    years_in_both_1_2 = list(intersect(years2_expanded, years1_expanded)),
    years_in_both_but_not_in_years3 = list(setdiff(years_in_both_1_2, years3_expanded)),
    #year_ranges_2 = list(create_year_ranges_from_lists(years_in_both_but_not_in_years3)),
    years_in_both_2_3 = list(union(years_in_both_but_not_in_years3, years3_expanded)),
    years_only_in_years1 = list(setdiff(years1_expanded, years_in_both_2_3)),
    #year_ranges_1 = list(create_year_ranges_from_lists(years_only_in_years1))
  )

split_expanded_data_sel <- split_expanded_data %>% 
  select(iso3, years_only_in_years1, years_in_both_but_not_in_years3, years3_expanded) %>% 
  mutate(years_in_years1_str = paste( unlist(years_only_in_years1), collapse=', '),
         years_in_years2_str = paste( unlist(years_in_both_but_not_in_years3), collapse=', '),
         years_in_years3_str = paste( unlist(years3_expanded), collapse=', ')) %>% 
  mutate(years_in_years1_str = ifelse(years_in_years1_str == "", NA, years_in_years1_str),
         years_in_years2_str = ifelse(years_in_years2_str == "", NA, years_in_years2_str),
         years_in_years3_str = ifelse(years_in_years3_str == "", NA, years_in_years3_str)) %>% 
  mutate(YearRanges1 = sapply(years_in_years1_str, create_year_ranges_3),
         YearRanges2 = sapply(years_in_years2_str, create_year_ranges_3),
         YearRanges3 = sapply(years_in_years3_str, create_year_ranges_3)) %>% 
  select(iso3,YearRanges1,YearRanges2,YearRanges3)


adm1_metaData <- adm0_reported_years_ranges %>% 
  filter(!is.na(adm0_source)) %>% 
  rename(YearRanges0 = YearRanges) %>% 
  left_join(adm1_reported_years_ranges) %>% 
  left_join(split_expanded_data_sel) %>% 
  mutate(Source_1 = ifelse(is.na(YearRanges1), NA, 'historical')) %>% 
  mutate(Source_2 = ifelse(is.na(YearRanges2), NA, Source_2),
         WWW_2 = ifelse(is.na(YearRanges2), NA, WWW_2),
         Notes_2 = ifelse(is.na(YearRanges2), NA, Notes_2)) %>% 
  select(iso3, COUNTRY, YearRanges0, adm0_source, YearRanges, YearRanges1, Source_1, 
         YearRanges2, Source_2, WWW_2, Notes_2, YearRanges3, Source_3, WWW_3, Notes_3)

write_csv(adm1_metaData, 'results/adm1_metaData_feb2024.csv')



#### number of observations for downscaling ----

admAreasDownscaling <- read_csv("downscalingMatlab/adm1DataForDownscaling_mar2024.csv")

nrow(admAreasDownscaling)



## plot data origin ----

# collect data

# source: https://www.naturalearthdata.com/downloads/

sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>%
  # simplify the shapefile
  #rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf() %>%
  filter(!iso_a3 == 'ATA')

sf_adm1 <- read_sf('results/gisData_GDP_pc_combined_feb2024.gpkg') 
  # simplify the shapefile
  #rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) 


sf_dataReported <- read_sf('results/gisData_GDP_pc_combined_feb2024.gpkg') %>% 
  st_drop_geometry() %>% 
  mutate(nmbrObs = rowSums(!is.na(.)) - 6) %>% 
  select(Country, iso3, GID_nmbr, nmbrObs) %>% 
  select(iso3, nmbrObs) %>% 
  distinct(.keep_all = T)

sf_dataReported_range <- read_sf('results/gisData_GDP_pc_combined_feb2024.gpkg') %>% 
  st_drop_geometry() %>% 
  select(-c(Country, cntry_code, Subnat, GID_1)) %>% 
  pivot_longer(-c('iso3','GID_nmbr'), names_to = 'year', values_to = 'gdo_pc') %>% 
  drop_na() %>% 
  group_by(iso3) %>% 
  summarise(minYear = min(year), maxYear=max(year)) %>% 
  ungroup() %>% 
  mutate(rangeYear = as.numeric(maxYear) - as.numeric(minYear) + 1)
  

sf_adm0_data <- sf_adm0 %>% 
  select(iso_a3) %>%
  rename(iso3 = iso_a3) %>% 
  left_join(sf_dataReported) %>% 
  
  left_join(sf_dataReported_range %>% distinct(iso3, .keep_all = T)) 
  



pal = scico(8, begin = 0.1, end = 0.9, direction = -1, palette = 'lajolla')
breaksNmbr = c(0,2,5,10,15,20,25,Inf)

plt_giniDataNmbrYears <- tm_shape(sf_adm0_data, projection = "+proj=robin") +
  tm_fill(col = "nmbrObs",
          palette = pal,
          breaks = breaksNmbr,
          #labels = birthDataOrigin,
          #contrast = c(0, 0.9)
  )+
  tm_shape(sf_adm1, projection = "+proj=robin") +
  tm_borders(col = "white",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)


plt_giniDataRangeYears <- tm_shape(sf_adm0_data, projection = "+proj=robin") +
  tm_fill(col = "rangeYear",
          palette = pal,
          breaks = breaksNmbr,
          #labels = birthDataOrigin,
          #contrast = c(0, 0.9)
  )+
  tm_shape(sf_adm1, projection = "+proj=robin") +
  tm_borders(col = "white",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE) 

plt_dataOrigin <- tmap_arrange(plt_giniDataNmbrYears, plt_giniDataRangeYears, 
                               ncol = 1 )


if (!dir.exists('figures/')) {
  dir.create('figures/')
}

tmap_save(plt_dataOrigin,filename = "figures/plt_gdpDataRange.pdf", width = 130, height = 95, units = "mm")








