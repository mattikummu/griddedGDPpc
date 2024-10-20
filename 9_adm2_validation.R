### validation at adm2 level

library(sf)
library(terra)
library(Rfast)
library(data.table)
library(zoo)
library(ggpubr)
library(gridExtra)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 

# clear environment
rm(list = ls())

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1. load data -----

cntry_info <- read_csv("data_in/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso3 = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso3 = ifelse(iso3 == 'XKX','KSV',iso3)) %>% 
  # northern cyprus
  mutate(iso3 = ifelse(iso3 == 'XNC','ZNC',iso3)) %>%  
  distinct(iso3, .keep_all = T)

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(iso3, .keep_all = T) %>% 
  left_join(cntry_info %>% select(iso3, cntry_id)) %>% 
  select(-cntry_code) %>% 
  select(cntry_id, iso2,  iso3,  Country  )


##### 2. validate against OECD adm 2 level data -----

# 2.1 read oecd gpkg

v_oecd_tl3 <- vect('/Users/mkummu/R/migration_data_bee/data_in/OECD_TL3_2020_fixed_valid.gpkg')
sf_oecd_tl3 <- st_read('/Users/mkummu/R/migration_data_bee/data_in/OECD_TL3_2020_fixed_valid.gpkg')

sf_oecd_tl3_noGeom <- sf_oecd_tl3 %>% 
  st_drop_geometry()

##### 2.2 read in OECD adm2 level reported data

adm0_oecd <- read_csv('data_in/OECD_tl3_sep2023.csv') %>% 
  filter(TL == 1) %>% 
  select(REG_ID, Region, Year, Value) %>% 
  arrange(REG_ID, Year)

iso3_adm0_oecd <- adm0_oecd %>% 
  distinct(REG_ID)

adm2_oecd <- read_csv('data_in/OECD_tl3_sep2023.csv') %>% 
  filter(TL == 3) %>% 
  select(REG_ID, Year, Value) %>% 
  arrange(REG_ID, Year)  %>% 
  rename(adm2_GDP = Value) %>% 
  left_join(sf_oecd_tl3_noGeom %>% rename(REG_ID = tl3_id)) %>% 
  left_join(adm0_oecd %>% rename(iso3 = REG_ID) %>% rename(adm0_GDP = Value)) %>% 
  mutate(oGdp_ratio = adm2_GDP / adm0_GDP) %>% 
  mutate(Year = as.character(Year))

adm2_oecd_wide <- adm2_oecd %>% 
  select(REG_ID, name_en, iso3, Region, Year, adm2_GDP) %>% 
  arrange(REG_ID, Year) %>% 
  pivot_wider(names_from = Year, values_from = adm2_GDP)%>% 
  select(REG_ID, name_en,       iso3,  Region, paste0(1995:2020))

adm2_oecd_wide_ratio <- adm2_oecd %>% 
  select(REG_ID, name_en, iso3, Region, Year, oGdp_ratio) %>% 
  arrange(REG_ID, Year) %>% 
  pivot_wider(names_from = Year, values_from = oGdp_ratio) %>% 
  select(REG_ID, name_en,       iso3,  Region, paste0(1995:2020))

write_csv(adm2_oecd_wide, "data_in/adm2_oecd_long.csv")

write_csv(adm2_oecd_wide_ratio, "data_in/adm2_oecd_wide_ratio.csv")


##### 2.3 extract data from adm2 raster

rast_gdpAdm2 <- rast('results/rast_adm2_gdp_perCapita_1990_2022.tif')


r_popCount_mod <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')

r_popCount_mod_ext <- extend(r_popCount_mod,subset(rast_gdpAdm2,1))


## extract

ext_gdp_x_pop <- exactextractr::exact_extract(rast_gdpAdm2*r_popCount_mod_ext,sf_oecd_tl3, fun='sum' )
dim(ext_gdp_x_pop)
ext_pop <- exactextractr::exact_extract(x= r_popCount_mod_ext,y=sf_oecd_tl3, fun='sum')
dim(ext_pop)

#### national data

adm0_gdp <- read_csv('results/adm0_gdp_pc_long_interpExtrap.csv') %>% 
  rename(r_gdpAdm0 = gdp_pc) %>% 
  mutate(Year = as.character(year)) %>% 
  distinct(.keep_all = T)


### 2.4 add to polygon

tl3_polyg_comb_gdpRaster <- sf_oecd_tl3_noGeom %>% 
  select(tl3_id) %>% 
  rename(REG_ID = tl3_id) %>% 
  bind_cols(ext_gdp_x_pop / ext_pop) %>% 
  #select(-c(ID)) %>% 
  set_names('REG_ID', paste0(1990:2022) ) %>% 
  as_tibble() %>% 
  pivot_longer(-c('REG_ID'), names_to = 'Year', values_to = 'r_gdpAdm2') %>% 
  distinct(.keep_all = T) %>% 
  left_join(sf_oecd_tl3_noGeom[,1:2] %>% rename(REG_ID = tl3_id)) %>% 
  left_join(adm0_gdp) %>% 
  mutate(rGdp_ratio = r_gdpAdm2 / r_gdpAdm0) %>% 
  distinct(.keep_all = T)


# 2.5 check countries for which OECD is finer resolution than the one collected for dataset

count_adm0_oecd <- sf_oecd_tl3_noGeom %>% 
  as_tibble() %>% 
  group_by(iso3) %>%
  summarise(iso3count = n())
    
count_adm0_rast <- st_read('results/polyg_adm1_gdp_perCapita_1990_2022.gpkg') %>% 
  as_tibble() %>% 
  group_by(iso3) %>%
  summarise(iso3count = n()) %>% 
  rename(iso3count_rast = iso3count)

# let's select countries for which we have at least double the amount of data in OECD
# than in our used reported data

comp_count <- count_adm0_oecd %>% 
  left_join(count_adm0_rast) %>% 
  mutate(countRatio = iso3count/iso3count_rast) %>% 
  filter(countRatio > 2)



##### 2.6 combine all the data

seed = 21

tl3_polyg_comb_rast_oecd <- adm2_oecd %>% 
  left_join(tl3_polyg_comb_gdpRaster %>% select(REG_ID, Year, r_gdpAdm2,r_gdpAdm0, rGdp_ratio)) %>% 
  # select only the countries identified above
  filter(iso3 %in% comp_count$iso3) %>% 
  filter(!oGdp_ratio == 0) %>% 
  mutate(adm0ratio = adm0_GDP /r_gdpAdm0 ) %>% 
  mutate(corrOECD_gdpAdm2 = adm2_GDP / adm0ratio) %>% 
  group_by(REG_ID) %>%
  sample_frac(.25) %>% 
  ungroup()

nSubnat = tl3_polyg_comb_rast_oecd %>% 
  distinct(REG_ID)

tl3_polyg_comb_rast_oecd_mean <- tl3_polyg_comb_rast_oecd %>% 
  group_by(REG_ID) %>% 
  summarise(mean_oRatio = mean(oGdp_ratio), mean_rRatio = mean(rGdp_ratio)) %>% 
  ungroup() %>% 
  mutate(ratio_Ratios = mean_oRatio / mean_rRatio)

tl3_polyg_comb_rast_oecd_mean_filt <- tl3_polyg_comb_rast_oecd_mean %>% 
  filter(mean_oRatio < quantile(tl3_polyg_comb_rast_oecd_mean$mean_oRatio, .975)) %>% 
  filter(mean_oRatio > quantile(tl3_polyg_comb_rast_oecd_mean$mean_oRatio, .025)) %>% 
  filter(mean_rRatio < quantile(tl3_polyg_comb_rast_oecd_mean$mean_rRatio, .975)) %>% 
  filter(mean_rRatio > quantile(tl3_polyg_comb_rast_oecd_mean$mean_rRatio, .025))

#### 2.7 plot correlation between reported and downscaled values


correlation_gdp <- c(cor(tl3_polyg_comb_rast_oecd$r_gdpAdm2,tl3_polyg_comb_rast_oecd$adm2_GDP,method='pearson') )

validation_gdp <- ggscatter(tl3_polyg_comb_rast_oecd, x = "corrOECD_gdpAdm2", y = "r_gdpAdm2", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method =  "pearson",
                              xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 100000) + 
  ylim(0, 100000) 

validation_gdp

# 2.8 plot same but filter out the largest GDP per capita values to check how much this impacts on correlation

tl3_polyg_comb_rast_oecd_filt <- tl3_polyg_comb_rast_oecd %>% 
  filter(r_gdpAdm2 < 70000)

correlation_gdp_filt <- c(cor(tl3_polyg_comb_rast_oecd_filt$r_gdpAdm2,tl3_polyg_comb_rast_oecd_filt$adm2_GDP,method='pearson') )

validation_gdp_filt <- ggscatter(tl3_polyg_comb_rast_oecd_filt, x = "corrOECD_gdpAdm2", y = "r_gdpAdm2", 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method =  "pearson",
                            xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 100000) + 
  ylim(0, 100000) 

validation_gdp_filt

plot_comb <- grid.arrange(validation_gdp, validation_gdp_filt, ncol = 2)

ggsave('figures/validation_gdp_oecd.pdf', plot_comb, width = 180, height = 80, units = 'mm')

#ggsave('figures/validation_gdp_oecd_filt.pdf', validation_gdp_filt, width = 120, height = 120, units = 'mm')


# 2.9 Calculate the deviation from the 1:1 line
tl3_polyg_comb_rast_oecd$deviation <- tl3_polyg_comb_rast_oecd$r_gdpAdm2 - tl3_polyg_comb_rast_oecd$adm2_GDP

# Print the first few rows of the data frame to check the new 'deviation' column
head(tl3_polyg_comb_rast_oecd)

# Summary statistics for deviations
summary(tl3_polyg_comb_rast_oecd$deviation)


##### 3. validate against WorldBank adm 0 level data -----

## 3.1 load data

sf_adm0 <- read_sf('data_gis/gdp_adm0_polyg_simple.gpkg')

rast_gdpAdm1 <- rast('results/rast_adm1_gdp_perCapita_1990_2022.tif')

rast_gdpAdm2 <- rast('results/rast_adm2_gdp_perCapita_1990_2022.tif')

r_popCount_mod <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')

r_popCount_mod_ext <- extend(r_popCount_mod,subset(rast_gdpAdm2,1))



#national data, reported (from 1_gdp_prepare_adm0.R)

adm0_reported <- read_csv('results/adm0_reported_data.csv') %>% 
  pivot_longer(-iso3, names_to = 'year', values_to = 'gdp_pc') %>% 
  drop_na()


### 3.2 calculate national data from downscaled raster

# join cntry info
adm0_polyg_final <- sf_adm0 %>% 
  left_join(cntry_info %>% 
              select(country_name, iso3, cntry_id)  %>% 
              rename(GID_nmbr = cntry_id) %>% 
              rename(Country = country_name)) %>% 
  filter(!is.na(GID_nmbr)) %>% 
  select(Country, GID_nmbr, iso3)



## extract

# adm1
ext_gdp_x_pop_adm1 <- exactextractr::exact_extract(rast_gdpAdm1*r_popCount_mod_ext,adm0_polyg_final, fun='sum' )
dim(ext_gdp_x_pop_adm1)

# adm2
ext_gdp_x_pop_adm2 <- exactextractr::exact_extract(rast_gdpAdm2*r_popCount_mod_ext,adm0_polyg_final, fun='sum' )
dim(ext_gdp_x_pop_adm2)

# pop
ext_pop <- exactextractr::exact_extract(x= r_popCount_mod_ext,y=adm0_polyg_final, fun='sum')
dim(ext_pop)



# 3.3 for admin 1 level data

sf_adm0_comb_adm1 <- adm0_polyg_final %>%
  st_drop_geometry() %>% 
  bind_cols(ext_pop[,33] / 10^6) %>% 
  bind_cols(ext_gdp_x_pop_adm1 / ext_pop) %>%
  
  #select(-c(ID)) %>%
  set_names('Country', 'GID_nmbr', 'iso3', 'Pop2022_millions', paste0(1990:2022) ) %>%
  as_tibble() %>%
  pivot_longer(-c('Country', 'GID_nmbr', 'iso3', 'Pop2022_millions'), names_to = 'year', values_to = 'gdp_pc_downscaled') %>%
  distinct(.keep_all = T) %>%
  left_join(adm0_reported) %>%
  distinct(.keep_all = T) %>% 
  drop_na() %>% 
  filter(!gdp_pc_downscaled == 0) #%>% 
  # let's use only countries with more than 100k people
  #filter(Pop2022_millions > 0.1)


# 3.3.1 correlation

correlation_gdp_adm0_adm1 <- c(cor(sf_adm0_comb_adm1$gdp_pc_downscaled,sf_adm0_comb_adm1$gdp_pc,method='pearson') )

validation_gdp_adm0_adm1 <- ggscatter(sf_adm0_comb_adm1, x = "gdp_pc", y = "gdp_pc_downscaled", 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method =  "pearson",
                            xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 150000) + 
  ylim(0, 150000) 

validation_gdp_adm0_adm1


length(unique(sf_adm0_comb_adm1$iso3))

# Calculate the deviation from the 1:1 line
sf_adm0_comb_adm1$deviation <- sf_adm0_comb_adm1$gdp_pc_downscaled - sf_adm0_comb_adm1$gdp_pc

# arrange
sf_adm0_comb_adm1 <-  sf_adm0_comb_adm1 %>% 
  arrange(deviation)




# 3.4 for admin 2 level data

sf_adm0_comb_adm2 <- adm0_polyg_final %>%
  bind_cols(ext_gdp_x_pop_adm2 / ext_pop) %>%
  st_drop_geometry() %>% 
  #select(-c(ID)) %>%
  set_names('Country', 'GID_nmbr', 'iso3', paste0(1990:2022) ) %>%
  as_tibble() %>%
  pivot_longer(-c('Country', 'GID_nmbr', 'iso3'), names_to = 'year', values_to = 'gdp_pc_downscaled') %>%
  distinct(.keep_all = T) %>%
  left_join(adm0_reported) %>%
  distinct(.keep_all = T) %>% 
  drop_na() %>% 
  filter(!gdp_pc_downscaled == 0)



correlation_gdp_adm0_adm2 <- c(cor(sf_adm0_comb_adm2$gdp_pc_downscaled,sf_adm0_comb_adm2$gdp_pc,method='pearson') )

validation_gdp_adm0_adm2 <- ggscatter(sf_adm0_comb_adm2, x = "gdp_pc", y = "gdp_pc_downscaled", 
                                      add = "reg.line", conf.int = TRUE, 
                                      cor.coef = TRUE, cor.method =  "pearson",
                                      xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1)  + 
  xlim(0, 150000) + 
  ylim(0, 150000) 

validation_gdp_adm0_adm2


## 3.5 save plot

plot_comb_adm0 <- grid.arrange(validation_gdp_adm0_adm1, validation_gdp_adm0_adm2, ncol = 2)

ggsave('figures/validation_gdp_reportedAdm0.pdf', plot_comb_adm0, width = 180, height = 80, units = 'mm')

length(unique(sf_adm0_comb_adm2$iso3))

# Calculate the deviation from the 1:1 line
sf_adm0_comb_adm2$deviation <- sf_adm0_comb_adm2$gdp_pc_downscaled - sf_adm0_comb_adm2$gdp_pc

# Print the first few rows of the data frame to check the new 'deviation' column
sf_adm0_comb_adm2 <-  sf_adm0_comb_adm2 %>% 
  arrange(deviation)


