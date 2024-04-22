### validation at adm2 level

library(sf)
library(terra)
library(Rfast)
library(data.table)
library(zoo)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 

# clear environment
rm(list = ls())

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### load data -----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(iso3, .keep_all = T)

cntry_info <- read_csv("data_in/cntry_ids.csv") %>%
  as_tibble() %>% 
  rename(iso3 = country_code) %>% 
  # change iso_code for kosovo to match the one in data
  mutate(iso3 = ifelse(iso3 == 'XKX','KSV',iso3)) %>% 
  # northern cyprus
  mutate(iso3 = ifelse(iso3 == 'XNC','ZNC',iso3)) %>%  
  distinct(iso3, .keep_all = T)

##### oecd gpkg

v_oecd_tl3 <- vect('data_gis/OECD_TL3_2020_fixed_valid.gpkg')
sf_oecd_tl3 <- st_read('data_gis/OECD_TL3_2020_fixed_valid.gpkg')

sf_oecd_tl3_noGeom <- sf_oecd_tl3 %>% 
  st_drop_geometry()

##### read in OECD adm2 level reported data

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


##### extract data from adm2 raster

rast_gdpAdm2 <- rast('results/rast_adm2_gdpPerCapita_1990_2022.tif')

r_popCount <- rast('data_gis/popRaster_1990_2020.tif')

# pop missing from some areas for 1990-1999; let's use year 2000 to fill those

r_popCount_1990_99 <- subset(r_popCount, 1:10)
r_popCount_2000 <- subset(r_popCount, 11)
r_popCount_1990 <-  subset(r_popCount, 1)

r_popCount_1990[is.na(r_popCount_1990)] <- r_popCount_2000

r_popCount_1990_99[is.na(subset(r_popCount_1990_99,10))] <- r_popCount_2000
r_popCount_1990_99[subset(r_popCount_1990_99,10) == 0] <- r_popCount_2000

r_popCount_mod <- c(r_popCount_1990_99, subset(r_popCount,11:31))


r_popCount_mod_ext <- extend(r_popCount_mod,subset(rast_gdpAdm2,1))

# use 2020 pop for 2021
r_popCount_mod_ext <- c(r_popCount_mod_ext,subset(r_popCount_mod_ext,31))

## extract

ext_gdp_x_pop <- terra::extract(subset(rast_gdpAdm2,1:32)*r_popCount_mod_ext,v_oecd_tl3, fun=sum, na.rm=T )
dim(ext_gdp_x_pop)
ext_pop <- terra::extract(x= r_popCount_mod_ext,y=v_oecd_tl3, fun=sum, na.rm=T)
dim(ext_pop)


#### national data

adm0_gdp <- read_csv('results/adm0_gdp_pc_long_interpExtrap.csv') %>% 
  rename(r_gdpAdm0 = gdp_pc) %>% 
  mutate(Year = as.character(year)) %>% 
  distinct(.keep_all = T)


### add to polygon

tl3_polyg_comb_gdpRaster <- sf_oecd_tl3_noGeom %>% 
  select(tl3_id) %>% 
  rename(REG_ID = tl3_id) %>% 
  bind_cols(ext_gdp_x_pop / ext_pop) %>% 
  select(-c(ID)) %>% 
  set_names('REG_ID', paste0(1990:2021) ) %>% 
  as_tibble() %>% 
  pivot_longer(-c('REG_ID'), names_to = 'Year', values_to = 'r_gdpAdm2') %>% 
  distinct(.keep_all = T) %>% 
  left_join(sf_oecd_tl3_noGeom[,1:2] %>% rename(REG_ID = tl3_id)) %>% 
  left_join(adm0_gdp) %>% 
  mutate(rGdp_ratio = r_gdpAdm2 / r_gdpAdm0) %>% 
  distinct(.keep_all = T)


# check countries for which OECD is finer resolution than the one collected for dataset

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



##### combine

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

#### correlation

correlation_ratio_sample  <- c(cor(tl3_polyg_comb_rast_oecd$rGdp_ratio,tl3_polyg_comb_rast_oecd$oGdp_ratio,method='pearson') )


correlation_ratioMean <- c(cor(tl3_polyg_comb_rast_oecd_mean$mean_rRatio,tl3_polyg_comb_rast_oecd_mean$mean_oRatio,method='pearson') )

#correlation_gdp <- c(cor(tl3_polyg_comb_rast_oecd$r_gdpAdm2,tl3_polyg_comb_rast_oecd$adm2_GDP,method='pearson') )

# wCorrelation <- weightedCorr(tl3_polyg_comb_rast_oecd$rGdp_ratio,tl3_polyg_comb_rast_oecd$oGdp_ratio,
#                              method = c("Pearson"), weights = oecdPolyData_mortality$pop)

library(ggpubr)


validation_ratio <- ggscatter(tl3_polyg_comb_rast_oecd, x = "oGdp_ratio", y = "rGdp_ratio", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method =  "pearson",
                              xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) 

validation_ratio


validation_ratio <- ggplot(tl3_polyg_comb_rast_oecd, aes(x = oGdp_ratio, y = rGdp_ratio, color = iso3)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  labs(x = "Observed Value", y = "Downscaled Value")


validation_ratio_mean <- ggscatter(tl3_polyg_comb_rast_oecd_mean, x = "mean_oRatio", y = "mean_rRatio", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method =  "pearson",
                              xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) 


validation_ratio_mean









correlation_ratio <- c(cor(tl3_polyg_comb_rast_oecd$rGdp_ratio,tl3_polyg_comb_rast_oecd$oGdp_ratio,method='pearson') )

correlation_gdp <- c(cor(tl3_polyg_comb_rast_oecd$r_gdpAdm2,tl3_polyg_comb_rast_oecd$adm2_GDP,method='pearson') )

# wCorrelation <- weightedCorr(tl3_polyg_comb_rast_oecd$rGdp_ratio,tl3_polyg_comb_rast_oecd$oGdp_ratio,
#                              method = c("Pearson"), weights = oecdPolyData_mortality$pop)

library(ggpubr)

validation_ratio <- ggscatter(tl3_polyg_comb_rast_oecd, x = "oGdp_ratio", y = "rGdp_ratio", 
                        add = "reg.line", conf.int = TRUE, 
                        cor.coef = TRUE, cor.method =  "pearson",
                        xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) 

validation_gdp <- ggscatter(tl3_polyg_comb_rast_oecd, x = "corrOECD_gdpAdm2", y = "r_gdpAdm2", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method =  "pearson",
                              xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) 

 

if (!dir.exists('figures/')) {
  dir.create('figures/')
}

ggsave('figures/validation_gdp_oecd.pdf', validation_gdp, width = 120, height = 120, units = 'mm')

ggsave('figures/validation_gdpRatio_oecd.pdf', validation_ratio, width = 90, height = 90, units = 'mm')





