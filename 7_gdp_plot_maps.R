

### plot GDP maps

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)


library(terra)
library(sf)

library(scico)
library(rnaturalearth)
library(rmapshaper)
library(tmap)

library(openxlsx)
library(readxl)

library(dplyr)
library(tidyverse)



# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



### 1. load data -----

sf_gdp_adm0 <- read_sf('results/polyg_adm0_gdp_perCapita_1990_2022.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  # remove antarctica
  filter(!iso3 == 'ATA')

sf_gdp_adm1 <- read_sf('results/polyg_adm1_gdp_perCapita_1990_2022.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  # remove antarctica
  filter(!iso3 == 'ATA')

sf_gdp_adm2 <- read_sf('results/polyg_adm2_gdp_perCapita_1990_2022.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  # remove antarctica
  filter(!iso3 == 'ATA')

sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>%
  # simplify the shapefile
  #rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf() %>%
  filter(!iso_a3 == 'ATA')



### 2. plot maps ----

# functions

source('functions/f_Plot_sfAbs.R')
source('functions/f_Plot_sfTrend.R') 


## 2.1 plot global maps

# get thresholds

minGDP <- quantile( log10(sf_gdp_adm2$'2022'), .01, na.rm=T)
maxGDP <- quantile( log10(sf_gdp_adm2$'2022'), .99, na.rm=T) 

gdpRange <- seq( plyr::round_any( minGDP,accuracy=0.1,f=floor ), 
                 plyr::round_any( maxGDP,accuracy=0.1,f=ceiling ) ,
                 by= 0.05) 


minSlope <- quantile(sf_gdp_adm2$slope, .01, na.rm = T)
maxSlope <- quantile(sf_gdp_adm2$slope, .99, na.rm = T)

slopeRange <- seq(round(minSlope,1),round(maxSlope,1),by=.05) 

p_gdpSlope_adm0 <- f_Plot_sfTrend(sf_gdp_adm0,'slope',slopeRange)
p_gdpSlope_adm1 <- f_Plot_sfTrend(sf_gdp_adm1,'slope',slopeRange)
p_gdpSlope_adm2 <- f_Plot_sfTrend(sf_gdp_adm2,'slope',slopeRange)

# gdp as a log10 as range is so large in abs numbers

sf_gdp_adm0_log10 <- sf_gdp_adm0 %>% 
  rename(yr2022 = '2022') %>% 
  mutate('log2022' = log10(yr2022)) %>% 
  select(log2022)


sf_gdp_adm1_log10 <- sf_gdp_adm1 %>% 
  rename(yr2022 = '2022') %>% 
  mutate('log2022' = log10(yr2022)) %>% 
  select(log2022)


sf_gdp_adm2_log10 <- sf_gdp_adm2 %>% 
  rename(yr2022 = '2022') %>% 
  mutate('log2022' = log10(yr2022)) %>% 
  select(log2022)

bbox_globe = st_bbox(sf_gdp_adm0_log10)

# plot

p_gdp_pc_adm0 <- f_Plot_sfAbs(sf_gdp_adm0_log10,'log2022',gdpRange, bbox_globe )
p_gdp_pc_adm1 <- f_Plot_sfAbs(sf_gdp_adm1_log10,'log2022',gdpRange, bbox_globe )
p_gdp_pc_adm2 <- f_Plot_sfAbs(sf_gdp_adm2_log10,'log2022',gdpRange, bbox_globe )



if (dir.exists('figures/figGDPpcMaps/')) {
  
} else {
  dir.create('figures/figGDPpcMaps/')  
}

layers <- list(p_gdp_pc_adm0, p_gdpSlope_adm0,
               p_gdp_pc_adm1, p_gdpSlope_adm1,
               p_gdp_pc_adm2, p_gdpSlope_adm2)

nameLayers <- c('p_gdp_pc_adm0', 'p_gdpSlope_adm0',
                'p_gdp_pc_adm1', 'p_gdpSlope_adm1',
                'p_gdp_pc_adm2', 'p_gdpSlope_adm2')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/figGDPpcMaps/fig_',nameLayers[i],'.png'),width = 400, units='mm', dpi = 600)
  
}


p_gdp <- tmap_arrange(p_gdp_pc_adm0, p_gdpSlope_adm0,
                      p_gdp_pc_adm1, p_gdpSlope_adm1,
                      p_gdp_pc_adm2, p_gdpSlope_adm2,
                      ncol = 2)

tmap_save(p_gdp,filename = paste0('figures/figGDPpcMaps/fig1_gdp_adm0_adm1_adm2_2022_log10slope1990_2022','.pdf'),
          width = 180, height=200, units='mm')




#### 2.2 plot some close ups 


bbox_india <- st_bbox(sf_adm0 %>% filter(iso_a3 == "IND"))

bbox_brazil <- st_bbox(sf_adm0 %>% filter(iso_a3 == "BRA"))

bbox_centralEur <- st_bbox(sf_adm0 %>% filter(iso_a3 %in% c("DEU", "ITA", "POL", "AND") ))


p_gdp_pc_adm0_IND <- f_Plot_sfAbs(sf_gdp_adm0_log10,'log2022',gdpRange, bbox_india )
p_gdp_pc_adm1_IND <- f_Plot_sfAbs(sf_gdp_adm1_log10,'log2022',gdpRange, bbox_india )
p_gdp_pc_adm2_IND <- f_Plot_sfAbs(sf_gdp_adm2_log10,'log2022',gdpRange, bbox_india )

p_gdp_pc_adm0_BRA <- f_Plot_sfAbs(sf_gdp_adm0_log10,'log2022',gdpRange, bbox_brazil )
p_gdp_pc_adm1_BRA <- f_Plot_sfAbs(sf_gdp_adm1_log10,'log2022',gdpRange, bbox_brazil )
p_gdp_pc_adm2_BRA <- f_Plot_sfAbs(sf_gdp_adm2_log10,'log2022',gdpRange, bbox_brazil )

p_gdp_pc_adm0_CE <- f_Plot_sfAbs(sf_gdp_adm0_log10,'log2022',gdpRange, bbox_centralEur )
p_gdp_pc_adm1_CE <- f_Plot_sfAbs(sf_gdp_adm1_log10,'log2022',gdpRange, bbox_centralEur )
p_gdp_pc_adm2_CE <- f_Plot_sfAbs(sf_gdp_adm2_log10,'log2022',gdpRange, bbox_centralEur )




if (dir.exists('figures/figGDPpcMaps_bbox/')) {
  
} else {
  dir.create('figures/figGDPpcMaps_bbox/')  
}

layers <- list(p_gdp_pc_adm0_IND,  p_gdp_pc_adm0_BRA, p_gdp_pc_adm0_CE,
               p_gdp_pc_adm1_IND,p_gdp_pc_adm1_BRA, p_gdp_pc_adm1_CE, 
               p_gdp_pc_adm2_IND, p_gdp_pc_adm2_BRA, p_gdp_pc_adm2_CE)

nameLayers <- c('p_gdp_pc_adm0_IND',  'p_gdp_pc_adm0_BRA', 'p_gdp_pc_adm0_CE',
                'p_gdp_pc_adm1_IND','p_gdp_pc_adm1_BRA', 'p_gdp_pc_adm1_CE', 
                'p_gdp_pc_adm2_IND', 'p_gdp_pc_adm2_BRA', 'p_gdp_pc_adm2_CE')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/figGDPpcMaps_bbox/fig_',nameLayers[i],'.png'),width = 100, units='mm', dpi = 600)
  
}

p_gdp_bbox <- tmap_arrange(p_gdp_pc_adm0_IND,  p_gdp_pc_adm0_BRA, p_gdp_pc_adm0_CE,
                           p_gdp_pc_adm1_IND,p_gdp_pc_adm1_BRA, p_gdp_pc_adm1_CE, 
                           p_gdp_pc_adm2_IND, p_gdp_pc_adm2_BRA, p_gdp_pc_adm2_CE,
                           ncol = 3)





tmap_save(p_gdp_bbox,filename = paste0('figures/figGDPpcMaps_bbox/figX_gdp_adm0_adm1_adm2_2022_bbox',Sys.Date(),'.pdf'),
          width = 180, height=200, units='mm')



#### 3. code for other plots -----

## metadata:  8_gdp_metadata_collect.R

## total GDP:  6_total_gdp.R

## validation: 9_adm2_validation.R 

## downscaling error: 10_downscalingErrorToMap.R



