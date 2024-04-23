

#library(raster)
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



### load data

sf_gdp_adm0 <- read_sf('results/polyg_adm0_gdp_pc_1990_2022.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  # remove antarctica
  filter(!iso3 == 'ATA')

sf_gdp_adm1 <- read_sf('results/polyg_adm1_gdp_pc_1990_2022.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  # remove antarctica
  filter(!iso3 == 'ATA')

sf_gdp_adm2 <- read_sf('results/polyg_adm2_gdp_pc_1990_2022.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  # remove antarctica
  filter(!iso3 == 'ATA')

# source: https://www.naturalearthdata.com/downloads/

sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>%
  # simplify the shapefile
  #rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf() %>%
  filter(!iso_a3 == 'ATA')

r_gdp_pc_2022 <- subset(rast('results/rast_adm2_gdp_pc_1990_2022.tif'), 33)

writeRaster(x = r_gdp_pc_2022,
            filename = "results/r_gdp_pc_2022.tif", gdal="COMPRESS=LZW",overwrite=T)







### map functions ----


myPlot_sfAbs<-function(sf_in,column_in,breaks_in, boundBox){
  
  pal <-  scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "nuuk")
  
  plt_subnatMigr <- tm_shape(sf_in, projection = "+proj=robin", bbox = boundBox) +
    tm_fill(col = column_in,
            palette = pal,
            #contrast = c(0, 0.7),
            breaks = breaks_in,
            lwd=0.0,
            legend.is.portrait = FALSE)+
    tm_shape(sf_adm0, projection = "+proj=robin", bbox = boundBox) +
    tm_borders(col = "black",
               lwd = 0.1)+
    tm_layout(#main.title = "Origin of data",
      main.title.position = "center",
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      legend.text.size = .25,
      legend.title.size = .75,
      legend.width = 0.6,
      frame = FALSE)
}

myPlot_sfNetMgr<-function(sf_in,column_in,breaks_in){
  
  pal <-  scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "vik")
  
  plt_subnatMigr <- tm_shape(sf_in, projection = "+proj=robin") +
    tm_fill(col = column_in,
            palette = pal,
            breaks = breaks_in,
            midpoint = 0,
            contrast = c(0, 0.7),
            lwd=0,
            legend.is.portrait = FALSE)+
    tm_shape(sf_adm0, projection = "+proj=robin") +
    tm_borders(col = "black",
               lwd = 0.1)+
    tm_layout(#main.title = "Origin of data",
      main.title.position = "center",
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      legend.text.size =.25,
      legend.title.size = .75,
      legend.width = 0.6,
      #legend.height = -10, 
      frame = FALSE)
}



#### plot maps ----

minGDP <- quantile( log10(sf_gdp_adm2$'2022'), .01, na.rm=T)
maxGDP <- quantile( log10(sf_gdp_adm2$'2022'), .99, na.rm=T) 

gdpRange <- seq( plyr::round_any( minGDP,accuracy=0.1,f=floor ), 
                 plyr::round_any( maxGDP,accuracy=0.1,f=ceiling ) ,
                 by= 0.05) 


minSlope <- quantile(sf_gdp_adm2$slope, .01, na.rm = T)
maxSlope <- quantile(sf_gdp_adm2$slope, .99, na.rm = T)

slopeRange <- seq(round(minSlope,1),round(maxSlope,1),by=.05) 

p_gdpSlope_adm0 <- myPlot_sfNetMgr(sf_gdp_adm0,'slope',slopeRange)
p_gdpSlope_adm1 <- myPlot_sfNetMgr(sf_gdp_adm1,'slope',slopeRange)
p_gdpSlope_adm2 <- myPlot_sfNetMgr(sf_gdp_adm2,'slope',slopeRange)

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

p_gdp_pc_adm0 <- myPlot_sfAbs(sf_gdp_adm0_log10,'log2022',gdpRange, bbox_globe )
p_gdp_pc_adm1 <- myPlot_sfAbs(sf_gdp_adm1_log10,'log2022',gdpRange, bbox_globe )
p_gdp_pc_adm2 <- myPlot_sfAbs(sf_gdp_adm2_log10,'log2022',gdpRange, bbox_globe )


p_gdp <- tmap_arrange(p_gdp_pc_adm0, p_gdpSlope_adm0,
                      p_gdp_pc_adm1, p_gdpSlope_adm1,
                      p_gdp_pc_adm2, p_gdpSlope_adm2,
                      ncol = 2)

 
if (!dir.exists('figures/')) {
  dir.create('figures/')
}

tmap_save(p_gdp,filename = paste0('figures/fig1_gdp_adm0_adm1_adm2_2022_log10slope1990_2022',Sys.Date(),'.pdf'),
          width = 180, height=200, units='mm')




#### plot some close ups ----


bbox_india <- st_bbox(sf_adm0 %>% filter(iso_a3 == "IND"))

bbox_brazil <- st_bbox(sf_adm0 %>% filter(iso_a3 == "BRA"))

bbox_centralEur <- st_bbox(sf_adm0 %>% filter(iso_a3 %in% c("DEU", "ITA") ))


p_gdp_pc_adm0_IND <- myPlot_sfAbs(sf_gdp_adm0_log10,'log2022',gdpRange, bbox_india )
p_gdp_pc_adm1_IND <- myPlot_sfAbs(sf_gdp_adm1_log10,'log2022',gdpRange, bbox_india )
p_gdp_pc_adm2_IND <- myPlot_sfAbs(sf_gdp_adm2_log10,'log2022',gdpRange, bbox_india )

p_gdp_pc_adm0_BRA <- myPlot_sfAbs(sf_gdp_adm0_log10,'log2022',gdpRange, bbox_brazil )
p_gdp_pc_adm1_BRA <- myPlot_sfAbs(sf_gdp_adm1_log10,'log2022',gdpRange, bbox_brazil )
p_gdp_pc_adm2_BRA <- myPlot_sfAbs(sf_gdp_adm2_log10,'log2022',gdpRange, bbox_brazil )

p_gdp_pc_adm0_CE <- myPlot_sfAbs(sf_gdp_adm0_log10,'log2022',gdpRange, bbox_centralEur )
p_gdp_pc_adm1_CE <- myPlot_sfAbs(sf_gdp_adm1_log10,'log2022',gdpRange, bbox_centralEur )
p_gdp_pc_adm2_CE <- myPlot_sfAbs(sf_gdp_adm2_log10,'log2022',gdpRange, bbox_centralEur )

p_gdp_bbox <- tmap_arrange(p_gdp_pc_adm0_IND,  p_gdp_pc_adm0_BRA, p_gdp_pc_adm0_CE,
                           p_gdp_pc_adm1_IND,p_gdp_pc_adm1_BRA, p_gdp_pc_adm1_CE, 
                           p_gdp_pc_adm2_IND, p_gdp_pc_adm2_BRA, p_gdp_pc_adm2_CE,
                           ncol = 3)


tmap_save(p_gdp_bbox,filename = paste0('figures/figX_gdp_adm0_adm1_adm2_2022_bbox',Sys.Date(),'.pdf'),
          width = 180, height=200, units='mm')



