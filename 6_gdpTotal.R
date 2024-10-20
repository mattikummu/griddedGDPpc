

### calculate the total GDP

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

library(sf)
library(terra)
library(openxlsx) #

library(broom)
library(tidyr)
library(tidyverse)
library(dplyr) 

library(tmap)


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1. load data -----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(iso3, .keep_all = T)



## 1.1 population data

# source: https://ghsl.jrc.ec.europa.eu/download.php?ds=pop

# 5 arcmin
r_popCount1990_2022 <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')


# 30 arcsec

path_ghgPop = '/Users/mkummu/R/GIS_data_common/GHS_POP/'

file.ghsPop <- list.files(path=path_ghgPop, 
                        pattern='V1_0.tif', recursive = T) %>% 
  as_tibble() %>% 
  filter(!grepl('.ovr', value))


r_popCount1990_2020_30arcsec <- rast(paste0(path_ghgPop,as.matrix(file.ghsPop)))

r_gdpPerCap <- rast("results/rast_adm2_gdp_perCapita_1990_2022.tif")


##### 2. calculate gdp tot -----

## 2.1 30 arcsec

r_gdpPerCap_30arcsec = resample(subset(r_gdpPerCap,c(1,6,11,16,21,26,31 )),subset(r_popCount1990_2020_30arcsec,1),method="bilinear")

gdpTot_2020_30arcsec <- subset(r_popCount1990_2020_30arcsec,2:8) * r_gdpPerCap_30arcsec
names(gdpTot_2020_30arcsec) <- c('gdp_1990','gdp_1995','gdp_2000','gdp_2005','gdp_2010','gdp_2015','gdp_2020')
terra::units(gdpTot_2020_30arcsec) <- 'GDP (PPP) in USD (2017 int. dollars)'

gdpTot_2020_30arcsec_round <- round(gdpTot_2020_30arcsec)

terra::units(gdpTot_2020_30arcsec_round) <- 'GDP (PPP) in USD (2017 int. dollars)'



# create folder if not exist

if (dir.exists('results_final/')) {
  
} else {
  dir.create('results_final/')  
}


writeRaster(x = gdpTot_2020_30arcsec_round,
            filename = "results_final/rast_gdpTot_1990_2020_30arcsec.tif", gdal="COMPRESS=LZW",overwrite=T)



## 2.2 5 arc-min

gdpTot_1990_2022_5arcmin <- terra::extend(r_popCount1990_2022, subset(r_gdpPerCap,31)) * r_gdpPerCap
names(gdpTot_1990_2022_5arcmin) <- paste0('gdp_tot_',1990:2022)

terra::units(gdpTot_1990_2022_5arcmin) <- 'GDP (PPP) in USD (2017 int. dollars)'

gdpTot_1990_2022_5arcmin_round <- round(gdpTot_1990_2022_5arcmin)

writeRaster(x = gdpTot_1990_2022_5arcmin_round,
            filename = "results_final/rast_gdpTot_1990_2022_5arcmin.tif", gdal="COMPRESS=LZW",overwrite=T)



## 2.3 30 arc-min

gdpTot_1990_2022_30arcmin <- terra::aggregate(x = gdpTot_1990_2022_5arcmin, fact = 6, fun="sum", na.rm=T)
names(gdpTot_1990_2022_30arcmin) <- paste0('gdp_tot_',1990:2022)

terra::units(gdpTot_1990_2022_30arcmin) <- 'GDP (PPP) in USD (2017 int. dollars)'

pop_1990_2022_30arcmin <- terra::aggregate(x = terra::extend(r_popCount1990_2022, subset(r_gdpPerCap,31)), 
                                           fact = 6, fun="sum", na.rm=T)

gdpPerCap_1990_2022_30arcmin <- gdpTot_1990_2022_30arcmin / pop_1990_2022_30arcmin

names(gdpPerCap_1990_2022_30arcmin) <- paste0('gdp_pc_',1990:2022)

writeRaster(x = gdpTot_1990_2022_30arcmin,
            filename = "results_final/rast_gdpTot_1990_2022_30arcmin.tif", gdal="COMPRESS=LZW",overwrite=T)

writeRaster(x = gdpPerCap_1990_2022_30arcmin,
            filename = "results_final/rast_gdpPerCap_1990_2020_30arcmin.tif", gdal="COMPRESS=LZW",overwrite=T)



#### 3 plot -----

gdpTot_1990_2022_5arcmin_round <-  rast( "results/rast_gdpTot_1990_2022_5arcmin.tif" )

cntry50 <- rnaturalearth::ne_download(scale=50, type="countries", category = "cultural") %>% 
  filter(!SOV_A3 == "ATA")

# 3.1 set colour map
gdp_pal <-  scico::scico(15, begin = 0.025, end = 0.975,direction = 1, palette = "lajolla")

# minAC <- quantile( ac_2020, .01, na.rm=T)
# maxAC <- quantile( adaptCapac$'ac_2020', .99, na.rm=T)



gdpTot_1990_2022_5arcmin_round_zero2NA <- subset(gdpTot_1990_2022_5arcmin_round,33) 
gdpTot_1990_2022_5arcmin_round_zero2NA[gdpTot_1990_2022_5arcmin_round_zero2NA < 1] = NA

# 3.2 get thresholds 

min_gdp <-  global(log10(gdpTot_1990_2022_5arcmin_round_zero2NA), quantile, probs=c(0.01), na.rm=TRUE)
max_gdp <-  global(log10(gdpTot_1990_2022_5arcmin_round_zero2NA), quantile, probs=c(0.99), na.rm=TRUE)

gdp_breaks <- seq( plyr::round_any( as.numeric(min_gdp),accuracy=0.5,f=floor ),
                plyr::round_any( as.numeric(max_gdp),accuracy=0.5,f=ceiling ) ,
                by= 0.25)

gdp_breaks_2 <- c(0,1,2,2^2,2^3,2^4,2^5,2^6,2^7,2^8,2^9,2^10,2^11, 2^12, 2^13)

# 3.3 mapping function
raster2map_v2 <- function(r_in,shape_in, colPalette, plotTitle) {
  
  
  tmapMap <- tm_shape(r_in) +
    tm_raster(palette = colPalette,
              #breaks = fishing.breaks,
              title = plotTitle,
              #n = 20,
              style = "fixed",
              breaks = gdp_breaks_2,
              colorNA = NULL,
              legend.is.portrait = FALSE) +
    tm_shape(shape_in,projection = "robin") +
    tm_borders(col="grey75",lwd = 0.3)+
    tm_layout(legend.bg.color = TRUE,
              legend.outside.position = "bottom",
              legend.outside = TRUE,
              frame = FALSE)
  
  
  return(tmapMap)
}


# 3.4 project raster

writeRaster(gdpTot_1990_2022_5arcmin_round_zero2NA, "results/rast_gdpTotal_2022.tif", overwrite=TRUE)

gdpTot_2022_5arcmin_round_zero2NA <-  rast("results/rast_gdpTotal_2022.tif")

crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

gdpTot_2022_5arcmin_round_zero2NA_robin <- terra::project(gdpTot_2022_5arcmin_round_zero2NA/10^6, crs)

p_gdp_total <- raster2map_v2(gdpTot_2022_5arcmin_round_zero2NA_robin,cntry50,gdp_pal,"GDP (PPP) million USD")


# 3.5 plot

if (dir.exists('figures/figGDPtotal/')) {
  
} else {
  dir.create('figures/figGDPtotal/')  
}

p_fig <- p_gdp_total + 
  tm_layout(legend.show=FALSE)

tmap_save(p_fig,filename = paste0('figures/figGDPtotal/fig_gdpTotal_millionUSD_log10_','.png'),width = 400, units='mm', dpi = 600)

tmap_save(p_gdp_total,filename = paste0('figures/figGDPtotal/fig_gdpTotal_millionUSD_log10_','.pdf'),width = 180, height=100, units='mm')


# 3.6 map close-ups

# 3.6.1 mapping function
raster2map_v2_bbox <- function(r_in,shape_in, colPalette, plotTitle,bboxUsed) {
  
  
  tmapMap <- tm_shape(r_in,  bbox= bboxUsed) +
    tm_raster(palette = colPalette,
              #breaks = fishing.breaks,
              title = plotTitle,
              #n = 20,
              style = "fixed",
              breaks = gdp_breaks_2,
              colorNA = NULL,
              legend.is.portrait = FALSE) +
    tm_shape(shape_in, bbox= bboxUsed) +
    tm_borders(col="white",lwd = 0.2)+
    tm_layout(legend.bg.color = TRUE,
              legend.outside.position = "bottom",
              legend.outside = TRUE,
              frame = FALSE)
  
  
  return(tmapMap)
}



sf_adm0 <- vect("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>%
  # simplify the shapefile
  #rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  terra::makeValid() %>% 
  st_as_sf() %>%
  tidyterra::filter(!iso_a3 == 'ATA')


# 3.6.2 bbox:es

bbox_india <- st_bbox(sf_adm0 %>% filter(iso_a3 %in% c("IND","THA" )))
bbox_brazil <- st_bbox(sf_adm0 %>% filter(iso_a3 %in% c("BRA","ECU" )))
bbox_centralEur <- st_bbox(sf_adm0 %>% filter(iso_a3 %in% c("DEU", "ITA", "AND", "POL") ))

p_gdp_IND <- raster2map_v2_bbox(crop(gdpTot_1990_2022_5arcmin_round_zero2NA/10^6, bbox_india),sf_adm0,gdp_pal,'GDP tota' ,bbox_india)
p_gdp_BRA <- raster2map_v2_bbox(crop(gdpTot_1990_2022_5arcmin_round_zero2NA/10^6, bbox_brazil),sf_adm0,gdp_pal,'GDP tota',bbox_brazil )
p_gdp_CE <- raster2map_v2_bbox(crop(gdpTot_1990_2022_5arcmin_round_zero2NA/10^6, bbox_centralEur),sf_adm0,gdp_pal,'GDP tota',bbox_centralEur )

# 3.6.3 map

layers <- list(p_gdp_IND,  p_gdp_BRA, p_gdp_CE)

nameLayers <- c('p_gdp_IND',  'p_gdp_BRA', 'p_gdp_CE')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/figGDPtotal/fig_',nameLayers[i],'.png'),width = 100, units='mm', dpi = 600)
  
}


p_gdp_bbox <- tmap_arrange(p_gdp_IND,  p_gdp_BRA, p_gdp_CE, ncol = 2)


tmap_save(p_gdp_bbox,filename = paste0('figures/figGDPtotal/fig_gdp_tot_2022_bbox',Sys.Date(),'.pdf'),
          width = 180, height=200, units='mm')



