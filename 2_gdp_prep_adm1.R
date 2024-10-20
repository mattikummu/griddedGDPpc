### processing subnational data

# code for subnational GDP per capita dataset
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)


library(sf)
library(terra)
library(Rfast)
library(data.table)
library(zoo)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1. load general data -----

# 1.1 cntry info

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
  select(-cntry_code)

# 1.2 subnational metadata

cntryList_hist <-  read_excel('data_in/subnational_gdp_revised.xlsx', sheet = 'subnational', skip = 0) %>% 
  distinct(ISO) %>% 
  rename(iso3 = ISO) %>% 
  # filter out countries for which we use different GIS unit than in previous version
  filter(!iso3 %in% (updGIS %>% filter(!GIS == 'old') %>% select(iso3))$iso3)


#### 2. process DOSE v2 data  ----

# https://www.nature.com/articles/s41597-023-02323-8

# 2.1 create function to process the data

myFunDose <- function(iso3_name = 'KAZ'){ 
  
  iso3_gdp_pc <- read_csv('data_in/DOSE_V2.csv') %>% 
    select(region, GID_0, GID_1, year, grp_pc_usd) %>% 
    filter(GID_0 == !!iso3_name) %>% 
    pivot_wider(names_from = 'year', values_from = 'grp_pc_usd') %>% 
    arrange(region)
  
  write_csv(iso3_gdp_pc, paste0('data_in/dose_v2_cntry/',iso3_name,'_gdp_pc.csv'))
  
  return(iso3_gdp_pc)
}

# 2.2 apply function to different countries

KAZ_gdp_pc <- myFunDose('KAZ')

EGY_gdp_pc <- myFunDose('EGY')

# source: https://mped.gov.eg/Governorate?lang=en
# unit: One thousand EGP
egy_tot_gdp <- read_csv('data_in/egy GDP by Governorate.csv') %>% 
  select(Governorate, Year, `Total Governorate GDP`) %>% 
  mutate(Year = substr(Year, start=6, stop =9)) %>% 
  arrange(Governorate, Year) %>% 
  filter(!grepl('Total', Governorate))

unique_gpd <- unique(egy_tot_gdp$Governorate)

columnsToAdd <- c(paste0(2007:2016),paste0(2018:2020))

egy_pop <-  read_excel('data_in/egy_pop.xlsx', sheet = 'Sheet2', skip = 0) %>% 
  tibble::add_column(!!!set_names(as.list(rep(NA, length(columnsToAdd))),nm=columnsToAdd)) %>% 
  select(-Level) %>% select(-'1996') %>% 
  pivot_longer(-Name, names_to = 'Year', values_to = 'pop') %>% 
  #mutate(year = as.numeric(year)) %>% 
  arrange(Name, Year) %>% 
  # interpolate
  group_by(Name) %>% 
  #https://stackoverflow.com/questions/70155104/interpolate-na-values-when-column-ends-on-na
  mutate(pop = na.approx(pop, maxgap = Inf, rule = 2)) %>% 
  ungroup() %>% 
  mutate(Governorate = str_extract(Name, "\\[(.*?)\\]") %>% 
           str_replace_all("\\[|\\]", "")) %>% 
  arrange(Governorate) %>% 
  mutate(Governorate = recode(Governorate, 'Assuan' = 'Aswan', 
                              'Ismaïlia' = 'Ismailia', 
                              'Kafr el-Sheikh' = 'Kafr ElSheikh', 
                              'Matrouh' = 'Matruh',
                              'Monufia' = 'Menoufia'))

egy_gdp_pc <- egy_tot_gdp %>% 
  left_join(egy_pop) %>% 
  select(-Name) %>% 
  mutate(gdp_pc = (`Total Governorate GDP` * 10^3) / pop )

egy_gdp_pc_wider <- egy_gdp_pc %>% 
  select(-`Total Governorate GDP`, -pop) %>% 
  mutate(gdp_pc = round(gdp_pc) ) %>% 
  pivot_wider(names_from = Year, values_from = gdp_pc)  


unique_pop <- unique(egy_pop$Governorate)

dplyr::all_equal(as.data.frame(unique_gpd),as.data.frame(unique_pop))

BIH_gdp_pc <- myFunDose('BIH')



IRN_gdp_pc <-  myFunDose('IRN')

MNG_gdp_pc <-  myFunDose('MNG')

NGA_gdp_pc <-  myFunDose('NGA')

NGA_gdp_pc <-  myFunDose('PAK')

NGA_gdp_pc <-  myFunDose('PRY') # income, not GDP per capita

NGA_gdp_pc <-  myFunDose('ZAF')


## the data were then manually entered to the xls



### 3. process different datasets  ----

#  updated GIS

updGIS <- read_excel('data_in/updates_feb2024.xlsx', sheet = 'meta', skip = 2)

# checkData <- read_excel('data_in/updates_feb2024.xlsx', sheet = 'check', skip = 0)

cntryList_data <-  read_excel('data_in/subnational_gdp_revised.xlsx', sheet = 'subnational', skip = 0) %>% 
  distinct(ISO) %>% 
  rename(iso3 = ISO) 

# 3.1 historical data from earlier publication of Kummu et al-----

adm1_hist <- read_excel('data_in/subnational_gdp_revised.xlsx', sheet = 'subnational', skip = 0) %>% 
  as_tibble() %>% 
  # # in GIS data, 1000 was added to OBJECTID
  # mutate(OBJECTID = 1000+OBJECTID) %>% 
  rename(iso3 = ISO) %>% 
  rename(Subnat = Region) %>% 
  rename(RegID = OBJECTID) %>% 
  left_join(cntryID,by='iso3') %>% 
  dplyr::filter(iso3 %in% cntryList_hist$iso3) %>% 
  arrange(iso3) %>% 
  select(-c(Country.x, Country.y, cntry_id, iso2, paste0(1950:1984)))

# names(subnat_data) <- c('iso3','RegID','Subnat',as.character(1991:2021))

id_subnat <- unique(adm1_hist$iso3)

gis_hist <- read_sf( 'data_gis/reg_gdp_regions.gpkg' ) %>% 
  # st_drop_geometry() %>% 
  #select(ISO,CNTRYNAMEE,DHSREGEN,REG_ID) %>% 
  rename(RegID = OBJECTID) %>% 
  select(-Country) %>% 
  left_join(adm1_hist[,1:3],by='RegID') %>% 
  left_join(cntryID,by='iso3') %>% 
  select(iso3,cntry_id,RegID, Country) %>% 
  dplyr::filter(iso3 %in% id_subnat) %>% 
  arrange(iso3, RegID)

subnat_gis_data <- gis_hist %>% 
  left_join(adm1_hist,by=c('iso3','RegID')) %>% 
  #st_drop_geometry() %>% 
  arrange(iso3,Subnat) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_id)*1000+rowNmbr) %>% 
  select(-rowNmbr) %>%
  select(Country,iso3,cntry_id,Subnat,GID_nmbr,as.character(1988:2021))

subnat_gis_data_hist <- subnat_gis_data
subnat_data_hist <- adm1_hist



# 3.2 GADM ----

cntryList_GADM <-  updGIS %>% 
  # filter out countries for which we use different GIS unit than in previous version
  filter(GIS == 'GADM')


# read data 

subnat_data <- read.xlsx('data_in/gadm_level1_gdp_pc.xlsx', sheet='gadm_level1_meta', startRow = 1) %>% 
  as_tibble() %>% 
  rename(iso3=GID_0) %>% 
  # select(-GID_1) %>% 
  # rename(GID_1 = GID_1_combined) %>% 
  dplyr::filter(iso3 %in% cntryList_GADM$iso3) %>% 
  rename(Subnat = NAME_1) %>% 
  rename(Country = NAME_0) %>% 
  select('iso3','GID_1','Subnat',as.character(1988:2021)) %>% 
  arrange(iso3)




# all data columns to numeric
cols.num <- c(as.character(1988:2021))
subnat_data[cols.num] <- sapply(subnat_data[cols.num],as.numeric)

id_subnat <- unique(subnat_data$iso3)

gis_data_GADM <- read_sf( '/Users/mkummu/R/GIS_data_common/gadm_410-levels.gpkg' ,  layer = 'ADM_1') %>% 
  #st_drop_geometry() %>% 
  select(GID_0,COUNTRY,NAME_1,GID_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(COUNTRY,iso3,cntry_id,GID_1)

# some of the adm1 levels are divided to those that are officially in a country and those that are 
# on conflict zones (between CHN, IND and PAK)
# let's get those from the older version of the GADM
# below is also code (commented out now) for another approach where we unite them with those that we have data for


adm1_gadm_old <- read_sf('/Users/mkummu/R/migration_data_bee/data_in/gadm_lev1.gpkg') %>% 
  #st_drop_geometry() %>% 
  rename(iso3 = GID_0) %>% 
  rename(COUNTRY = NAME_0) %>% 
  rename(Subnat = NAME_1) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(COUNTRY,iso3,cntry_id,GID_1) %>% 
  filter(iso3 %in% c('CHN', 'PAK', 'IND'))

adm1_polyg_comb <- gis_data_GADM %>% 
  filter(!iso3 %in% c('CHN', 'PAK', 'IND')) %>% 
  bind_rows(adm1_gadm_old) 



gis_data_GADM_filt <- adm1_polyg_comb %>% 
  dplyr::filter(iso3 %in% cntryList_GADM$iso3) %>% 
  arrange(iso3) %>% 
  mutate(cntry_id = ifelse(is.na(cntry_id), mean_cntry_id, cntry_id))

test_IND <- gis_data_GADM_filt %>% 
  filter(iso3 == 'IND')

id_gis <- unique(gis_data_GADM_filt$iso3)




subnat_gis_data <- gis_data_GADM_filt %>% 
  left_join(subnat_data,by=c('iso3','GID_1')) %>% 
  #st_drop_geometry() %>% 
  arrange(iso3,Subnat) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_id)*1000+rowNmbr) %>% 
  select(-rowNmbr) %>%
  rename(Country = COUNTRY) %>% 
  select(Country,iso3,cntry_id,Subnat,GID_1,GID_nmbr,as.character(1988:2021))

subnat_gis_data_GADM <- subnat_gis_data
subnat_data_GADM <- subnat_data

temp_subnat_gis_data_GADM <- subnat_gis_data_GADM %>% 
  st_drop_geometry()



### 3.3 EUROSTAT NUTS2 ----


cntryList_NUTS2 <-  updGIS %>% 
  # filter out countries for which we use different GIS unit than in previous version
  filter(GIS == 'nuts2')


subnat_data <- read.xlsx('data_in/nuts2_gdp_pps.xlsx', sheet='Sheet 1', startRow = 8) %>% 
  as_tibble() %>% 
  rename(iso2=code) %>% 
  
  left_join(cntryID) %>% 
  dplyr::filter(iso3 %in% cntryList_NUTS2$iso3) %>% 
  select(iso3, 'GEO.(Codes)',as.character(2010:2021)) %>% 
  mutate(across(as.character(2010:2021), as.numeric ))



subnat_gis_data <-  read_sf('/Users/mkummu/R/GIS_data_common/nuts2021/NUTS_RG_01M_2021_4326_LEVL_2.shp') %>% 
  #st_drop_geometry() %>% 
  filter(LEVL_CODE == 2) %>% 
  select(CNTR_CODE,NUTS_ID, NAME_LATN) %>% 
  rename(iso2 = CNTR_CODE) %>% 
  left_join(subnat_data, by=c("NUTS_ID" = "GEO.(Codes)")) %>% 
  left_join(cntryID %>% select(-iso3),by='iso2') %>% 
  dplyr::filter(iso3 %in% cntryList_NUTS2$iso3) %>% 
  
  rename(GID_1 = NUTS_ID) %>% 
  rename(Subnat = NAME_LATN) %>% 
  arrange(iso3,Subnat) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_id)*1000+rowNmbr) %>% 
  select(-rowNmbr) %>% 
  select(Country,iso3,cntry_id,Subnat,GID_1,GID_nmbr,everything()) %>% 
  select(-iso2)

#plot(subnat_gis_data)

subnat_gis_data_NUTS2 <- subnat_gis_data %>% 
  rename(geom = geometry)

subnat_gis_data_NUTS2_noGeom <- subnat_gis_data %>% 
  st_drop_geometry()
subnat_data_NUTS2 <- subnat_data




### 3.4 OECD ----

cntryList_OECD <-  updGIS %>% 
  # filter out countries for which we use different GIS unit than in previous version
  filter(GIS == 'OECD')

meta_OECD <- read.xlsx('/Users/mkummu/R/GIS_data_common/OECD Territorial grid and Regional typologies - March 2023.xlsx',
                       sheet='List of regions - 2023', startRow = 3) %>% 
  as_tibble() %>% 
  select(ISO3, REG_ID) %>% 
  filter(!ISO3 == REG_ID) %>%  # iso3 = AUT, reg_id = AUT -> confusion with reg2 with AUS
  mutate(reg2 = substr(REG_ID,1,2) ) %>% 
  rename(iso3 = ISO3) %>% 
  select(iso3, reg2) %>% 
  filter(!iso3 == "IRL_OLD") %>% 
  distinct()


# read data

subnat_data <- read.xlsx('data_in/oecd_gdp_PPP_feb2023.xlsx', sheet='subnat', startRow = 1) %>%
  as_tibble() %>% 
  rename(Subnat = Region) %>%
  select(c(REG_ID, Subnat, as.character(1990:2020))) %>%
  mutate(reg2 = substr(REG_ID,1,2) ) %>%
  left_join(meta_OECD,by='reg2') %>%
  left_join(cntryID,by='iso3') %>%
  dplyr::filter(iso3 %in% cntryList_OECD$iso3) %>%
  select(-c(iso2, reg2)) %>%
  select(Country, iso3, cntry_id, everything()) %>% 
  arrange(REG_ID) %>%
  rename(tl2_id = REG_ID) %>% 
  as_tibble()

#names(subnat_data) <- c('tl2_id','iso3',as.character(2001:2019))

id_subnat <- unique(subnat_data$iso3) %>% 
  sort()

gis_data <- read_sf( '/Users/mkummu/R/GIS_data_common/oecd_tl2.gpkg' ) %>%
  #st_drop_geometry() %>%
  select(tl2_id,iso3,name_en) %>%
  rename(Subnat = name_en) %>%
  left_join(cntryID,by='iso3') %>%
  select(iso3,tl2_id) %>%
  dplyr::filter(iso3 %in% cntryList_OECD$iso3) %>%
  arrange(iso3)

id_gis <- unique(gis_data$iso3) %>% sort

compare::compare(id_subnat,id_gis)

subnat_gis_data <- gis_data %>%
  left_join(subnat_data,by=c('tl2_id','iso3')) %>%
  #st_drop_geometry() %>%
  arrange(iso3,Subnat) %>%
  group_by(iso3) %>%
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>%
  mutate(GID_1 = paste0(iso3,".",rowNmbr,"_1")) %>%
  mutate(GID_nmbr = (1000+cntry_id)*1000+rowNmbr) %>%
  select(-rowNmbr)%>%
  select(Country,iso3,cntry_id,Subnat,GID_1,GID_nmbr,as.character(c(1990:2020)))

subnat_gis_data_OECD <- subnat_gis_data
temp_oecd <- subnat_gis_data_OECD %>% 
  st_drop_geometry()

subnat_data_OECD <- subnat_data



### 3.5 OECD level 3 ----

cntryList_OECD3 <-  updGIS %>% 
  # filter out countries for which we use different GIS unit than in previous version
  filter(GIS == 'oecd3')

meta_OECD3 <- read.xlsx('/Users/mkummu/R/GIS_data_common/OECD Territorial grid and Regional typologies - March 2023.xlsx',
                        sheet='List of regions - 2023', startRow = 3) %>% 
  as_tibble() %>% 
  filter(TL == 3) %>% 
  select(ISO3, REG_ID) %>% 
  filter(!ISO3 == REG_ID) %>%  # iso3 = AUT, reg_id = AUT -> confusion with reg2 with AUS
  #rename(reg3 = REG_ID ) %>% 
  rename(iso3 = ISO3) %>% 
  select(iso3, REG_ID) %>% 
  filter(!iso3 == "IRL_OLD") %>% 
  distinct()




# read data
sf_oecd_tl3 <- st_read('/Users/mkummu/R/migration_data_bee/data_in/OECD_TL3_2020_fixed_valid.gpkg')

sf_oecd_tl3_noGeom <- sf_oecd_tl3 %>% 
  st_drop_geometry()


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
  pivot_wider(names_from = Year, values_from = adm2_GDP)

write_csv(adm2_oecd_wide, "data_in/adm2_oecd_wide.csv")

### some gap filling in excel, and storing as excel file
## read it in

subnat_data <- read.xlsx('data_in/adm2_oecd_level3_wide.xlsx', sheet='adm2_oecd_wide', startRow = 1) %>%
  as_tibble() %>% 
  rename(Subnat = name_en) %>%
  select(c(REG_ID, Subnat, as.character(1990:2020))) %>%
  #mutate(reg2 = substr(REG_ID,1,2) ) %>%
  left_join(meta_OECD3,by='REG_ID') %>%
  left_join(cntryID,by='iso3') %>%
  dplyr::filter(iso3 %in% cntryList_OECD3$iso3) %>%
  #select(-c(iso2, reg2)) %>%
  select(Country, iso3, cntry_id,REG_ID, everything()) %>% 
  arrange(REG_ID) %>%
  rename(tl3_id = REG_ID) %>% 
  as_tibble()

#names(subnat_data) <- c('tl2_id','iso3',as.character(2001:2019))

id_subnat <- unique(subnat_data$iso3) %>% 
  sort()

gis_data <- read_sf('/Users/mkummu/R/migration_data_bee/data_in/OECD_TL3_2020_fixed_valid.gpkg')%>%
  #st_drop_geometry() %>%
  select(tl3_id,iso3,name_en) %>%
  rename(Subnat = name_en) %>%
  left_join(cntryID,by='iso3') %>%
  select(iso3,tl3_id) %>%
  dplyr::filter(iso3 %in% cntryList_OECD3$iso3) %>%
  arrange(iso3, tl3_id)

id_gis <- unique(gis_data$iso3) %>% sort

compare::compare(id_subnat,id_gis)

subnat_gis_data <- gis_data %>%
  left_join(subnat_data,by=c('tl3_id','iso3')) %>%
  #st_drop_geometry() %>%
  arrange(iso3,Subnat) %>%
  group_by(iso3) %>%
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>%
  mutate(GID_1 = paste0(iso3,".",rowNmbr,"_1")) %>%
  mutate(GID_nmbr = (1000+cntry_id)*1000+rowNmbr) %>%
  select(-rowNmbr)%>%
  select(Country,iso3,cntry_id,Subnat,GID_1,GID_nmbr,as.character(c(1990:2020)))

subnat_gis_data_OECD3 <- subnat_gis_data
temp_oecd3 <- subnat_gis_data_OECD3 %>% 
  st_drop_geometry()

subnat_data_OECD3 <- subnat_data


### admin areas for which no data

temp_oecd3_noData <- temp_oecd3 %>% 
  filter(is.na(cntry_id)) %>% 
  select(iso3, GID_1)

write_csv(temp_oecd3_noData, "data_in/oecd3_noData.csv")


# 3.6 individual countries (GIS layer done within this article) -----

# 3.6.1 bahama


cntryList_BHS <-  updGIS %>% 
  # filter out countries for which we use different GIS unit than in previous version
  filter(GIS == 'own')


subnat_data_BHS <- read.xlsx('data_in/BHS_adm1.xlsx', sheet='Sheet1', startRow = 1) %>% 
  as_tibble() %>% 
  rename(iso3=GID_0) %>% 
  rename(Subnat = region) %>% 
  rename(Country = country) %>% 
  select('iso3','GID_1','Subnat',as.character(2015:2019)) 



gis_data_BHS <- read_sf( '/Users/mkummu/R/subnat_gdp_2023/data_gis/DOSE_shapefiles.gpkg' ) %>% 
  #st_drop_geometry() %>% 
  filter(GID_0 == 'BHS') %>% 
  select(GID_0,NAME_0,NAME_1,GID_1) %>% 
  rename(COUNTRY = NAME_0) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(COUNTRY,iso3,cntry_id,GID_1)



subnat_gis_data <- gis_data_BHS %>% 
  left_join(subnat_data_BHS,by=c('iso3','GID_1')) %>% 
  #st_drop_geometry() %>% 
  arrange(iso3,Subnat) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_id)*1000+rowNmbr) %>% 
  select(-rowNmbr) %>%
  rename(Country = COUNTRY) %>% 
  select(Country,iso3,cntry_id,Subnat,GID_1,GID_nmbr,as.character(2015:2019))

subnat_gis_data_BHS <- subnat_gis_data
subnat_data_BHS <- subnat_data



# write.xlsx(subnat_data_OECD,"data_raw/oecd_wide.xlsx")


##### 4. Combine all gis data ------


subnat_gis_combined <- bind_rows(subnat_gis_data_hist,
                                 subnat_gis_data_NUTS2,
                                 subnat_gis_data_OECD,
                                 subnat_gis_data_OECD3,
                                 subnat_gis_data_GADM, 
                                 subnat_gis_data_BHS) %>% 
  select(Country,iso3,cntry_id,Subnat,GID_1,GID_nmbr,as.character(1989:2021)) %>% 
  arrange(iso3, GID_nmbr, .keep_all = T)

testGIS <- subnat_gis_combined %>% 
  st_drop_geometry()

st_write(subnat_gis_combined, "results/gisData_GDP_pc_combined_feb2024.gpkg",delete_dsn = TRUE)

write_csv(testGIS, "results/subnat_gis_combined_feb2024.csv")


### 5.  Calculate adm0 level data based on adm1 data ----

v_subnat_gis_combined <- vect("results/gisData_GDP_pc_combined_feb2024.gpkg")

polyg_subnat_gis_combined <- read_sf("results/gisData_GDP_pc_combined_feb2024.gpkg")

sf_subnat_gis_combined <- polyg_subnat_gis_combined %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate('2022' = as.numeric(NA))


sf_subnat_gis_combined_long <- sf_subnat_gis_combined %>% 
  pivot_longer(-c(Country, iso3, cntry_id, Subnat, GID_1, GID_nmbr), 
               names_to = 'year', values_to = 'gdp')



ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)



if (file.exists('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')){
  # load it
  r_pop <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')
} else { # create it
  
  r_popCount_GP <- rast("/Users/mkummu/R/GIS_data_common/GHS_POP/r_pop_GHS_1990_2022_5arcmin.tif")
  
  writeRaster(r_popCount_GP,'data_gis/r_pop_GHS_1990_2022_5arcmin.tif', overwrite=TRUE)
  
  r_pop <- r_popCount_GP
  
}

# population for each subnat

v_subnatPop <- exactextractr::exact_extract(r_pop, polyg_subnat_gis_combined, fun = 'sum')

v_subnatPop_comb <- sf_subnat_gis_combined %>% 
  st_drop_geometry() %>% 
  select(GID_nmbr, cntry_id) %>% 
  bind_cols(v_subnatPop) %>% 
  #select(-ID) %>% 
  set_names(c('GID_nmbr', 'cntry_id', paste0('pop',1990:2022))) %>% 
  # mutate(pop2021 = pop2020) %>%
  mutate(pop1989 = pop1990) %>% 
  # mutate(pop2022 = pop2020) %>% 
  select(GID_nmbr, cntry_id, paste0('pop',1989:2022))

# gdp per capita

sf_gdp <- sf_subnat_gis_combined %>% 
  select(GID_nmbr, as.character(1989:2022)) %>% 
  #mutate('2022' = NA) %>% 
  as_tibble()

# weight with population

sf_gdp_pop_weighted <- sf_gdp * v_subnatPop_comb %>% select(-cntry_id) 


sf_gdp_pop_weighted <-  sf_gdp_pop_weighted %>% 
  as_tibble() %>% 
  select(-GID_nmbr) %>% 
  bind_cols(sf_subnat_gis_combined[,3]) %>% 
  select(cntry_id, as.character(1989:2022))

sf_adm0_gdp_pop_weighted <- sf_gdp_pop_weighted %>% 
  group_by(cntry_id) %>% 
  summarise(across(everything(), list(sum)))

sf_adm0_pop <- v_subnatPop_comb %>% 
  select(-GID_nmbr) %>% 
  group_by(cntry_id) %>% 
  summarise(across(everything(), list(sum)))

sf_adm0_gdp <- sf_adm0_gdp_pop_weighted / sf_adm0_pop 

sf_adm0_gdp  <- sf_adm0_gdp %>% 
  as_tibble() %>% 
  select(-cntry_id) %>% 
  bind_cols(sf_adm0_pop[,1]) %>% 
  left_join(cntryID %>% select(cntry_id, iso3)) %>% 
  select(cntry_id, iso3, everything()) %>% 
  set_names('cntry_id', 'iso3', as.character(1989:2022))

sf_adm0_gdp_long <- sf_adm0_gdp %>% 
  pivot_longer(-c(cntry_id, iso3), names_to = 'year', values_to = 'gdp')

write_csv(sf_adm0_gdp_long, "results/sf_adm0_gdp_long_feb2024.csv")
write_csv(sf_subnat_gis_combined_long, "results/sf_subnat_gis_combined_long_feb2024.csv")


### 6. interpolate adm1 dataset ----------------

# load data

sf_adm0_gdp_long <-  read_csv("results/sf_adm0_gdp_long_feb2024.csv")
sf_subnat_gis_combined_long <-  read_csv("results/sf_subnat_gis_combined_long_feb2024.csv")

# laod function that calculates subnational GDP ratio and interpolates between missing years
# for tail and head missing years, the latest value is used

source('functions/f_interp_adm1.R')

# apply function

adm1_gdp_ratio_interp <- f_interp_adm1('gdp') 

# write results

write_csv(adm1_gdp_ratio_interp, 'results/adm1_gdp_ratio_interp_feb2024.csv')




