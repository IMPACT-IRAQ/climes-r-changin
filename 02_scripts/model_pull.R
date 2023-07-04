# PURPOSE: Pull GEE data by district for use in the UoM Water Scarcity Model
# AUTHOR: Cody Adelson | Data Specialist
# DATE CREATED: May 28, 2023
# NOTES: 

rm(list = ls())
library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

ee_Initialize(user = "cody", drive = T, gcs = T)

admin_boundary_path <-  "01_inputs/03_admin_boundary/"
admin2_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm2_cso_20190603.shp")) %>% 
  filter(ADM1_EN == "Kirkuk") %>% 
  select(ADM2_EN, ADM1_EN, geometry) %>% 
  mutate(FID = row_number()) %>% 
  st_transform(4326)

first_tier <- 1
second_tier <- 2
third_tier <- 3
fourth_tier <- 4

all_tier <- c("first_tier", "second_tier", "third_tier", "fourth_tier")

############# Precipitation ############# 
CHIRPS <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$select("precipitation")$
  map(ee_utils_pyfunc(
    function(x){x$copyProperties(x,x$propertyNames())}
  )) # read CHIRPS with property info

chirps_tidy<- as_tidyee(CHIRPS) # make tidygee object

current_year_mean <- chirps_tidy %>%
  filter(year>=1990) %>% 
  group_by(year, month) %>% 
  summarise(stat = "mean")

current_year_mean_renamed <- current_year_mean %>% select(mean_current_precipitation = "precipitation_mean")

precipitation_final <- list()

for ( i in all_tier){
  grid_filter <- admin2_boundary[get(i),]
  precipitation_final[[i]] <- current_year_mean_renamed  %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 5500,via = "drive")}

precipitation_bind_m1990 <- do.call("bind_rows",precipitation_final)
precipitation_bind_m1990 <- precipitation_bind_m1990 %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date,label = T,abbr = F))

precip_combined <- precipitation_bind_m1990 %>% 
  bind_rows(precipitation_bind_l1990) %>% 
  mutate(month_days = lubridate::days_in_month(date),
         value = value*month_days)
  

write_csv(precip_combined, "gee_data_raw/model_data/precipitation_baghdad.csv")

############# Temperature ############# 
#Read in image collection and temperature data from modis satellite
modis_link <- "MODIS/061/MOD11A1"
modisIC <- ee$ImageCollection(modis_link)

modis_temperature <- modisIC$
  select("LST_Day_1km")$
  map(
    ee_utils_pyfunc(
      function(x){x$
          multiply(0.02)$subtract(273.15)$
          copyProperties(x,x$propertyNames())}))

modis_temperature_tidy <- as_tidyee(modis_temperature) #as tidy object

temp_recent_monthly <- modis_temperature_tidy %>%                                         # RECENT 
  filter(year>=2010) %>% 
  group_by(year,month) %>%  
  summarise(stat="mean") %>% 
  dplyr::select(temp="LST_Day_1km_mean")

temp_final <- list()

for ( i in all_tier){
  
  grid_filter <- admin2_boundary[get(i),]
  temp_final[[i]] <- temp_recent_monthly %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}

temp_final_bind_m2010 <- do.call("bind_rows",temp_final)
temp_final_bind_m2010 <- temp_final_bind_m2010 %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date,label = T,abbr = F),
    parameter = case_when(
      parameter == "temp" ~ "Surface Temperature"))

temp_combined <- temp_final_bind_m2010 %>% 
  bind_rows(temp_final_bind_l2010)

write_csv(temp_combined, "gee_data_raw/model_data/temperature_baghdad.csv")

############# Evapotranspiration ############# 

modis_link <- "MODIS/006/MOD16A2"
modisIC <- ee$ImageCollection(modis_link)

modis_evapotranspiration <- ee$ImageCollection(modis_link)$select("ET")$
  map(ee_utils_pyfunc(
    function(x){x$copyProperties(x,x$propertyNames())}
  )) # read CHIRPS with property info

evapotranspiration_tidy<- as_tidyee(modis_evapotranspiration) # make tidygee object

evapotranspiration_df <- evapotranspiration_tidy %>% 
  #filter(year >= 2022) %>% 
  group_by(year, month) %>% 
  summarise(stat = "mean")

evapotranspiration_final <- list()

for ( i in all_tier){
  
  grid_filter <- admin2_boundary[get(i),]
  evapotranspiration_final[[i]] <- evapotranspiration_df %>%  
    ee_extract_tidy(y = grid_filter, sf = T,stat = "mean", scale = 250, via = "drive")
}

evapotranspiration_bind <- do.call("bind_rows", evapotranspiration_final)
evapotranspiration_bind <- evapotranspiration_bind %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date,label = T, abbr = F))

write_csv(evapotranspiration_bind, "gee_data_raw/model_data/evapotranspiration_kirkuk.csv")
