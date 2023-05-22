# PURPOSE: Pull data for Al-rashidia weather station for Mosul model validation
# AUTHOR: Cody Adelson | Data Specialist
# DATE CREATED: May 2, 2023
# NOTES: 

rm(list = ls())
library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

ee_Initialize(user = "cody", drive = T, gcs = T)

#grid <- st_read("01_inputs/01_hexagon/cluster_20_km.shp") %>% st_transform(4326)

point <- st_point(c(43.11085980746673, 36.409024392458875))
point_df <- data.frame(id = 1, geometry = st_as_text(point))
grid <- st_as_sf(point_df, wkt = "geometry")
grid <- st_set_crs(grid, 4326)


#Precipitation
CHIRPS <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$select("precipitation")$
  map(ee_utils_pyfunc(
    function(x){x$copyProperties(x,x$propertyNames())}
  )) # read CHIRPS with property info

chirps_tidy<- as_tidyee(CHIRPS) # make tidygee object

current_year_sum <- chirps_tidy %>% 
  filter(year >= 2022) %>% 
  group_by(year, month) %>% 
  summarise(stat = "sum")

current_year_sum_renamed <- current_year_sum %>% select(sum_current_precipitation = "precipitation_sum")

precipitation_final <- list()

precipitation_final <- current_year_sum_renamed %>% 
  ee_extract_tidy(y = grid, sf = T, stat = "sum", scale = 5500, via = "drive")

precipitation_defic_bind <- do.call("bind_rows",precipitation_final)
precipitation_defic_bind <- precipitation_defic_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F)
)

write.csv(precipitation_defic_bind,"gee_data_raw/model_validation/precipitation_validation.csv")

# Temperature

modis_link <- "MODIS/061/MOD11A1"
modisIC <- ee$ImageCollection(modis_link)

# Select band including daytime land surface temperature, change to celcius
modis_temperature <- modisIC$
  select("LST_Day_1km")$
  map(
    ee_utils_pyfunc(
      function(x){x$
          multiply(0.02)$subtract(273.15)$
          copyProperties(x,x$propertyNames())
      }
    )
  )

modis_temperature_tidy <- as_tidyee(modis_temperature) #as tidy object

temp_recent_monthly <- modis_temperature_tidy %>%                                         # RECENT 
  filter(year>=2022) %>% 
  group_by(year, month) %>%  
  summarise(stat="mean") %>% 
  dplyr::select(temp="LST_Day_1km_mean")

temp_final <- list()

temp_final <- temp_recent_monthly %>% 
  ee_extract_tidy(y = grid, sf = T, stat = "mean", scale = 250, via = "drive")

temp_final_bind <- do.call("bind_rows",temp_final)

temp_final_bind <- temp_final_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F),
  parameter = case_when(
    parameter == "temp" ~ "Surface Temperature")
)

write.csv(temp_final_bind,"gee_data_raw/model_validation/temperature_validation.csv")

# Evapotranspiration

modis_link <- "MODIS/006/MOD16A2"
modisIC <- ee$ImageCollection(modis_link)

modis_evapotranspiration <- ee$ImageCollection(modis_link)$select("ET")$
  map(ee_utils_pyfunc(
    function(x){x$copyProperties(x,x$propertyNames())}
  )) # read CHIRPS with property info

evapotranspiration_tidy<- as_tidyee(modis_evapotranspiration) # make tidygee object

evapotranspiration_current_year <- evapotranspiration_tidy %>% 
  filter(year >= 2022) %>% 
  group_by(year, month) %>% 
  summarise(stat = "sum")

evapotranspiration_final <- list()

evapotranspiration_final <- evapotranspiration_current_year %>% 
  ee_extract_tidy(y = grid, sf = T, stat = "sum", scale = 10000, via = "drive")

evapotranspiration_bind <- do.call("bind_rows", evapotranspiration_final)
evapotranspiration_bind <- evapotranspiration_bind %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date,label = T, abbr = F))

write.csv(evapotranspiration_bind,"gee_data_raw/model_validation/evapotranspiration_validation.csv")
