rm(list = ls())

#use_python("04_py_env/Scripts/python.exe")
library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

# read cluster ------------------------------------------------------------

grid <- st_read("01_inputs/01_hexagon/cluster_20_km.shp") %>% st_transform(4326)

# initiate gee -----------------------------------------------------------
ee_Initialize(user = "cody", drive = T, gcs = T)
# Temperature --------------------------------------------------------------------
############################## READ modis data ###########################
#Read in image collection and temperature data from modis satellite
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
############################################################################
modis_temperature_tidy <- as_tidyee(modis_temperature) #as tidy object
############################ ####Monthly TEMP MEAN ##########################
temp_recent_monthly <- modis_temperature_tidy %>%                                         # RECENT 
  filter(year>=2023,
         month == 3) %>% 
  group_by(year,month) %>%  
  summarise(stat="mean") %>% 
  dplyr::select(temp="LST_Day_1km_mean")

####################################################################

temp_final <- list()

#Final figure  determined from grid

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:21823

all_tier <- c("first_tier","second_tier","third_tier")

for ( i in all_tier){
  
  grid_filter <- grid[get(i),]
  temp_final[[i]] <- temp_recent_monthly %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}



temp_final_bind <- do.call("bind_rows",temp_final)
temp_final_bind <- temp_final_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F),
  parameter = case_when(
    parameter == "temp" ~ "Surface Temperature")
)

write.csv(temp_final_bind,"gee_data_raw/20kmsqdata/temperature_march_2023.csv")
