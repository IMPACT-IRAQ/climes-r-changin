rm(list = ls())
library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

# current_year = 2022

# read cluster ------------------------------------------------------------
grid <- st_read("01_inputs/01_hexagon/cluster_20_km.shp") %>% st_transform(4326)
# ee initiating  ----------------------------------------------------------
ee_Initialize(user = "cody", drive = T, gcs = T)
############################# READING CHIRPS DATA ############################################
CHIRPS <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$select("precipitation")$
  map(ee_utils_pyfunc(
    function(x){x$copyProperties(x,x$propertyNames())}
  )) # read CHIRPS with property info

chirps_tidy<- as_tidyee(CHIRPS) # make tidygee object

current_year_sum <- chirps_tidy %>% 
  filter(year > 2022,
         month > 1) %>% 
  group_by(year, month) %>% 
  summarise(stat = "sum")

current_year_sum_renamed <- current_year_sum %>% select(sum_current_precipitation = "precipitation_sum")

precipitation_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:21823

all_tier <- c("first_tier","second_tier","third_tier")
for ( i in all_tier){
  
  grid_filter <- grid[get(i),]
  precipitation_final[[i]] <- current_year_sum_renamed  %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "sum",scale = 5500,via = "drive")
}

precipitation_defic_bind <- do.call("bind_rows",precipitation_final)
precipitation_defic_bind <- precipitation_defic_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F)
)

write.csv(precipitation_defic_bind,"gee_data_raw/20kmsqdata/precipitation_feb_may_2023.csv")
