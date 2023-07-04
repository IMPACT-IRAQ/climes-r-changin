rm(list = ls())
library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

current_year = 2023

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
################################ Monthly PRECIPITATION MEAN ##########################
previous_year <- "2021"

historical_mean <- chirps_tidy %>% 
  filter(year %in% 1981:previous_year) %>% 
  group_by(month) %>% 
  summarise(stat = "mean")

historical_mean_renamed <- historical_mean %>% select (mean_his_precipitation = "precipitation_mean")

current_year_mean <- chirps_tidy %>% 
  filter(
    year %in% c(2023),
    month > 1) %>% 
  group_by(month, year) %>% 
  summarise(stat = "mean")

current_year_mean_renamed <- current_year_mean %>% select(mean_current_precipitation = "precipitation_mean")
################################ JOIN HISTORICAL AND CURRENT PRECIPITATION ########################
precipitation_recent_and_historical<- inner_join(x = current_year_mean_renamed,
                                      y = historical_mean_renamed,
                                      by = "month")

precipitation_recent_and_historical_ee <- precipitation_recent_and_historical %>%  
  as_ee()

precipitaion_defict <- precipitation_recent_and_historical_ee$map(
  function(img){
    precipitation_defict <- img$expression(
      "float(current_year_mean_renamed - historical_mean_renamed)",
      opt_map= list(current_year_mean_renamed= img$select("mean_current_precipitation"),
                    historical_mean_renamed= img$select("mean_his_precipitation")
      )
    )$rename("precipitation_defict")
    img$select("mean_current_precipitation","mean_his_precipitation")$addBands(precipitation_defict)
  }
) %>% as_tidyee()

precipitaion_defict_processed <- precipitaion_defict %>% 
  filter(year >= previous_year) %>% 
  select("precipitation_defict")

precipitation_defict_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:21823

all_tier <- c("first_tier","second_tier","third_tier")


for ( i in all_tier){

grid_filter <- grid[get(i),]
# 1000o is working 
precipitation_defict_final[[i]] <- precipitaion_defict_processed %>%  
  ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 5500,via = "drive")
}

precipitation_defic_bind <- do.call("bind_rows",precipitation_defict_final)
precipitation_defic_bind <- precipitation_defic_bind %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date,label = T,abbr = F),
    parameter = case_when(
      parameter == "precipitation_defict" ~ "Precipitation deficit",
      TRUE ~ parameter))

write.csv(precipitation_defic_bind,"gee_data_raw/20kmsqdata/precipitation_deficit_feb_may_2023.csv")

# df %>% 
#   mutate(value_new = case_when(
#     value %in% percent_cont | value %in% percent_cat ~ paste(value, sign),
#     TRUE ~ value)
