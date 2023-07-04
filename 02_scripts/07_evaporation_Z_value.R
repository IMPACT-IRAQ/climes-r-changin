rm(list = ls())

library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(dplyr)
#library(tidyverse)

# read cluster ------------------------------------------------------------
grid <- st_read("01_inputs/01_hexagon/cluster_20_km.shp") %>% st_transform(4326)
# initiate gee -----------------------------------------------------------
ee_Initialize(user = "cody", drive = T, gcs = T)
# read data ---------------------------------------------------------------
modis_link <- "ECMWF/ERA5_LAND/MONTHLY_BY_HOUR"
modisIC <- ee$ImageCollection(modis_link)

bands_to_sum <- c("evaporation_from_bare_soil", "evaporation_from_open_water_surfaces_excluding_oceans",
                  "evaporation_from_the_top_of_canopy", "evaporation_from_vegetation_transpiration")

sum_bands <- function(image) {
  sum_bands <- image$select(bands_to_sum) %>% 
    ee$Image$reduce(ee$Reducer$sum())
  return(image$addBands(sum_bands))
}

modisIC_sum <- modisIC$map(sum_bands)

modis_evapotranspiration <- modisIC_sum$
  select(c("sum"))$
  map(ee_utils_pyfunc(
    function(x){x$copyProperties(x,x$propertyNames())}
  ))

modis_evapotranspiration_tidy <- as_tidyee(modis_evapotranspiration) #as tidy object
############################ ####Monthly evapotranspiration MEAN ##########################

monthly_baseline <- modis_evapotranspiration_tidy |>                                           # Historical
  filter(year %in% 2000:2015) |> 
  group_by(month) |> 
  summarise(stat=list("mean", "sd"))

evapotranspiration_recent_monthly <- modis_evapotranspiration_tidy |>                                        # RECENT 
  filter(year %in% c(2023),
         month > 1) |> 
  group_by(year, month) |> 
  summarise(stat="mean")

evapotranspiration_recent_renamed <- evapotranspiration_recent_monthly |> 
  dplyr::select(evpt="sum_mean")


evapotranspiration_recent_and_baseline<- inner_join(x = evapotranspiration_recent_renamed,
                                                    y = monthly_baseline,
                                                    by = "month")

evapotranspiration_recent_baseline_imageCol <- evapotranspiration_recent_and_baseline |> 
  as_ee()

evapotranspiration_zscore<- evapotranspiration_recent_baseline_imageCol$map(
  function(img){
    zscore<- img$expression(
      "float((evpt-sum_mean)/(sum_sd))",
      opt_map= list(evpt=img$select("evpt"),
                    sum_mean= img$select("sum_mean"),
                    sum_sd= img$select("sum_sd")
      )
    )$rename("evt_z_score")
    img$select("evpt","sum_mean")$addBands(zscore)
  }
) 

evapotranspiration_z <- as_tidyee(evapotranspiration_zscore)

evapotranspiration_z_pre_processed <- evapotranspiration_z |> 
  filter(year>=2022,
         month == 2) |> 
  dplyr::select("evt_z_score") 

rm(modis_evapotranspiration_tidy, monthly_baseline)

evapotranspiration_final <- list()

first_tier<-1:2000 
second_tier <- 2001:4000
third_tier <- 4001:6000
fourth_tier <- 6001:7000
fifth_tier <- 8001:10000
sixth_tier <- 10001:12000
seventh_tier <- 12001:14000
eighth_tier <- 14001:16000
ninth_tier <- 16001:18000
tenth_tier <- 18001:20000
eleventh_tier <- 20001:21823

all_tier <- c("first_tier","second_tier","third_tier","fourth_tier","fifth_tier", "sixth_tier", "seventh_tier", "eighth_tier", "ninth_tier",
              "tenth_tier", "eleventh_tier")

first_tier <- 7001:8000
all_tier<-c("first_tier")

# first_tier<-1:10000
# second_tier <- 10001:20000
# third_tier <- 20001:21823
# 
# all_tier <- c("first_tier","second_tier","third_tier")

for ( i in all_tier){
  grid_filter <- grid[get(i),]
  evapotranspiration_final[[i]] <- evapotranspiration_z_pre_processed %>%
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}

evapotranspiration_final_bind <- do.call("bind_rows",evapotranspiration_final)
evapotranspiration_final_bind <- evapotranspiration_final_bind %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date,label = T,abbr = F),
    parameter = case_when(
      parameter == "evt_z_score" ~ "Evaporation anomaly"
  )
)

write.csv(evapotranspiration_final_bind,"gee_data/20kmsqdata/evaporation_Z_value6and7k.csv")


