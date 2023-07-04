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
monthly_baseline <- modis_temperature_tidy |>                                             # Historical
  filter(year %in% 2000:2015) |> 
  group_by(month) |> 
  summarise(stat=list("mean","sd"))

temp_recent_monthly <- modis_temperature_tidy |>                                        # RECENT 
  filter(year %in% c(2023)) |> 
  group_by(year,month) |> 
  summarise(stat="mean")

####################################################################

#Why rename this?
temp_recent_renamed <- temp_recent_monthly |> 
  dplyr::select(temp="LST_Day_1km_mean")


temp_recent_and_baseline<- inner_join(x = temp_recent_renamed,
                                      y = monthly_baseline,
                                      by = "month")

temp_recent_baseline_imageCol <- temp_recent_and_baseline |> 
  as_ee()

# Caclulate the zscore, temp is recent year temperature, lst mean is historical. Returning three bands.
temp_zscore<- temp_recent_baseline_imageCol$map(
  function(img){
    zscore<- img$expression(
      "float((temp-LST_Day_1km_mean)/(LST_Day_1km_sd))",
      opt_map= list(temp= img$select("temp"),
                    LST_Day_1km_mean= img$select("LST_Day_1km_mean"),
                    LST_Day_1km_sd= img$select("LST_Day_1km_sd")
      )
    )$rename("temp_z_score")
    img$select("temp","LST_Day_1km_mean")$addBands(zscore)
  }
  
) 
temp_z <- as_tidyee(temp_zscore)

#
temp_z_pre_processed <- temp_z |> 
  filter(year>=2023,
         month==3) |> 
  dplyr::select("temp_z_score")

temp_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:21823

all_tier <- c("first_tier","second_tier","third_tier")

for ( i in all_tier){
  
  grid_filter <- grid[get(i),]
  temp_final[[i]] <- temp_z_pre_processed %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}



temp_final_bind <- do.call("bind_rows",temp_final)
temp_final_bind <- temp_final_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F),
  parameter = case_when(
    parameter == "temp_z_score" ~ "Temperature anomaly"))

write.csv(temp_final_bind,"gee_data_raw/20kmsqdata/temperature_Z_value_march_2023.csv")
