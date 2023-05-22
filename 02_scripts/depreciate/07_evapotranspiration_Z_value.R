rm(list = ls())

#Issue was for each hexagon it was all NAs when you pulled data from google earth engine

library(reticulate)
# use_python("04_py_env/Scripts/python.exe")
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

# read cluster ------------------------------------------------------------

grid <- st_read("01_inputs/01_hexagon/cluster_10_km.shp") %>% st_transform(4326)

# initiatin gee -----------------------------------------------------------
ee_Initialize(user = "cody", drive = T, gcs = T)
# read data ---------------------------------------------------------------
modis_link <- "MODIS/006/MOD16A2"
modisIC <- ee$ImageCollection(modis_link)

# modis_evapotranspiration <- modisIC$
#   select("LE")$
#   map(
#     ee_utils_pyfunc(
#       function(x){x$
#           multiply(10000)$
#           copyProperties(x,x$propertyNames())
#       }
#     )
#   )

modis_evapotranspiration <- modisIC$
  select("ET")$
  map(
    ee_utils_pyfunc(
      function(x){x$
          multiply(10000)$
          copyProperties(x,x$propertyNames())
      }
    )
  )

modis_evapotranspiration_tidy <- as_tidyee(modis_evapotranspiration) #as tidy object


############################ ####Monthly evapotranspiration MEAN ##########################

monthly_baseline <- modis_evapotranspiration_tidy |>                                           # Historical
  filter(year %in% 2000:2015) |> 
  group_by(month) |> 
  summarise(stat=list("mean","sd"))


evapotranspiration_recent_monthly <- modis_evapotranspiration_tidy |>                                        # RECENT 
  filter(year %in% c(2016:2022)) |> 
  group_by(year,month) |> 
  summarise(stat="mean")


# evapotranspiration_recent_renamed <- evapotranspiration_recent_monthly |> 
#   dplyr::select(evpt="LE_mean")

evapotranspiration_recent_renamed <- evapotranspiration_recent_monthly |> 
  dplyr::select(evpt="ET_mean")


evapotranspiration_recent_and_baseline<- inner_join(x = evapotranspiration_recent_renamed,
                                      y = monthly_baseline,
                                      by = "month")


evapotranspiration_recent_baseline_imageCol <- evapotranspiration_recent_and_baseline |> 
  as_ee()


# evapotranspiration_zscore<- evapotranspiration_recent_baseline_imageCol$map(
#   function(img){
#     zscore<- img$expression(
#       "float((evpt-LE_mean)/(LE_sd))",
#       opt_map= list(evpt=img$select("evpt"),
#                     LE_mean= img$select("LE_mean"),
#                     LE_sd= img$select("LE_sd")
#       )
#     )$rename("evt_z_score")
#     img$select("evpt","LE_mean")$addBands(zscore)
#   }
# ) 

evapotranspiration_zscore<- evapotranspiration_recent_baseline_imageCol$map(
  function(img){
    zscore<- img$expression(
      "float((evpt-ET_mean)/(ET_sd))",
      opt_map= list(evpt=img$select("evpt"),
                    ET_mean= img$select("ET_mean"),
                    ET_sd= img$select("ET_sd")
      )
    )$rename("evt_z_score")
    img$select("evpt","ET_mean")$addBands(zscore)
  }
) 


evapotranspiration_z <- as_tidyee(evapotranspiration_zscore)


evapotranspiration_z_pre_processed <- evapotranspiration_z |> 
  filter(year>=2022) |> 
  #filter(month==1) |> 
  dplyr::select("evt_z_score") 


evapotranspiration_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:30000
fourth_tier <- 30001:40000
fifth_tier <- 40001:43648

all_tier <- c("first_tier","second_tier","third_tier","fourth_tier","fifth_tier")

grid_filter <- grid[1,] #Temporary

for ( i in all_tier){
  grid_filter <- grid[get(i),]
  # 1000o is working 
  evapotranspiration_final[[i]] <- evapotranspiration_z_pre_processed %>%
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}



evapotranspiration_final_bind <- do.call("bind_rows",evapotranspiration_final)
evapotranspiration_final_bind <- evapotranspiration_final_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F)
)



write.csv(evapotranspiration_final_bind,"05_outputs/06_evapotranspiration/evapotranspiration_Z_value_2.csv")





