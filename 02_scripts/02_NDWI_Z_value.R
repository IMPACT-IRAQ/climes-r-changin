rm(list = ls())

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
# ndwi --------------------------------------------------------------------
############################## READ modis data ###########################
modis_link <- "MODIS/MOD09GA_006_NDWI"
modisIC <- ee$ImageCollection(modis_link)

modis_ndWi <- modisIC$
  select("NDWI")$
  map(
    ee_utils_pyfunc(
      function(x){x$
          multiply(0.0001)$
          copyProperties(x,x$propertyNames())
      }
    )
  )
############################################################################
modis_ndWi_tidy <- as_tidyee(modis_ndWi) #as tidy object
############################ ####Monthly NDwI MEAN ##########################
monthly_baseline <- modis_ndWi_tidy |>                                           # Historical
  filter(year %in% 2000:2015) |> 
  group_by(month) |> 
  summarise(stat=list("mean","sd"))

ndWi_recent_monthly <- modis_ndWi_tidy |>                                        # RECENT 
  filter(year %in% c(2018:2023)) |> 
  group_by(year, month) |> 
  summarise(stat="mean")
####################################################################

ndWi_recent_renamed <- ndWi_recent_monthly |> 
  dplyr::select(NDWI="NDWI_mean")

ndWi_recent_and_baseline<- inner_join(x = ndWi_recent_renamed,
                                      y = monthly_baseline,
                                      by = "month")

ndWi_recent_baseline_imageCol <- ndWi_recent_and_baseline |> 
  as_ee()

ndWi_zscore<- ndWi_recent_baseline_imageCol$map(
  function(img){
    zscore<- img$expression(
      "float((NDWI-NDWI_mean)/(NDWI_sd))",
      opt_map= list(NDWI= img$select("NDWI"),
                    NDWI_mean= img$select("NDWI_mean"),
                    NDWI_sd= img$select("NDWI_sd")
      )
    )$rename("NDWI_z_score")
    img$select("NDWI","NDWI_mean")$addBands(zscore)
  }
) 

ndWi_z <- as_tidyee(ndWi_zscore)

ndWi_z_pre_processed <- ndWi_z |> 
  filter(year=="2022",
         month>=10) |> 
  dplyr::select("NDWI_z_score")

ndWi_final <- list()

first_tier<-1:1500 
second_tier <- 1501:3000
third_tier <- 3001:4500
fourth_tier <- 4501:6000
fifth_tier <- 6001:7500
sixth_tier <- 7501:9000
seventh_tier <- 9001:10500
eighth_tier <- 10501:12000
ninth_tier <- 12001:13500
tenth_tier <- 13501:15000
eleventh_tier <- 15001:16500
twelfth_tier <- 16501:18000
thirteenth_tier <- 18001:19500
fourteenth_tier <- 19501:21000
fifteenth_tier <- 21000:21823



all_tier <- c("first_tier","second_tier","third_tier","fourth_tier","fifth_tier", "sixth_tier", "seventh_tier", "eighth_tier", "ninth_tier",
              "tenth_tier", "eleventh_tier", "twelfth_tier", "thirteenth_tier", "fourteenth_tier", "fifteenth_tier")

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:21823

all_tier <- c("first_tier","second_tier","third_tier")

for ( i in all_tier){
  
  grid_filter <- grid[get(i),]
  # 1000o is working 
  ndWi_final[[i]] <- ndWi_z_pre_processed %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}

ndWi_final_bind <- do.call("bind_rows",ndWi_final)
ndWi_final_bind <- ndWi_final_bind %>% 
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date,label = T,abbr = F),
    parameter = case_when(
      parameter == "NDWI_z_score" ~ "NDWI anomaly"))

write.csv(ndWi_final_bind,"gee_data/20kmsqdata/NDWI_Z_value_october2022andafter.csv")


