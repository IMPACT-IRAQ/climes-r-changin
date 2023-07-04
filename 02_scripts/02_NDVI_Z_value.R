rm(list = ls())

library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

# read cluster ------------------------------------------------------------
grid <- st_read("01_inputs/01_hexagon/cluster_20_km.shp")
# initiate gee -----------------------------------------------------------
ee_Initialize(user = "cody", drive = T, gcs = T)
# ndvi --------------------------------------------------------------------

############################## READ modis data ###########################
#modis_link <- "MODIS/006/MOD13Q1"
#previous modis link stopped updating in february 2023
modis_link <- "MODIS/061/MOD13Q1"
modisIC <- ee$ImageCollection(modis_link)

modis_ndvi <- modisIC$
  select("NDVI")$
  map(
    ee_utils_pyfunc(
      function(x){x$
          multiply(0.0001)$
          copyProperties(x,x$propertyNames())
      }
    )
  )

############################################################################
modis_ndvi_tidy <- as_tidyee(modis_ndvi) #as tidy object
############################ ####Monthly NDVI MEAN ##########################
monthly_baseline <- modis_ndvi_tidy |>                                           # Historical
  filter(year %in% 2000:2015) |> 
  group_by(month) |> 
  summarise(stat=list("mean","sd"))

ndvi_recent_monthly <- modis_ndvi_tidy |>                                        # RECENT 
  filter(year %in% c(2016:2023)) |> 
  group_by(year,month) |> 
  summarise(stat="mean")

####################################################################
ndvi_recent_renamed <- ndvi_recent_monthly |> 
  dplyr::select(NDVI="NDVI_mean")

ndvi_recent_and_baseline<- inner_join(x = ndvi_recent_renamed,
                                      y = monthly_baseline,
                                      by = "month")

ndvi_recent_baseline_imageCol <- ndvi_recent_and_baseline |> 
  as_ee()

ndvi_zscore<- ndvi_recent_baseline_imageCol$map(
  function(img){
    zscore<- img$expression(
      "float((NDVI-NDVI_mean)/(NDVI_sd))",
      opt_map= list(NDVI= img$select("NDVI"),
                    NDVI_mean= img$select("NDVI_mean"),
                    NDVI_sd= img$select("NDVI_sd")
      )
    )$rename("NDVI_z_score")
    img$select("NDVI","NDVI_mean")$addBands(zscore)
  }
  
) 
ndvi_z <- as_tidyee(ndvi_zscore)

#
ndvi_z_pre_processed <- ndvi_z |> 
  filter(year>2022,
         month>2) |> 
  dplyr::select("NDVI_z_score")

# leaflet::leaflet(grid ) |>
#   leaflet::addTiles() |> 
#   leaflet::addPolygons()


ndvi_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:21823

all_tier <- c("first_tier","second_tier","third_tier")

for ( i in all_tier){
  
  grid_filter <- grid[get(i),]
  ndvi_final[[i]] <- ndvi_z_pre_processed %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}

ndvi_final_bind <- do.call("bind_rows",ndvi_final)
ndvi_final_bind <- ndvi_final_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F),
  parameter = case_when(
    parameter == "NDVI_z_score" ~ "NDVI anomaly"))

write.csv(ndvi_final_bind,"gee_data_raw/20kmsqdata/NDVI_Z_value_march_may_2023.csv")


