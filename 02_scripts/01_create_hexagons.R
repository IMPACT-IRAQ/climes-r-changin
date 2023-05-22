rm (list = ls())

start_time <- Sys.time()

library(sf)
library(raster)
library(tidyverse)
library(sp)

options(scipen = 999)

 

# read admin boundary -----------------------------------------------------

admin_gbd <- "~/github/common_shapefiles/"
admin_zero <- st_read(paste0(admin_gbd,"irq_admbnda_adm0_cso_itos_20190603.shp")) %>% 
  st_transform(crs = 32638) 
iraq_boundary <- admin_zero %>% as_Spatial()


# create hexagon ----------------------------------------------------------

#HexPts <-spsample(iraq_boundary, "hexagonal",n=50394, offset=c(0,0)) # 10 kmsq
HexPts <-spsample(iraq_boundary, "hexagonal",n=25197, offset=c(0,0)) # 20 kmsq

grid <- HexPoints2SpatialPolygons(HexPts)
grid <- grid %>% st_as_sf() %>% st_transform(crs= 32638)
st_write(grid, "01_inputs/01_hexagon/cluster_20_km.shp")

