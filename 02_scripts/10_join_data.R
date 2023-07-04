# PURPOSE: Read in climate data and join, write to csv file for use in shiny application
# AUTHOR: Cody Adelson | Data Specialist
# DATE CREATED: March 9, 2022
# NOTES: 

library(dplyr)
library(tidyr)
library(feather)
library(purrr)
library(sf)
library(readr)
library(lubridate)

grid <- st_read("01_inputs/01_hexagon/cluster_20_km.shp") %>% st_transform(4326) 

dates_2022<- rev(as.character(format(seq(my("January 2022"), by ="month", length.out = 12), "%B %Y")))
dates_2023<- rev(as.character(format(seq(my("January 2023"), by ="month", length.out = (Sys.Date()-my("January 2023"))/30), "%B %Y")))

dates_order<-c("Yearly Average 2023", dates_2023, "Yearly Average 2022", dates_2022)

### Test
temp <- read_csv("gee_data_raw/20kmsqdata/temperature.csv")
temp <- read_csv("gee_data_raw/20kmsqdata/temperature_Z_value.csv")
temp <- read_csv("gee_data_raw/20kmsqdata/temperature_march_2023.csv")
temp <- read_csv("gee_data_raw/20kmsqdata/temperature_Z_value_march_2023.csv")
temp <- read_csv("gee_data_raw/20kmsqdata/precipitation.csv")

indicator_files <- list.files("gee_data_raw/20kmsqdata/", "*.csv", full.names = TRUE)
climate_data <- 
  map_dfr(.x = indicator_files, .f = ~read.csv(.x), col_types = list(date = col_character())) %>% 
  #select(-...1, -date) %>% 
  select(-date) %>% 
  mutate(value = round(value, 3),
         parameter = case_when(
           parameter == "temp" ~ "Surface Temperature",
           parameter == "sum_current_precipitation" ~ "Precipitation",
           TRUE ~ parameter),
         #date = paste(month, year),
         date = paste(month, year, sep = "_"))

yearly_average <- climate_data %>% 
  group_by(year, FID, parameter) %>% 
  summarise(value = mean(value, na.rm= T)) %>% 
  #mutate(date = paste("Yearly Average", year)) %>% 
  mutate(date = paste("Yearly Average_", year, sep=""))

climate_data_bind <- bind_rows(climate_data, yearly_average) %>% 
  select(-month, -year) %>% 
  mutate(rank = match(date, dates_order)) %>%
  arrange(rank) %>%
  select(-rank) %>% 
  distinct() 

climate_data_bind_wide <- climate_data_bind %>% 
  mutate(n=row_number()) %>% 
  pivot_wider(names_from = c(parameter, date), values_from = value) %>% 
  select(-n) %>% 
  group_by(FID) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  left_join(grid) %>% 
  select(-FID) %>% 
  saveRDS(file = "05_outputs/processed_indicators/climate_data.rds")



# Scratch

# admin_boundary_path <-  "01_inputs/03_admin_boundary/"
# admin2_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm2_cso_20190603.shp"))

# climate_data_grid <- climate_data_bind %>% 
#   left_join(grid, by = "FID")

# climate_data_grid_sf <- st_as_sf(climate_data_grid, 
#                                  crs = 4326)
# 
# admin2_boundary_sf <- st_as_sf(admin2_boundary)
# 
# 
# joined_sf <- st_join(climate_data_grid_sf, admin2_boundary_sf)

#warning - takes a long time
# joined_sf_sub_district <- joined_sf %>% 
#   group_by(ADM2_EN, ADM1_EN, date.x, parameter) %>% 
#   summarise(value = mean(value)) %>% 
#   ungroup()

# st_write(joined_sf_sub_district, "05_outputs/sub_district_values.shp", driver = "CSV")