# PURPOSE: Create dataset with temp, evapotranspiration and precipitation for use in water scarcity model
# AUTHOR: Cody Adelson | Data Specialist
# DATE CREATED: April 30, 2023
# NOTES: 

library(zoo)
library(stringr)
library(tidyr)

model_data_raw <- read_xlsx("01_inputs/model_data.xlsx") %>% 
  #mutate(n=row_number()) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  #select(-n) %>%
  mutate(
    irrigation_use = as.character(round(evapotranspiration - rainfall, 1)),
    "irrigation_consumptive_use"= case_when(
      rainfall > evapotranspiration ~ "Rainfall Satisfies Crop Requirement",
      TRUE ~ paste0("shortfall:", irrigation_use, "mm"))) %>% 
  pivot_longer(cols = c(evapotranspiration, temperature, rainfall), names_to = "indicator", values_to = "value") %>%
  filter(!is.na(value)) %>% 
  mutate(
    indicator = case_when(
      indicator == "temperature" ~ "Temperature °C",
      indicator == "rainfall" ~ "Rainfall (mm)",
      indicator == "evapotranspiration" ~ "Evapotranspiration (mm)"),
    date = zoo::as.yearmon(date),
    year = as.numeric(format(as.Date(date), "%y")),
    type_color = case_when(
      type == "data" ~ denim,
      TRUE ~ burnt_sienna),
    value = round(value, 1)) %>% 
  write.xlsx("05_outputs/model_data_output.xlsx")
