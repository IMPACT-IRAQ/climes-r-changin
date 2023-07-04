# PURPOSE: Create dataset with temp, evapotranspiration and precipitation for use in water scarcity model
# AUTHOR: Cody Adelson | Data Specialist
# DATE CREATED: April 30, 2023
# NOTES: Change TO 0s BEFORE calculating irrigation_use

library(zoo)
library(stringr)
library(tidyr)
library(openxlsx)
library(readxl)
library(glitr)
library(lubridate)
library(tidyverse)

model_data_raw <- read_xlsx("01_inputs/model_data_2023.xlsx") %>% 
  mutate(
    value = case_when(
      indicator != "temperature" & value <0 ~ 0,
      TRUE ~ round(value, 1))) %>%
  #mutate(n=row_number()) %>% 
  pivot_wider(names_from = "indicator", values_from = "value") %>% 
  #select(-n) %>%
  mutate(
    irrigation_use = as.character(round(evapotranspiration - (.8*rainfall), 1)),
    irrigation_num = as.numeric(irrigation_use),
    "irrigation_consumptive_use"= case_when(
      irrigation_use <= 0 ~ "Rainfall Satisfies Crop Requirement",
      TRUE ~ paste0("shortfall:", irrigation_use, "mm"))) %>% 
  pivot_longer(cols = c(evapotranspiration, temperature, rainfall, irrigation_num), names_to = "indicator", values_to = "value") %>%
  filter(!is.na(value)) %>% 
  mutate(
    indicator = case_when(
      indicator == "temperature" ~ "Surface Temperature °C",
      indicator == "rainfall" ~ "Rainfall (mm)",
      indicator == "evapotranspiration" ~ "Evapotranspiration (mm)",
      indicator == "irrigation_num" ~ "Predicted Irrigation Consumptive Use (mm/month)"),
    date = zoo::as.yearmon(date),
    year = as.numeric(format(as.Date(date), "%y")),
    year_full = as.numeric(format(as.Date(date), "%Y")),
    type_color = case_when(
      type == "data" ~ denim,
      TRUE ~ burnt_sienna),
    value_label = case_when(
      indicator == "Predicted Irrigation Consumptive Use (mm/month)" ~ irrigation_consumptive_use,
      TRUE ~ as.character(value)),
    value = case_when(
      indicator != "temperature" & value <0 ~ 0,
      TRUE ~ round(value, 1))) %>%
  # group_by(district, as.character(year_full)) %>% 
  # mutate(mean_irrigation_use = mean(irrigation_use)) %>% 
  filter(year_full > 2000)

irrigation <- model_data_raw %>% 
  filter(year_full == max(year_full),
         indicator == "Predicted Irrigation Consumptive Use (mm/month)") %>% 
  group_by(district) %>% 
  mutate(mean_irrigation = mean(value)) %>% 
  select(governorate, district, date, indicator, mean_irrigation)

model_data_raw %>% 
  left_join(irrigation, by = c("governorate", "district", "date", "indicator")) %>%
  filter(governorate %in% c("Ninewa", "Duhok", "Diyala")) %>% 
  write.xlsx("05_outputs/model_data_output.xlsx")
