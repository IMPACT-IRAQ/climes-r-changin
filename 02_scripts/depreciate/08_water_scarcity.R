### Purpose: Calculate rainfall forecast based on monthly data input
### Date: 21 Feb 23
library(tidyverse)
library(readxl)
library(data.table)

# Set smoothing constant
alpha <- 1
beta <- 1
gamma <- 1

# Load in data and create year, month and day variables
weather_data <- read_excel("01_inputs/08_water_scarcity.xlsx") %>% 
  separate(date, sep = "-", c("year", "month", "day"), remove = FALSE)

# Conduct first year calculations, which are different from subsequent years
first_year <- weather_data %>% 
  filter(year == min(year)) %>%
  mutate(seasonal = rainfall - mean(rainfall),
         level = case_when(
           month == "12" ~ mean(rainfall),
           TRUE ~ NA_real_),
         trend = case_when(
           month == "12" ~ (rainfall/lag(seasonal, 11)) - (lag(rainfall, 1)/seasonal),
           TRUE ~ NA_real_))

#Bind with subsequent years and conduct calculations
subsequent_years <- weather_data %>%
  filter(year != min(year)) %>%
  bind_rows(first_year) %>% 
  arrange(date) %>% 
  ## CALCULATE FIRST ROW FIRST BEFORE
  mutate(
    level = case_when(
      year != min(year) ~ alpha * (rainfall - lag(seasonal, 12) + (1 - alpha) * (lag(level, 1) + lag(trend, 1))),
      TRUE ~ level),
    trend = case_when(
      year != min(year) ~ beta * (level - lag(level, 1) + (1 - beta) * lag(trend, 1)),
      TRUE ~ trend),
    seasonal = case_when(
      year != min(year) ~ gamma * (rainfall - lag(level, 1) + lag(trend, 1) + (1 - gamma) * lag(seasonal, 12)),
      TRUE ~ seasonal))


####################################### Fix

dat <- weather_data %>%
  filter(year != min(year)) %>%
  bind_rows(first_year) %>% 
  arrange(date) %>% 
  mutate(
    x = case_when(
      year == "1988" & month != "12" ~ 1,
      TRUE ~ level),
    y = case_when(
      year == "1988" & month != "12" ~ 1,
      TRUE ~ trend),
    year = as.numeric(year),
    month = as.numeric(month),
    row = row_number()) %>% 
  rename("z" = seasonal) %>% 
  select(row, rainfall, x, y, z) %>% 
  print(n=20)

calc_row <- function(dat, run = nrow(dat)) {
  
  omit_na_dat <- na.omit(dat)
  
  if (nrow(omit_na_dat) == run) {
    return(dat)
  }
  
  row_idx <- nrow(omit_na_dat)
  
  new_x = omit_na_dat[row_idx, ][["x"]] - omit_na_dat[row_idx - 11, ][["z"]]
  new_y = beta * omit_na_dat[row_idx, ][["x"]]
  new_z = gamma * omit_na_dat[row_idx, ][["x"]] + omit_na_dat[row_idx, ][["y"]]
  new_row = omit_na_dat[row_idx, ][["row"]] + 1
  
  dat <- rows_update(dat, tibble(row = new_row,
                                 x = new_x,
                                 y = new_y,
                                 z = new_z
                                 ),
                     by = "row"
  )
  calc_row(dat, run = run)
}

calc_row(dat) %>%
  print(n=20)
  View()

  # 
  # library(tidyverse)
  # 
  # dat <- tibble(year = c(2010, 2011, 2012, 2013, 2014), x=c(1, 3, NA, NA, NA), y=c(2, 4, NA, NA, NA), z = 1)
  # 
  # calc_row <- function(dat, run = nrow(dat)) {
  #   
  #   omit_na_dat <- na.omit(dat)
  #   
  #   if (nrow(omit_na_dat) == run) {
  #     return(dat)
  #   }
  #   
  #   row_idx <- nrow(omit_na_dat)
  #   
  #   new_x = omit_na_dat[row_idx, ][["x"]] + omit_na_dat[row_idx, ][["y"]]
  #   new_y = omit_na_dat[row_idx - 1, ][["x"]] + omit_na_dat[row_idx - 1, ][["y"]]
  #   new_year = omit_na_dat[row_idx, ][["year"]] + 1
  #   new_z = dat[row_idx + 1, ][["z"]] + 1
  #   
  #   dat <- rows_update(dat, tibble(year = new_year,
  #                                  x = new_x,
  #                                  y = new_y,
  #                                  z = new_z),
  #                      by = "year")
  #   
  #   calc_row(dat, run = run)
  # }
  # 