### created: 11/30/2023
### last updated: 12/07/2023

# 01 - FILTER ATLANTIC MACKEREL DATA ####


## OBJECTIVE ####
# save copies of only Atlantic mackerel data for future analyses
# creates datasets where outliers are removed and are maintained to compare best model fit in sdmtmb framework 

### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


### LOAD DATA ####
# expanded data created here("tidy-data", "04-complete-datasets.R")
data <- readRDS(here("data", "rds", "completed_bts_data.rds")) |> filter(SVSPP == 121) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# set the years that will be included in the dataset because they are full survey years 
full_survey <- c(2009:2019, 2021)


## FILTER DATA ####
# without outliers removed 
am_spring <- data |> 
  filter(SEASON == "SPRING", YEAR %in% full_survey)

# with outliers removed 
# filter raw data by season for seasonal model fitting
am_spring_no.out <- data |> 
  filter(SEASON == "SPRING", YEAR %in% full_survey, EXPCATCHWT <= quantile(EXPCATCHWT, 0.99))


### save the data 
saveRDS(data, here("data", "atlmackerel", "atlmack_complete_data.rds"))
saveRDS(am_spring, here("data", "atlmackerel", "atlmackerel_spring.rds"))
saveRDS(am_spring_no.out, here("data", "atlmackerel", "atlmackerel_spring_no-out.rds"))

