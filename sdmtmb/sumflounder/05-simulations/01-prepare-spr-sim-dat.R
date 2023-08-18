### created: 07/24/2023
### last updated: 

# 01a -  DATA ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"
#source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
set.seed(123)


## LOAD DATA ####
# read in spring data from model fitting
sf_spring <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_spring.rds"))
# 
# sf_fall21 <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds")) |> 
#   filter(EST_YEAR == 2021)

sf_spring|> group_by(EST_YEAR) |> mutate(tow = str_c("STRATUM", "CRUISE6", "STATION")) |> summarise(tow = length(tow)) |> filter(EST_YEAR != 2020) |> summary()

## DATA WRANGLE ####
# extract model fitting data
# moddat <- fall_mod$data |>
#   filter(EST_YEAR %)

# extract the most recent year of survey data 
# last_yr_dat <- sf_fall |>
#   filter(EST_YEAR == 2021)

# set the future years that will be simulated 
future_years <- c(2022:2026)

# replicate the most recent year of data for the number of future years to be simulated and add a "replicate" column
replicate_data <- map_dfr(seq_along(future_years), ~sf_spring |> mutate(rep = .x)) 

# resample the data to create a new dataset for each replicate 
future_data <- replicate_data |>
  group_by(rep) |>
  nest() |> 
  mutate(resamp = map(data, ~replicate_data[sample(nrow(.), size = 130, replace = TRUE),]))|> # size = the average number of tows for the time series 
  select(rep, resamp) |>
  mutate(rep = rep+2021) |> #case_when(
    #rep ==1 ~ 2020, 
    #rep > 1 ~ rep+2020)) |> 
  rename(rep_yr = rep) |>
  unnest(cols = c(resamp)) |> 
  select(-c(EST_YEAR, rep)) |>
  rename(EST_YEAR = rep_yr)

# save the data
saveRDS(future_data, here("sdmtmb", "sumflounder", "data", "simulations", "spring_future_data.rds"))
  
#sf_fall_dat <- bind_rows(sf_fall, resample_data)

spring_mesh <- make_mesh(future_data, xy_cols = c("X", "Y"), cutoff = 10)
plot(spring_mesh)

saveRDS(spring_mesh, here("sdmtmb", "sumflounder", "data", "simulations", "spring_future_mesh.rds"))

