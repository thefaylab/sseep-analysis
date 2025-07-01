### created: 01/29/2024
### last updated: 04/27/2025

# 01 - ATLANTIC MACKEREL SYSTEMATIC PRECLUSION ####

## OBJECTIVE ####
# create a loop that begins in the most recent year and calculates the stratified mean under the preclusion scenario, then calculates each subsequent year under the preclusion scenario until all years have tows precluded from the average


### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
source(here("R", "StratMeanFXs_v2.R"))

system.precl.dat <- here("data", "atlmackerel", "systematic-preclusion")
atlmackerel.dat <- here("data", "atlmackerel")

### LOAD DATA ####
# spring Atlantic mackerel data consisting of observations filtered here("atlmackerel", "02-filter-atlantic-mackerel.R")
data <- readRDS(here(atlmackerel.dat, "atlmackerel_spring.rds"))

#active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

# strata and year combinations that will be removed during the stratified mean calculations because only one tow was conducted. created from `05b-spatial-filter-data.R` here("tidy-data").
one_tow_strata <- readRDS(here("data", "rds", "one_tow_strata.rds"))

## DATA WRANGLE ####
# create data frames of year and strata combinations where only one tow occurred
### Status quo effort 
remove_incl_strata <- one_tow_strata |> 
  select(!precl_strata) |> unnest(cols = incl_strata)

# filter out years and strata for the status quo scenario
incl_data <- data |> 
  anti_join(remove_incl_strata, by = c("SEASON", "EST_YEAR", "STRATUM"))

### Wind-precluded effort
remove_precl_strata <- one_tow_strata |> 
  select(!incl_strata) |> unnest(cols = precl_strata)

# filter out years and strata for the preclusion scenario
precl_data <- data |> 
  filter(AREA == "OUTSIDE") |>
  anti_join(remove_precl_strata, by = c("SEASON", "EST_YEAR", "STRATUM"))

## WIND INCLUDED LOOP ####
# create storage vector for years 
x <- c()

# create storage data frame for stratified mean values 
ww.data <- data.frame()
# create year object for the years in the dataframe to filter for in the loop 
years <- c(2022, sort(unique(data$EST_YEAR), decreasing = TRUE))

### TEST LOOP ###
# test <- strata.mean(sf_spring)
# 
# x <- NULL
# x <- append(x, 2022)
# 
# y <- data |> 
#   filter(EST_YEAR %in% x) |>
#   strata.mean() |>
#   mutate(step = length(x))
# ww.data <- bind_rows(all.data, y)
# 
# y <- data |> 
#   filter(!EST_YEAR %in% x) |>
#   strata.mean() |>
#   mutate(step = length(x))
# wo.data <- bind_rows(wo.data, y)



### LOOP #### 
for(i in years){ # count backwards by one starting at 2022 
  x <- append(x, i) # add i to the storage vector
  y <- incl_data |> 
    filter(!EST_YEAR %in% x) |> # filter out x value from year
    group_by(EST_YEAR) |> 
    nest() |> 
    mutate(stratmean = map(data, ~stratified.mean(., strata))) |> # calculate stratified mean
    dplyr::select(!data) |> 
    unnest(cols = stratmean) |> 
    mutate(STEP = (length(x) - 1), # count each loop starting at -1 to represent the number of years removed
           effort = "Status quo survey effort")
  ww.data <- bind_rows(ww.data, y) # add each loop step the stratified means to the with wind dataframe
}

# remove NA column 
ww.data <- ww.data |> dplyr::select(!stratmean)

### save data 
saveRDS(ww.data, here(system.precl.dat, "with_wind-system_rm.rds"))

## WIND PRECLUDED LOOP ####
# create storage vector for years 
x <- c()

# create storage dataframe for stratified mean values
wo.data <- data.frame()


### LOOP ####
for(i in years){ # count backwards by one starting at 2021 
  x <- append(x, i) # add i to the storage vector
  y <- precl_data |> 
    filter(EST_YEAR %in% c(x)) |>#, AREA == "OUTSIDE") |> # filter out x value from year, and wind tows 
    group_by(EST_YEAR) |> 
    nest() |> 
    mutate(stratmean = map(data, ~stratified.mean(., strata))) |> # calculate stratified mean
    dplyr::select(!data) |> 
    unnest(cols = stratmean) |> 
    mutate(STEP = length(x)-1, # count each loop starting at 1 to represent the number of years removed
           effort = "Wind-precluded survey effort")
  wo.data <- bind_rows(wo.data, y) # add each loop step the stratified means to the with wind dataframe
}

# remove NA column 
wo.data <- wo.data |> dplyr::select(!stratmean)

### save data 
saveRDS(wo.data, here(system.precl.dat, "without_wind-system_rm.rds"))


## COMBINE DATA ####
# bind with wind and without wind stratified mean values to the same dataframe 
all.data <- bind_rows(ww.data, wo.data) |>
  arrange(STEP)

### save data 
saveRDS(all.data, here(system.precl.dat, "system_rm-fullset.rds"))

# averages <- all.data |>
#   group_by(STEP) |>
#   summarise(avg = mean(stratmu))
