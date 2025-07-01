### created: 03/17/2023
### last updated: 04/27/2025

# 01a - SYSTEMATIC PRECLUSION: FALL ####

## OBJECTIVE ####
# create a loop that begins in the most recent year and calculates the stratified mean under the preclusion scenario, then calculates each subsequent year under the preclusion scenario until all years have tows precluded from the average 


### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
source(here("R", "StratMeanFXs_v2.R")) # stratified mean functions

system.precl.dat <- here("data", "sumflounder", "systematic-preclusion")
sumflounder.dat <- here("data", "sumflounder")

### LOAD DATA ####
# summer flounder data consisting of observations filtered based on seasonal footprints created here("sumflounder", "04-filter-summer-flounder.R")
sf_fall <- readRDS(here(sumflounder.dat, "sumflounder_fall.rds"))


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
incl_data <- sf_fall |> 
  anti_join(remove_incl_strata, by = c("SEASON", "EST_YEAR", "STRATUM"))

### Wind-precluded effort
remove_precl_strata <- one_tow_strata |> 
  select(!incl_strata) |> unnest(cols = precl_strata)

# filter out years and strata for the preclusion scenario
precl_data <- sf_fall |> 
  filter(AREA == "OUTSIDE") |>
  anti_join(remove_precl_strata, by = c("SEASON", "EST_YEAR", "STRATUM"))

## WIND INCLUDED LOOP ####
# create storage vector for years 
x <- c()
# create storage data frame for stratified mean values 
ww.data <- data.frame()
# create year object for the years in the dataframe to filter for in the loop 
years <- c(2022, sort(unique(sf_fall$EST_YEAR), decreasing = TRUE))

### TEST LOOP ###
# test <- stratified.mean(sf_fall)
# 
# x <- 2009
# x <- append(x, 2010)
# 
# y <- sf_fall |> 
#   filter(EST_YEAR %in% x) |>
#   stratified.mean() |>
#   mutate(step = length(x))
# ww.data <- bind_rows(ww.data, y)
# 
# y <- sf_fall |> 
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
    mutate(STEP = (length(x)-1), # count each loop starting at -1 to represent the number of years removed
           effort = "Status quo survey effort")
  ww.data <- bind_rows(ww.data, y) |> # add each loop step the stratified means to the with wind dataframe
    arrange(STEP, EST_YEAR)
}

# remove NA column 
ww.data <- ww.data |> dplyr::select(!stratmean)

# 2020 was not surveyed for summer flounder so 2021 was removed twice, remove the duplicate of this time step while maintaining a sequence for number of years removed
# ww.data <- filter(ww.data, !STEP == 0) |> 
#   mutate(STEP = ifelse(STEP == -1, 0, STEP)) # replace -1 with 0, representing no years removed 

### save data 
saveRDS(ww.data, here(system.precl.dat, "sf_fall-with_wind-system_rm.rds"))

## WIND PRECLUDED LOOP ####
# create storage vector for years 
x <- c()

# create storage dataframe for stratified mean values
wo.data <- data.frame()

# create year object for the years in the dataframe to filter for in the loop 
years <- sort(unique(sf_fall$EST_YEAR), decreasing = TRUE)

### LOOP ####
for(i in years){ # count backwards by one starting at 2022 
  x <- append(x, i) # add i to the storage vector
  y <- precl_data |> 
    filter(EST_YEAR %in% c(x)) |>#, AREA == "OUTSIDE") |> # filter out x value from year, and tows indicated as wind tows 
    group_by(EST_YEAR) |> 
    nest() |> 
    mutate(stratmean = map(data, ~stratified.mean(., strata))) |> # calculate stratified mean
    dplyr::select(!data) |> 
    unnest(cols = stratmean) |> 
    mutate(STEP = (length(x)), # count each loop starting at -1 to represent the number of years removed
           effort = "Wind-precluded survey effort")
  wo.data <- bind_rows(wo.data, y) # add each loop step the stratified means to the with wind dataframe
}


# filter out repeated values of 2021, keep where time step == 1 to represent 1 year precluded 
# wo.data <- filter(wo.data, !STEP == 0)

### save data 
saveRDS(wo.data, here(system.precl.dat, "sf_fall-without_wind-system_rm.rds"))

## COMBINE DATA ####
# bind with wind and without wind stratified mean values to the same dataframe 
all.data <- bind_rows(ww.data, wo.data) |>
  arrange(STEP)

### save data 
saveRDS(all.data, here(system.precl.dat, "sf_fall-system_rm-fullset.rds"))

# averages <- all.data |>
#   group_by(STEP) |>
#   summarise(avg = mean(stratmu))
