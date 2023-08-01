### created: 03/17/2023
### last updated: 07/28/2023

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

### LOAD DATA ####
# summer flounder data 
sf_fall <- readRDS(here("data", "sumflounder", "sf_95fall_data.rds"))

# stratified mean
# sf_stratmu <- readRDS(here("data", "sumflounder", "sf_stratmu.rds")) |> 
#   group_by(EST_YEAR, TYPE, SEASON) |> 
#   mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper)) # if the upper quantile is NaN, replace with 0


# find the average over 12 years for comparison later
#sum(sf_stratmu$stratmu)/length(sf_stratmu$stratmu)


## WIND INCLUDED LOOP ####
# create storage vector for years 
x <- c()
# create storage data frame for stratified mean values 
ww.data <- data.frame()

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
for(i in seq(2022, 2009, -1)){ # count backwards by one starting at 2022 
  x <- append(x, i) # add i to the storage vector
  y <- sf_fall |> 
    filter(!EST_YEAR %in% x) |> # filter out x value from year
    stratified.mean() |> # calculate stratified mean
    mutate(STEP = (length(x)-2), # count each loop starting at -1 to represent the number of years removed
           TYPE = "Included")
  ww.data <- bind_rows(ww.data, y) # add each loop step the stratified means to the with wind dataframe
}

# 2020 was not surveyed for summer flounder so 2021 was removed twice, remove the duplicate of this time step while maintaining a sequence for number of years removed
ww.data <- filter(ww.data, !STEP == 0) |> 
  mutate(STEP = ifelse(STEP == -1, 0, STEP)) # replace -1 with 0, representing no years removed 

### save data 
saveRDS(ww.data, here("data", "sumflounder", "sf_fall-with_wind-system_rm.rds"))

## WIND PRECLUDED LOOP ####
# create storage vector for years 
x <- c()

# create storage dataframe for stratified mean values
wo.data <- data.frame()


### LOOP ####
for(i in seq(2022,2009, -1)){ # count backwards by one starting at 2022 
  x <- append(x, i) # add i to the storage vector
  y <- sf_fall |> 
    filter(EST_YEAR %in% c(x), AREA == "OUTSIDE") |> # filter out x value from year, and tows indicated as wind tows 
    stratified.mean() |> # calculate stratified mean
    mutate(STEP = (length(x)-2), # count each loop starting at -1 to represent the number of years removed
           TYPE = "Precluded")
  wo.data <- bind_rows(wo.data, y) # add each loop step the stratified means to the with wind dataframe
}

# filter out repeated values of 2021, keep where time step == 1 to represent 1 year precluded 
wo.data <- filter(wo.data, !STEP == 0)

### save data 
saveRDS(wo.data, here("data", "sumflounder", "sf_fall-without_wind-system_rm.rds"))

## COMBINE DATA ####
# bind with wind and without wind stratified mean values to the same dataframe 
all.data <- bind_rows(ww.data, wo.data) |>
  arrange(STEP)

### save data 
saveRDS(all.data, here("data", "sumflounder", "sf_fall-system_rm-fullset.rds"))

# averages <- all.data |>
#   group_by(STEP) |>
#   summarise(avg = mean(stratmu))
