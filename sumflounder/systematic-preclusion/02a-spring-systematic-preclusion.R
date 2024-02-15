### created: 03/17/2023
### last updated: 02/14/2024

# 02a - SYSTEMATIC PRECLUSION: SPRING ####

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

system.precl.dat <- here("data", "sumflounder", "systematic-preclusion")
sumflounder.dat <- here("data", "sumflounder")

### LOAD DATA ####
# summer flounder data 
sf_spring <- readRDS(here(sumflounder.dat, "sumflounder_spring.rds"))

#active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))


# filter for summer flounder data only
# sf_stratmean <- readRDS(here("data", "sumflounder", "sf_stratmu.rds")) |>  
#   group_by(EST_YEAR, TYPE, SEASON) %>% 
#   mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) %>% # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper)) # if the upper quantile is NaN, replace with 0

# find the average over 12 years for comparison later
# sum(sf_stratmean$stratmu)/length(sf_stratmean$stratmu)


## WIND INCLUDED LOOP ####
# create storage vector for years 
x <- c()

# create storage data frame for stratified mean values 
ww.data <- data.frame()
# create year object for the years in the dataframe to filter for in the loop 
years <- c(2022, sort(unique(sf_spring$EST_YEAR), decreasing = TRUE))

### TEST LOOP ###
# test <- strata.mean(sf_spring)
# 
# x <- NULL
# x <- append(x, 2022)
# 
# y <- sf_spring |> 
#   filter(EST_YEAR %in% x) |>
#   strata.mean() |>
#   mutate(step = length(x))
# ww.data <- bind_rows(all.data, y)
# 
# y <- sf_spring |> 
#   filter(!EST_YEAR %in% x) |>
#   strata.mean() |>
#   mutate(step = length(x))
# wo.data <- bind_rows(wo.data, y)



### LOOP #### 
for(i in years){ # count backwards by one starting at 2022 
  x <- append(x, i) # add i to the storage vector
  y <- sf_spring |> 
    filter(!EST_YEAR %in% x) |> # filter out x value from year
    group_by(EST_YEAR) |> 
    nest() |> 
    mutate(stratmean = map(data, ~stratified.mean(., strata))) |> # calculate stratified mean
    dplyr::select(!data) |> 
    unnest(cols = stratmean) |>
    mutate(STEP = (length(x) - 1), # count each loop starting at -1 to represent the number of years removed
           effort = "With Wind Included")
  ww.data <- bind_rows(ww.data, y) |> # add each loop step the stratified means to the with wind dataframe
    arrange(STEP, EST_YEAR)
}

# remove NA column 
ww.data <- ww.data |> dplyr::select(!stratmean)


# calculate stratified mean with all twelve years with wind included 
# full.strats <- sf_spring |>
#   strata.mean() |>
#   mutate(STEP = 0, 
#          TYPE = "Included") 
# 
# # add to looped dataframe as step 1 
# ww.data <-  bind_rows(ww.data, full.strats) |>
#   arrange(STEP)

### save data 
saveRDS(ww.data, here(system.precl.dat, "sf_spr-with_wind-system_rm.rds"))

## WIND PRECLUDED LOOP ####
# create storage vector for years 
x <- c()

# create storage dataframe for stratified mean values
wo.data <- data.frame()


### LOOP ####
for(i in years){ # count backwards by one starting at 2021 
  x <- append(x, i) # add i to the storage vector
  y <- sf_spring |> 
    filter(EST_YEAR %in% c(x), AREA == "OUTSIDE") |> # filter out x value from year
    group_by(EST_YEAR) |> 
    nest() |> 
    mutate(stratmean = map(data, ~stratified.mean(., strata))) |> # calculate stratified mean
    dplyr::select(!data) |> 
    unnest(cols = stratmean) |>
    mutate(STEP = length(x)-1, # count each loop starting at 1 to represent the number of years removed
           effort = "With Wind Precluded")
  wo.data <- bind_rows(wo.data, y) # add each loop step the stratified means to the with wind dataframe
}

# remove NA column 
wo.data <- wo.data |> dplyr::select(!stratmean)


### save data 
saveRDS(wo.data, here(system.precl.dat, "sf_spr-without_wind-system_rm.rds"))


## COMBINE DATA ####
# bind with wind and without wind stratified mean values to the same dataframe 
all.data <- bind_rows(ww.data, wo.data) |>
  arrange(STEP)

### save data 
saveRDS(all.data, here(system.precl.dat, "sf_spr-system_rm-fullset.rds"))

# averages <- all.data |>
#   group_by(STEP) |>
#   summarise(avg = mean(stratmu))
