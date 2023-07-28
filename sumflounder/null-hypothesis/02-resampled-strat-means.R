### created: 07/27/2023
### last updated: 

#  02 - NULL HYPOTHESIS TESTING: STRATIFIED MEANS OF RESAMPLED DATA  ####

## OBJECTIVE ####
# calculate stratified means for each season, and replicate combination based on the resampled data.  


### LOAD PACKAGES ####
library(tidyverse)
library(here)
library(sf)
library(patchwork)
library(boot)
library(infer)
library(broom)
source(here("R", "StratMeanFXs_v2.R"))

set.seed(120)

### LOAD DATA ####
# "Wind Included" dataset containing 1000 replicates of summer flounder data resampled for 100% of the historical observations created here("sumflounder", "01-data-resample.R")
full_reps <- readRDS(here("data", "sumflounder", "fulldat_resample.rds"))

# "Wind Precluded" dataset containing 1000 replicates of summer flounder data resampled for 90% of the historical observations created here("sumflounder", "01-data-resample.R")
wind_reps <- readRDS(here("data", "sumflounder", "winddat_resample.rds"))

## STRATIFIED MEAN CALCULATIONS ####

### WITH WIND INCLUDED ####
fullreps_mu <- full_reps |>
  group_by(replicate, SEASON) |>
  nest() |>
  mutate(stratmean = map(data, ~stratified.mean(.))) |>
  select(-data)



### WITH WIND PRECLUDED ####
windreps_mu <- wind_reps |>
  group_by(replicate, SEASON) |>
  nest() |>
  mutate(stratmean = map(data, ~stratified.mean(.))) |>
  select(-data)



### save the data
saveRDS(fullreps_mu, here("data", "sumflounder", "full_resamp_stratmu.rds"))
saveRDS(windreps_mu, here("data", "sumflounder", "wind_resamp_stratmu.rds"))


