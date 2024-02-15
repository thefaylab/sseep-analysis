### created: 01/29/2024
### last updated: 02/15/2024

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

null.hyp.data <- here("data", "atlmackerel", "null-hypothesis")

set.seed(120)

### LOAD DATA ####
# "Wind Included" dataset containing 1000 replicates of Atlantic mackerel data resampled for 100% of the historical observations created here("atlmackerel", "01-data-resample.R")
full_reps <- readRDS(here(null.hyp.data, "fulldat_resample.rds"))

# "Wind Precluded" dataset containing 1000 replicates of Atlantic mackerel data resampled for 90% of the historical observations created here("atlmackerel", "01-data-resample.R")
wind_reps <- readRDS(here(null.hyp.data, "winddat_resample.rds"))

#active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

## STRATIFIED MEAN CALCULATIONS ####

### WITH WIND INCLUDED ####
fullreps_mu <- full_reps |>
  group_by(replicate, SEASON, EST_YEAR) |>
  nest() |>
  mutate(stratmean = map(data, ~stratified.mean(., strata))) |>
  select(-data) |> 
  unnest(cols = stratmean)



### WITH WIND PRECLUDED ####
windreps_mu <- wind_reps |>
  group_by(replicate, SEASON, EST_YEAR) |>
  nest() |>
  mutate(stratmean = map(data, ~stratified.mean(., strata))) |>
  select(-data) |> 
  unnest(cols = stratmean)



### save the data
saveRDS(fullreps_mu, here(null.hyp.data, "full_resamp_stratmu.rds"))
saveRDS(windreps_mu, here(null.hyp.data, "wind_resamp_stratmu.rds"))


