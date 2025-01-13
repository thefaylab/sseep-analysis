### created: 02/08/2024
### last updated: 11/10/2024

# 01 - CALCULATE RETROSPECTIVE INDICES: WITH AND WITHOUT PRECLUSION ####


## OBJECTIVE ####
# calculate stratified means for each species, strata, and year combination based on the historical time series data and on strata that appear in the spatially filtered trawl data only
# consolidates all scripts in /retro-analysis/old folder by calling on stratified mean functions 
#
# script will: 
# calculate the status quo stratified mean abundance index using all available catch rates
# calcaulte a wind-precluded stratified mean abundance index by removing wind tows prior to calculation
# calculate the difference between the two indices to find the relative errors and mean percent difference
#


### Load packages ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(gt)
source(here("R", "StratMeanFXs_v2.R"))

### File locations ####
dat.files <- here("data", "rds")

### Read in data ####
# dataset created from `05b-spatial-filter-data.R` here("tidy-data"). Contains complete observations for each species and unique tow filtered based on 95% cumulative distribution of biomass. 
data <- readRDS(here(dat.files, "95filtered_complete_bts.rds")) |> filter(EST_YEAR != 2020)

# species dataframe for adding to final dataset 
species <- readRDS(here(dat.files, "95filtered-species.rds"))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here(dat.files, "active_strata_wts.rds"))

## CALCULATE STRATIFIED MEANS ####
### With wind included ####
incl_stratmu <- data |>
  group_by(SVSPP, SEASON, EST_YEAR) |> 
  nest() |>  # nest the data by species, season and year 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> # find the stratified mean using the nested data
  dplyr::select(!data) |>
  unnest(cols = stratmu) |> 
  mutate(effort = "With Wind Included")

### With wind precluded ####
precl_stratmu <- data |>
  filter(AREA == "OUTSIDE") |>  #  filter for tows occurring outside wind areas only
  group_by(SVSPP, SEASON, EST_YEAR) |>
  nest() |>   # nest the data by species, season and year 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  dplyr::select(!data) |>
  unnest(cols = stratmu) |> 
  mutate(effort = "With Wind Precluded")

## BIND DATA #### 
# long dataframe of stratmeans
stratmeans_rows <- bind_rows(incl_stratmu, precl_stratmu) |> #bind rows of the two datasets 
  left_join(species, by = "SVSPP") |> # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)

# wide dataframe of stratmeans
stratmeans_cols <- left_join(incl_stratmu, precl_stratmu, by = c("SVSPP", "SEASON", "EST_YEAR")) |> 
  left_join(species, by = "SVSPP") |> # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)

## ESTIMATE POPULATION TRENDS ####
stratmu_linreg <- stratmeans_rows |> 
  group_by(SEASON, effort, SVSPP, COMNAME) |> 
  nest() |> 
  mutate(model = map(data, ~lm(stratmu~EST_YEAR, data = .)), 
         coefs = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(coefs) |>
  select(SEASON, SVSPP, COMNAME, effort, term, estimate, conf.low, conf.high) |> 
  filter(term == "EST_YEAR")

# wrangle to calculate MRE and <MARE
precl_linreg <- stratmu_linreg |> 
  filter(effort == "With Wind Precluded")
incl_linreg <- stratmu_linreg |> 
  filter(effort == "With Wind Included")
linreg_cols <- left_join(incl_linreg, precl_linreg, by = c("SEASON", "SVSPP", "COMNAME"))


## CALCULATE RELATIVE PERCENT DIFFERENCE ####
mudiff <- stratmeans_cols |> 
  group_by(SVSPP, SEASON, COMNAME) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = stratmu.x, observed = stratmu.y)), # calculate the relative errors 
         mudiff = map(errors, ~tail(., 5) |> mean.diff())) |> # calculate the mean percent difference
  dplyr::select(!c(data, errors)) |> 
  unnest(cols = mudiff)

cvdiff <-  stratmeans_cols |> 
  group_by(SVSPP, SEASON, COMNAME) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = cv.x, observed = cv.y)), # calculate the relative errors 
         cv.diff = map(errors, ~tail(., 5) |> mean.diff())) |> # calculate the mean percent difference
  dplyr::select(!c(data, errors)) |> 
  unnest(cols = cv.diff)

linreg_diff <- linreg_cols |> 
  group_by(SVSPP, SEASON, COMNAME) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = estimate.x, observed = estimate.y)), # calculate the relative errors 
         slope.diff = map(errors, ~tail(., 5) |> mean.diff())) |> # calculate the mean percent difference
  dplyr::select(!c(data, errors)) |> 
  unnest(cols = slope.diff)



## SAVE THE DATA ####
save.data <- list("stratmu_included.rds" = incl_stratmu, 
                  "stratmu_precluded.rds" = precl_stratmu,
                  "all_stratmu_rows.rds" = stratmeans_rows,
                  "all_stratmu_cols.rds" = stratmeans_cols,
                  "stratmu_slopes_rows.rds" = stratmu_linreg,
                  "stratmu_slopes_cols.rds" = linreg_cols,
                  "species_stratmu-diff.rds" = mudiff, 
                  "species_cv-diff.rds" = cvdiff,
                  "species_slope-diff.rds" = linreg_diff)

pmap(list(save.data, names(save.data)), ~saveRDS(.x, here(dat.files, "retro-analysis", .y)))

