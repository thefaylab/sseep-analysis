### created: 02/08/2024
### last updated: 04/25/2025

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

## Environment set up
# Set the years of complete survey effort (n >= 350)
complete_fall_yrs <- c(2009:2016, 2018, 2019, 2021)
complete_spr_yrs <- c(2009:2019, 2021)

### Read in data ####
# strata and year combinations that will be removed during the stratified mean calculations because only one tow was conducted. created from `05b-spatial-filter-data.R` here("tidy-data").
one_tow_strata <- readRDS(here(dat.files, "one_tow_strata.rds"))

# dataset created from `05b-spatial-filter-data.R` here("tidy-data"). Contains complete observations for each species and unique tow filtered based on 95% cumulative distribution of biomass.  
data <- readRDS(here(dat.files, "95filtered_complete_bts.rds")) 

# species dataframe for adding to final dataset 
species <- readRDS(here(dat.files, "95filtered-species.rds"))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here(dat.files, "active_strata_wts.rds"))

## DATA WRANGLE ####
# create data frames of year and strata combinations where only one tow occurred
### Status quo effort 
remove_incl_strata <- one_tow_strata |> 
  select(!precl_strata) |> unnest(cols = incl_strata)
### Wind-precluded effort
remove_precl_strata <- one_tow_strata |> 
  select(!incl_strata) |> unnest(cols = precl_strata)

# Filter out incomplete survey years as well as year and strata combinations where only one tow occurred
## Status quo effort
incl_data <- data |>
  anti_join(remove_incl_strata, by = c("SEASON", "EST_YEAR", "STRATUM")) |> 
  group_by(SEASON, SVSPP) |> 
  nest() |> 
  mutate(seasonal_data = case_when( #create a new nested column where
    # fall season data only contains complete survey years
    SEASON == "FALL" ~ map(data, ~filter(., EST_YEAR %in% complete_fall_yrs)),
    # spring season data only contains complete survey years
    SEASON == "SPRING" ~ map(data, ~filter(., EST_YEAR %in% complete_spr_yrs))),
    # find years where species is not observed
    sums = map(seasonal_data, ~group_by(., EST_YEAR) |> 
                                       summarise(mu_wt = mean(EXPCATCHWT))|>
                                       filter(mu_wt == 0)),
    # filter out years where there are no observations
    filtered_data = map2(seasonal_data, sums, ~anti_join(.x, .y, by = c("EST_YEAR")))) |>
  select(!c(data, seasonal_data, sums)) |> 
  unnest(cols = filtered_data)

## Wind-precluded effort
precl_data <- data |>
  filter(AREA == "OUTSIDE") |>
  anti_join(remove_precl_strata, by = c("SEASON", "EST_YEAR", "STRATUM")) |>
  group_by(SEASON, SVSPP) |> 
  nest() |> 
  mutate(seasonal_data = case_when( #create a new nested column where
    # fall season data only contains complete survey years
    SEASON == "FALL" ~ map(data, ~filter(., EST_YEAR %in% complete_fall_yrs)),
    # spring season data only contains complete survey years
    SEASON == "SPRING" ~ map(data, ~filter(., EST_YEAR %in% complete_spr_yrs))),
    # find years where species is not observed
    sums = map(seasonal_data, ~group_by(., EST_YEAR) |> 
                 summarise(mean_wt = mean(EXPCATCHWT))|>
                 filter(mean_wt == 0)),
    # filter out years where there are no observations
    filtered_data = map2(seasonal_data, sums, ~anti_join(.x, .y, by = c("EST_YEAR")))) |>
  select(!c(data, seasonal_data, sums)) |> 
  unnest(cols = filtered_data)

## CALCULATE STRATIFIED MEANS ####
# calculate the abundance indices under stratus quo survey effort 

### With wind included ####
incl_stratmu <- incl_data |>
  group_by(SVSPP, SEASON, EST_YEAR) |> 
  nest() |>  # nest the data by species, season and year 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> # find the stratified mean using the nested data
  dplyr::select(!c(data)) |> 
  unnest(cols = stratmu) |> 
  mutate(effort = "Status quo survey effort")

### With wind precluded ####
precl_stratmu <- precl_data |>
  group_by(SVSPP, SEASON, EST_YEAR) |>
  nest() |>   # nest the data by species, season and year 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  dplyr::select(!data) |>
  unnest(cols = stratmu) |> 
  mutate(effort = "Wind-precluded survey effort")

## BIND DATA #### 
# long dataframe of stratmeans
stratmeans_rows <- bind_rows(incl_stratmu, precl_stratmu) |> #bind rows of the two datasets 
  left_join(species, by = "SVSPP") |> # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)

# wide dataframe of stratmeans
stratmeans_cols <- right_join(incl_stratmu, precl_stratmu, by = c("SVSPP", "SEASON", "EST_YEAR")) |> 
  left_join(species, by = "SVSPP") |> # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)

## ESTIMATE POPULATION TRENDS ####
stratmu_linreg <- stratmeans_rows |> 
  filter(SVSPP != 979) |> # only one year of data for this "species" after above steps; and is "Unknown 02"
  group_by(SEASON, effort, SVSPP, COMNAME) |> 
  nest() |> 
  mutate(model = map(data, ~lm(stratmu~EST_YEAR, data = .)), 
         coefs = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(coefs) |>
  select(SEASON, SVSPP, COMNAME, effort, term, estimate, conf.low, conf.high) |> 
  filter(term == "EST_YEAR")

# wrangle to calculate MRE and MARE
### Wind-precluded trends
precl_linreg <- stratmu_linreg |> 
  filter(effort == "Wind-precluded survey effort")
### Status quo trends
incl_linreg <- stratmu_linreg |> 
  filter(effort == "Status quo survey effort")

linreg_cols <- left_join(incl_linreg, precl_linreg, by = c("SEASON", "SVSPP", "COMNAME"))


## CALCULATE RELATIVE PERCENT DIFFERENCE ####
mudiff <- stratmeans_cols |> 
  group_by(SVSPP, SEASON, COMNAME) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = stratmu.x, observed = stratmu.y)), # calculate the relative errors 
         year_sel = map(errors, ~arrange(., EST_YEAR) |> tail(5)), # select most recent 5 years
         mudiff = map(year_sel, ~mean.diff(., group_by = NULL))) |> # calculate the mean percent difference
  dplyr::select(!c(data, errors, year_sel)) |> 
  unnest(cols = mudiff)

cvdiff <-  stratmeans_cols |> 
  group_by(SVSPP, SEASON, COMNAME) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = cv.x, observed = cv.y)), # calculate the relative errors 
         year_sel = map(errors, ~arrange(., EST_YEAR) |> tail(5)), # select most recent 5 years
         cv.diff = map(year_sel, ~mean.diff(., group_by = NULL))) |> # calculate the mean percent difference; NULL drops any groups remaining on the df
  dplyr::select(!c(data, errors, year_sel)) |> 
  unnest(cols = cv.diff)

linreg_diff <- linreg_cols |> 
  group_by(SVSPP, SEASON, COMNAME) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = estimate.x, observed = estimate.y)), # calculate the relative errors 
         slope.diff = map(errors, ~mean.diff(., group_by = NULL))) |> # calculate the mean percent difference
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

