### created: 02/08/2024
### last updated: 

# consolidate retro analysis ####


## OBJECTIVE ####

# consolidate all scripts in retro-analysis folder by calling on stratified mean functions 
# calculate stratified mean based on strata that appear in the spatially filtered trawl data only


### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
source(here("R", "StratMeanFXs_v2.R"))


### LOAD DATA ####
# dataset created from `05b-spatial-filter-data.R` here("tidy-data"). Contains complete observations for each species and unique tow filtered based on 95% cumulative distribution of biomass. 
data <- readRDS(here("data", "rds", "95filtered_complete_bts.rds"))

# species dataframe for adding to final dataset 
species <- readRDS(here("data", "rds", "95filtered-species.rds"))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

## CALCULATE STRATIFIED MEANS ####
### With wind included ####
incl_stratmu <- data |>
  group_by(SVSPP, SEASON) |>
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  dplyr::select(!data) |>
  unnest(cols = stratmu) |> 
  mutate(Type = "With Wind Included")

### With wind precluded ####
precl_stratmu <- data |>
  filter(AREA == "OUTSIDE") |> 
  group_by(SVSPP, SEASON) |>
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  dplyr::select(!data) |>
  unnest(cols = stratmu) |> 
  mutate(Type = "With Wind Precluded")

## BIND DATA #### 
stratmeans <- bind_rows(incl_stratmu, precl_stratmu) |> #bind rows of the two datasets
  left_join(species, by = "SVSPP") |> # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)


## CALCULATE RELATIVE PERCENT DIFFERENCE ####
mudiff <- stratmeans |> 
  group_by(SVSPP, SEASON) |> 
  nest() |> 
  mutate(mudiff = map(data, ~mean.diff(.))) |> 
  dplyr::select(!data) |> 
  unnest(cols = mudiff)




