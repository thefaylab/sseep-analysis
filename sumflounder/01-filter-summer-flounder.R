### created: 07/24/2023
### last updated: 

# 01 - FILTER SUMMER FLOUNDER DATA ####


## OBJECTIVE ####
# save copies of only summer flounder data for future analyses


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


### LOAD AND FILTER DATA ####

# raw data based on cumulative distribution filter created here("tidy-data", "05b-spatial-filter-data.R")
data <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |> 
  filter(SVSPP == 103)


# stratified mean abundance indices created here("retro-analysis") 
sf_stratmu <- readRDS(here("data", "rds", "retro-analysis", "strat-mu_all.rds")) |> 
  filter(SVSPP == 103)


# mean squared relative difference of abundance indices created here("retro-analysis", "03-strat-mu-diff.R")
sf_mudiff <- readRDS(file = here("data", "rds", "species_mean-sq-diff.rds")) |> 
  filter(SVSPP == 103)


# linear regression slope estimates for summer flounder for change in abudance indices over time for each scenario
sf_slopes <- readRDS(here("retro-analysis", "active_strata_wts_only", "obs-stratmu-slopes.rds")) |>
  filter(SVSPP == 103)

### save the data 
saveRDS(data, here("data", "sumflounder", "sf_95bts_data.rds"))
saveRDS(sf_stratmu, here("data", "sumflounder", "sf_stratmu.rds"))
saveRDS(sf_mudiff, here("data", "sumflounder", "sf_mudiff.rds"))
saveRDS(sf_slopes, here("data", "sumflounder", "sf_obs-slopes.rds"))
