### created: 07/24/2023
### last updated: 12/05/2023

# 04 - FILTER SUMMER FLOUNDER DATA ####


## OBJECTIVE ####
# filter observations of summer flounder from the bottom trawl dataset based on the strata identified in the seasonal spatial footprints. 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


### LOAD AND FILTER DATA ####

# raw data based on cumulative distribution filter created here("tidy-data", "05b-spatial-filter-data.R")
# data <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |>
#   filter(SVSPP == 103)

# expanded bts data created here("tidy-data", "04-complete-datasets.R")
sumflounder_complete <- readRDS(here("data", "rds", "completed_bts_data.rds")) |> filter(SVSPP == 103) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# fall footprint
fall_footprint <- readRDS(here("data", "sumflounder", "sf_fall_footprint.rds"))
fall_strata <- unique(fall_footprint$STRATUM)

# spring footprint
spring_footprint <- readRDS(here("data", "sumflounder", "sf_spring_footprint.rds"))
spring_strata <- unique(spring_footprint$STRATUM)


## FILTER DATA ####
# fall data 
sf_fall <- sumflounder_complete |> 
  filter(SEASON == "FALL", STRATUM %in% fall_strata, YEAR != 2017, !AVGDEPTH > 150)

#spring data
sf_spring <- sumflounder_complete |> 
  filter(SEASON == "SPRING", STRATUM %in% spring_strata, YEAR != 2020)

# full dataset 
sumflounder <- bind_rows(sf_spring, sf_fall)


# stratified mean abundance indices created here("retro-analysis") 
# sf_stratmu <- readRDS(here("data", "rds", "retro-analysis", "strat-mu_all.rds")) |> 
#   filter(SVSPP == 103)
# 
# 
# # mean squared relative difference of abundance indices created here("retro-analysis", "03-strat-mu-diff.R")
# sf_mudiff <- readRDS(file = here("data", "rds", "species_mean-sq-diff.rds")) |> 
#   filter(SVSPP == 103)
# 
# 
# # linear regression slope estimates for summer flounder for change in abudance indices over time for each scenario
# sf_slopes <- readRDS(here("retro-analysis", "active_strata_wts_only", "obs-stratmu-slopes.rds")) |>
#   filter(SVSPP == 103)

### save the data 
saveRDS(sumflounder, here("data", "sumflounder", "sumflounder.rds"))
saveRDS(sf_fall, here("data", "sumflounder", "sumflounder_fall.rds"))
saveRDS(sf_spring, here("data", "sumflounder", "sumflounder_spring.rds"))
# saveRDS(sf_stratmu, here("data", "sumflounder", "sf_stratmu.rds"))
# saveRDS(sf_mudiff, here("data", "sumflounder", "sf_mudiff.rds"))
# saveRDS(sf_slopes, here("data", "sumflounder", "sf_obs-slopes.rds"))
