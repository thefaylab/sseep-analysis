### created: 07/27/2023
### last updated: 

#  03 - NULL HYPOTHESIS TESTING: LINEAR REGRESSIONS OF STRATIFIED MEANS  ####

## OBJECTIVE ####
# Each replicate and stratified mean abundance index were fit to a linear regression model to find the change in abundance index over time for each .

### LOAD PACKAGES ####
library(tidyverse)
library(here)
library(patchwork)
library(boot)
library(infer)
library(broom)
source(here("R", "StratMeanFXs_v2.R"))

set.seed(120)


### LOAD DATA ####
# "No preclusion" dataset containing 1000 replicates of summer flounder data resampled for 100% of the historical observations created here("sumflounder", "01-data-resample.R")
fullreps_mu <- readRDS(here("data", "sumflounder", "full_resamp_stratmu.rds"))

# "Preclusion" dataset containing 1000 replicates of summer flounder data resampled for 90% of the historical observations created here("sumflounder", "01-data-resample.R")
windreps_mu <- readRDS(here("data", "sumflounder", "wind_resamp_stratmu.rds"))



## LINEAR REGRESSIONS OF ABUNDANCE INDICES ####
### WITH WIND INCLUDED ####
fullreps_mod <- fullreps_mu |>
  group_by(SEASON) |>
  mutate(model = map(stratmean, ~lm(stratmu ~ EST_YEAR, data = .)),
         coef = map(model, broom::tidy, conf.int = TRUE), 
         TYPE = "With Wind Included") |>
  unnest(coef) |>
  select(replicate, term, estimate, conf.low, conf.high, TYPE) |>
  filter(term == "EST_YEAR")




### WITH WIND PRECLUDED ####
windreps_mod <- windreps_mu |>
  group_by(SEASON) |>
  mutate(model = map(stratmean, ~lm(stratmu ~ EST_YEAR, data = .)),
         coef = map(model, broom::tidy, conf.int = TRUE), 
         TYPE = "With Wind Precluded") |>
  unnest(coef) |>
  select(replicate, term, estimate, conf.low, conf.high, TYPE) |>
  filter(term == "EST_YEAR") 




### save the data
saveRDS(fullreps_mod, here("data", "sumflounder", "fullresamp_mods.rds"))
saveRDS(windreps_mod, here("data", "sumflounder", "windresamp_mods.rds"))
