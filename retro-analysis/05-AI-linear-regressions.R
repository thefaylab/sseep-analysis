### created: 07/24/2022
### last updated: 01/29/2024

# 05 - LINEAR REGRESSION OF ABUNDANCE INDICES ####

## OBJECTIVE ####
# fit linear regression models of annual stratified means for each species over time
# extract slope estimates


### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

retro.analysis <- here("data", "rds", "retro-analysis")

### LOAD DATA ####
# stratified mean abundance indices created here("retro-analysis") 
stratmeans <- readRDS(here(retro.analysis, "strat-mu_all.rds")) 


## PERFORM LINEAR REGRESSION #####
# fit a linear model to the stratified mean abundance indices over time for each species and season 
stratmu_mods <- stratmeans |> 
  group_by(SVSPP, COMNAME, SEASON, TYPE) |> 
  nest() |> # create a list for each species, season, and scenario
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .x))) |> # map the linear model function to each data object with stratified mean as a function of year
  mutate(coefs = map(model, broom::tidy, conf.int = TRUE)) |> # extract the coefficients from the model objects
  unnest(coefs) # convert the coefficient lists to columns added to the dataframe for each variable

# filter for the slope estimates predicted by each species, season, and scenario's linear model
slopes <- stratmu_mods |> 
  select(SVSPP, COMNAME, SEASON, TYPE, term, estimate, conf.low, conf.high) |>
  filter(term == "EST_YEAR") |> 
  ungroup() |>
  arrange(estimate)

### save the data 
saveRDS(stratmu_mods, here(retro.analysis, "obs-stratmu-mods.rds"))
saveRDS(slopes, here(retro.analysis, "obs-stratmu-slopes.rds"))
