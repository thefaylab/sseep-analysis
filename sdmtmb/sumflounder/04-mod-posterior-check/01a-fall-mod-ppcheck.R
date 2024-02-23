### created: 12/10/2022
### last updated: 02/22/2024

# 01a - FALL POSTERIOR PREDICTIVE CHECK: SIMULATE CHANGES IN ABUNDANCE INDICES OVER TIME ####

## OBJECTIVE ####
# simulate new data with fitted model 
# bind to summer flounder survey data 
# calculate abundance index and change in abundance index over time


### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)

post.check.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data/post-check"

### LOAD DATA ####
# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

# extract fitted data 
fall_moddat <- fall_mod$data |> 
  mutate(EST_YEAR = YEAR, 
         AREA = as.character(AREA))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

# load the model predicted estimates of biomass created here("sdmtmb", "sumflounder", "02-mod-diagnostics", "03a-fall-mod-resids.R")
fall_mod_preds <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod_preds.rds")) |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)), 
         AREA = as.character(AREA)) |> 
  select(CRUISE6, STATION, SEASON, STRATUM, EST_YEAR, AREA, AREA_CODE, TOWID, est)|> 
  mutate(EXPCATCHWT = exp(est))

## SIMULATE DATASETS ####
sim_fall <- simulate(fall_mod, nsim = 1000)

# create unique column names 
colnames(sim_fall) <- paste0("sim", seq(1:1000))


### BIND DATASETS FOR CALCULATIONS ####
# bind summer flounder data with simulated data from m15a model
fall_simdat <- fall_moddat |> 
  select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) |>
  bind_cols(sim_fall) |>
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") 


# save the data
saveRDS(fall_simdat, here(post.check.dat, "fall_simdat.rds"))


## CALCULATE STRATIFIED MEANS ####

# read in stratified mean functions
source(here("R", "StratMeanFXs_v2.R")) 

### MODEL PREDICTED ESTIMATES ####
preds_ww <- mod_preds |>
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(., strata)), 
         effort = "With Wind Included",
         SEASON = "FALL") |>
  select(!data) |> 
  unnest(cols = stratmu) |> 
  arrange(EST_YEAR)

preds_wow <- mod_preds |>
  filter(AREA == "OUTSIDE") |> 
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(., strata)), 
         effort = "With Wind Precluded",
         SEASON = "FALL") |>
  select(!data) |> 
  unnest(cols = stratmu) |> 
  arrange(EST_YEAR)

saveRDS(preds_ww, here(post.check.dat, "fall_mod-pred_incl-stratmu.rds"))

saveRDS(preds_wow, here(post.check.dat, "fall_mod-pred_precl-stratmu.rds"))

### SIMULATED ESTIMATES ####
# calculate the stratified mean abundance index for each scenario's replicate and year 
#### WITH WIND  ####
FallMu_ww <- fall_simdat |>
  group_by(nsim, SEASON, EST_YEAR) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(., strata)),
         effort = "With Wind Included") |>
  select(!data) |> 
  unnest(cols = stratmu)

### save the data
saveRDS(FallMu_ww, here(post.check.dat, "fall_sim-ww-means.rds"))

#### WITHOUT WIND (WIND PRECLUDED) ####
FallMu_wow <- fall_simdat |> 
  filter(AREA == "OUTSIDE") |>
  group_by(nsim, SEASON, EST_YEAR) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(., strata)), 
         effort = "With Wind Precluded") |>
  select(!data) |>
  unnest(cols = stratmu)

### save the data
saveRDS(FallMu_wow, here(post.check.dat, "fall_sim-wow-means.rds"))


## BIND STRATIFIED MEANS ####
### MODEL PREDICTED MEANS 
pred_stratmu <- bind_rows(preds_ww, preds_wow)

### SIMULATED MEANS ####
fall_stratmu <- bind_rows(FallMu_ww, FallMu_wow)

### save the data
saveRDS(pred_stratmu, here(post.check.dat, "fall_pred-stratmu.rds"))

saveRDS(fall_stratmu, here(post.check.dat, "fall_sim-stratmu.rds"))


## FIT LINEAR REGRESSIONS ####
### MODEL-PREDICTED TREND ####
# estimate the change in model predicted abundance index over time for each scenario
pred_lms <- pred_stratmu |> 
  group_by(effort) |> 
  nest() |> 
  mutate(mods = map(data, ~lm(stratmu~EST_YEAR, data = .)), 
         coefs = map(mods, broom::tidy, conf.int = TRUE), 
         SEASON = "FALL") |> 
  unnest(cols = coefs) |> 
  select(!c(data, mods)) |> 
  filter(term == "EST_YEAR")

### save the data 
saveRDS(pred_lms, file = here(post.check.dat, "fall_predicted_slopes.rds"))

### SIMULATED TREND ####
# estimate the simulated change in abundance index over time for each scenario
fall_lms <- fall_stratmu |>
  group_by(nsim, SEASON, effort) |> 
  nest() |> 
  #select(nsim, stratmu, SEASON, TYPE) |> 
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)),
         coef = map(model, broom::tidy, conf.int = TRUE)) |> 
  unnest(cols = coef) |>
  select(nsim, term, estimate, conf.low, conf.high, SEASON, effort) %>%
  filter(term == "EST_YEAR") 

### save the data 
saveRDS(fall_lms, file = here(post.check.dat, "fall_slopes.rds"))



