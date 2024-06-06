### created: 12/10/2022
### last updated: 12/28/2023

# 01b - SPRING POSTERIOR PREDICTIVE CHECK: SIMULATE CHANGES IN ABUNDANCE INDICES OVER TIME ####

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
set.seed(154326)

post.check.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data/post-check"

### LOAD DATA ####
# best fit model , and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01b-spring-forecasts.R")
spring_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mod.rds"))

# extract fitted data 
spring_moddat <- spring_mod$data |> 
  mutate(EST_YEAR = YEAR, 
         AREA = as.character(AREA))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

# load the model predicted estimates of biomass created here("sdmtmb", "sumflounder", "02-mod-diagnostics", "03b-spring-mod-resids.R")
spring_mod_preds <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mod_preds.rds")) |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)), 
         AREA = as.character(AREA)) |> 
  select(CRUISE6, STATION, SEASON, STRATUM, EST_YEAR, AREA, AREA_CODE, TOWID, est)|> 
  mutate(EXPCATCHWT = exp(est))


## SIMULATE DATASETS ####
sim_spring <- simulate(spring_mod, nsim = 1000)

# create unique column names for each simulated dataset
colnames(sim_spring) <- paste0("sim", seq(1:1000))


### BIND DATASETS FOR CALCULATIONS ####
# bind summer flounder data with simulated data from model
spr_simdat <- spring_moddat |> 
  select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) |>
  bind_cols(sim_spring) |>
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") 

### save the data
saveRDS(spr_simdat, here(post.check.dat, "spr_simdat.rds"))


## CALCULATE STRATIFIED MEANS ###
# calculate the stratied mean abundance index for each scenario's replicate and year 

# read in stratified mean functions
source(here("R", "StratMeanFXs_v2.R")) 

spr_preds_ww <- spring_mod_preds |>
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(., strata)), 
          effort = "With Wind Included",
          SEASON = "FALL") |>
  select(!data) |> 
  unnest(cols = stratmu) |> 
  arrange(EST_YEAR)

spr_preds_wow <- spring_mod_preds |>
  filter(AREA == "OUTSIDE") |> 
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(., strata)), 
         effort = "With Wind Precluded",
         SEASON = "FALL") |>
  select(!data) |> 
  unnest(cols = stratmu) |> 
  arrange(EST_YEAR)

saveRDS(spr_preds_ww, here(post.check.dat, "spring_mod-pred_incl-stratmu.rds"))

saveRDS(spr_preds_wow, here(post.check.dat, "spring_mod-pred_precl-stratmu.rds"))

### WITH WIND INCLUDED ####
SprMu_ww <- spr_simdat |>
  group_by(nsim, SEASON, EST_YEAR) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(., strata)), 
         effort = "With Wind Included") |> 
  select(!data) |>
  unnest(cols = stratmu)

### save the data
saveRDS(SprMu_ww, here(post.check.dat, "spr_sim-ww-means.rds"))

### WITHOUT WIND (WIND PRECLUDED) ####
SprMu_wow <- spr_simdat  |> 
  filter(AREA == "OUTSIDE") |>
  group_by(nsim, SEASON, EST_YEAR) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(., strata)), 
         effort = "With Wind Precluded") |> 
  select(!data) |>
  unnest(cols = stratmu)

### save the data
saveRDS(SprMu_wow, here(post.check.dat, "spr_sim-wow-means.rds"))

## BIND STRATIFIED MEANS ####
### MODEL PREDICTED MEANS 
spr_pred_stratmu <- bind_rows(spr_preds_ww, spr_preds_wow)

### SIMULATED MEANS ####
spring_stratmu <- bind_rows(SprMu_ww, SprMu_wow)

### save the data
saveRDS(spr_pred_stratmu, here(post.check.dat, "spring_pred-stratmu.rds"))
saveRDS(spring_stratmu, here(post.check.dat, "spring_sim-stratmu.rds"))

## FIT LINEAR REGRESSIONS ####
### MODEL-PREDICTED TREND ####
# estimate the change in model predicted abundance index over time for each scenario
spr_pred_lms <- spr_pred_stratmu |> 
  group_by(effort) |> 
  nest() |> 
  mutate(mods = map(data, ~lm(stratmu~EST_YEAR, data = .)), 
         coefs = map(mods, broom::tidy, conf.int = TRUE), 
         SEASON = "FALL") |> 
  unnest(cols = coefs) |> 
  select(!c(data, mods)) |> 
  filter(term == "EST_YEAR")

### save the data 
saveRDS(spr_pred_lms, file = here(post.check.dat, "spring_predicted_slopes.rds"))

### SIMULATED TREND ####
# estimate the simulated change in abundance index over time for each scenario
spring_lms <- spring_stratmu |>
  group_by(nsim, SEASON, effort) |> 
  nest() |> 
  # select(nsim, stratmu, SEASON, effort) |> 
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .))) |>  
  mutate(coef = map(model, broom::tidy, conf.int = TRUE)) |> 
  unnest(cols = coef) |>
  select(nsim, term, estimate, conf.low, conf.high, SEASON, effort) |>
  filter(term == "EST_YEAR") 

### save the data 
saveRDS(spring_lms, file = here(post.check.dat, "spring_sim_slopes.rds"))



