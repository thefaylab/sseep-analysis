### created: 07/24/2023
### last updated: 

# 02b -  DATA ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"
source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
#set.seed(123)

## LOAD DATA ####
# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

fall_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "fall_predictions.rds"))

# here("sdmtmb", "sumflounder", "05-simulations", "01a-prepare-fall-sim-dat.R")
future_data <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "fall_future_data.rds"))

# here("sdmtmb", "sumflounder", "05-simulations", "01a-prepare-fall-sim-dat.R")
fall_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "fall_future_mesh.rds"))

# here("sdmtmb", "sumflounder", "data")
stratmeans <- readRDS(here("data", "sumflounder", "sf_stratmu.rds")) |> 
  filter(SEASON == "FALL", TYPE == "With Wind Included") |>
  select(!c(COMNAME, SCINAME, TYPE, SEASON))


### DATA WRANGLE ####

tidy(fall_mod)
tidy(fall_mod, "ran_pars")

fall_info <- future_data |>
  ungroup() |>
  dplyr::select(STATION, STRATUM, CRUISE6, AREA)

fall_omegas <- mean(fall_preds$omega_s)
fixed_re <-  list(fall_omegas, epsilon_st = NULL, zeta_s = NULL)


## SIMULATE INCREASE SCENARIO ####
# create empty storage data frame
increase_sims <- data.frame()

# loop 
for(i in seq(1:1000)){

x <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = future_data,
  mesh = fall_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.66, -41.8, -25.4, log(4)), # coefficient estimates 
  range = 74.5, 
  #rho = 0.158, # AR1 correlation
  sigma_O = 1.33,
  sigma_E = 0.959,
  phi = 1.73, # dispersion parameter
  tweedie_p = 1.26, # tweedie power parameter 
  fixed_re = fixed_re)#,
  #seed = 42)#,
  #extra_time = fall_extra_years)

increase_dat <- bind_cols(x, fall_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103, 
         rep = i)

increase_sims <- bind_rows(increase_sims, increase_dat)

}

saveRDS(increase_sims, here("sdmtmb", "sumflounder", "data", "simulations", "fall_increase_sim.rds"))


## CALCULATE STRATIFIED MEAN ####
fall.increase_incl.stratmu <- increase_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

saveRDS(fall.increase_precl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimIncrease_incl-stratmu.rds"))

increase_precl.stratmu <- increase_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

saveRDS(increase_precl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimInc_precl-stratmu.rds"))


## BIND TO HISTORICAL STRATIFIED MEAN ####
# replicate the stratified means from the historical dataset with all observations used, 1000x to match the number of replicates
stratmeans <- sdmTMB::replicate_df(stratmeans, "rep", c(1:1000))

# select only the stratified mean 
increase_stratmu <- increase_stratmu |> 
  select(rep, stratmu) |> 
  unnest(stratmu)

# bind the two stratified mean dataframes 
full_stratmu <- bind_rows(stratmeans, increase_stratmu) 

# save the data 
saveRDS(full_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "fall_inc_all-stratmu.rds"))


