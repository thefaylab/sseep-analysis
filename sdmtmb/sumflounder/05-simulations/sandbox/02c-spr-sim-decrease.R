### created: 07/24/2023
### last updated: 

# 02c -  DATA ####


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
# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-spring-forecasts.R")
spring_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mod.rds"))

spring_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "spring_predictions.rds"))

# here("sdmtmb", "sumflounder", "05-simulations", "01a-prepare-spring-sim-dat.R")
future_data <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "spring_future_data.rds"))

# here("sdmtmb", "sumflounder", "05-simulations", "01a-prepare-spring-sim-dat.R")
spring_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "spring_future_mesh.rds"))

# here("sdmtmb", "sumflounder", "data")
stratmeans <- readRDS(here("data", "sumflounder", "sf_stratmu.rds")) |> 
  filter(SEASON == "FALL", TYPE == "With Wind Included") |>
  select(!c(COMNAME, SCINAME, TYPE, SEASON))


### DATA WRANGLE ####

tidy(spring_mod)
tidy(spring_mod, "ran_pars")

spring_info <- future_data |>
  ungroup() |>
  dplyr::select(STATION, STRATUM, CRUISE6, AREA)

spring_omegas <- mean(spring_preds$omega_s)
fixed_re <-  list(spring_omegas, epsilon_st = NULL, zeta_s = NULL)


## SIMULATE DECREASE SCENARIO ####
# create empty storage data frame
spr.decrease_sims <- data.frame()

# loop 
for(i in seq(1:1000)){

x <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = future_data,
  mesh = spring_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.06, 7.66, -27.7, log(0.052)), # coefficient estimates 
  range = 96.2, 
  #rho = 0.158, # AR1 correlation
  sigma_O = 1.15,
  sigma_E = 0.844,
  phi = 1.83, # dispersion parameter
  tweedie_p = 1.39, # tweedie power parameter 
  fixed_re = fixed_re)#,
  #seed = 42)#,
  #extra_time = fall_extra_years)

decrease_dat <- bind_cols(x, spring_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103, 
         rep = i)

spr.decrease_sims <- bind_rows(spr.decrease_sims, decrease_dat)

}

saveRDS(spr.decrease_sims, here("sdmtmb", "sumflounder", "data", "simulations", "spring_decrease_sim.rds"))


## CALCULATE STRATIFIED MEAN ####
# With Wind Included
spr.decrease_incl.stratmu <- spr.decrease_sims |> 
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(spr.decrease_incl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimDec_incl-stratmu.rds"))

# With Wind Precluded
spr.decrease_precl.stratmu <- spr.decrease_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(spr.decrease_precl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimDec_precl-stratmu.rds"))


## BIND TO HISTORICAL STRATIFIED MEAN ####
# replicate the stratified means from the historical dataset with all observations used, 1000x to match the number of replicates
stratmeans <- sdmTMB::replicate_df(stratmeans, "rep", c(1:1000))

# select only the stratified mean 
decrease_stratmu <- decrease_stratmu |> 
  select(rep, stratmu) |> 
  unnest(stratmu)

# bind the two stratified mean dataframes 
full_stratmu <- bind_rows(stratmeans, decrease_stratmu) 

# save the data 
saveRDS(full_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "spring_dec_all-stratmu.rds"))

