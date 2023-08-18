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
# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-spring-forecasts.R")
spring_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mod.rds"))

spring_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "spring_predictions.rds"))

# here("sdmtmb", "sumflounder", "05-simulations", "01a-prepare-spring-sim-dat.R")
future_data <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "spring_future_data.rds"))

# here("sdmtmb", "sumflounder", "05-simulations", "01a-prepare-spring-sim-dat.R")
spring_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "spring_future_mesh.rds"))

# here("sdmtmb", "sumflounder", "data")
stratmeans <- readRDS(here("data", "sumflounder", "sf_stratmu.rds")) |> 
  filter(SEASON == "SPRING", TYPE == "With Wind Included") |>
  select(!c(COMNAME, SCINAME, TYPE, SEASON))


### DATA WRANGLE ####

tidy(spring_mod)
tidy(spring_mod, "ran_pars")

spring_info <- future_data |>
  ungroup() |>
  dplyr::select(STATION, STRATUM, CRUISE6, AREA)

spring_omegas <- mean(spring_preds$omega_s)
fixed_re <-  list(spring_omegas, epsilon_st = NULL, zeta_s = NULL)


## SIMULATE INCREASE SCENARIO ####
# create empty storage data frame
spr.increase_sims <- data.frame()

# loop 
for(i in seq(1:1000)){

x <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = future_data,
  mesh = spring_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.06, 7.66, -27.7, log(6)), # coefficient estimates 
  range = 96.2, 
  #rho = 0.158, # AR1 correlation
  sigma_O = 1.15,
  sigma_E = 0.844,
  phi = 1.83, # dispersion parameter
  tweedie_p = 1.39, # tweedie power parameter 
  fixed_re = fixed_re)#,
  #seed = 42)#,
  #extra_time = spring_extra_years)

increase_dat <- bind_cols(x, spring_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103, 
         rep = i)

spr.increase_sims <- bind_rows(spr.increase_sims, increase_dat)

}

saveRDS(spr.increase_sims, here("sdmtmb", "sumflounder", "data", "simulations", "spring_increase_sim.rds"))


## CALCULATE STRATIFIED MEAN ####
# With Wind Included
spr.increase_incl.stratmu <- spr.increase_sims |> 
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(spr.increase_incl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimIncrease_incl-stratmu.rds"))

# With Wind Precluded
spr.increase_precl.stratmu <- spr.increase_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(spr.increase_precl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimIncrease_precl-stratmu.rds"))


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
saveRDS(full_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "spring_inc_all-stratmu.rds"))


