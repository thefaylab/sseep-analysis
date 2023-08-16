### created: 12/10/2022
### last updated: 08/14/2023

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
set.seed(154326)

### LOAD DATA ####
# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

# extract fitted data 
fall_moddat <- fall_mod$data


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
               values_to = "EXPCATCHWT") |>
  filter(EST_YEAR %in% c(2009:2019, 2021)) # filter for the years present in the historical data

# fall_simdat |> 
#   pivot_longer(cols = c(sim1:sim1000), 
#                names_to = "nsim", 
#                values_to = "EXPCATCHWT")# |> 
  # group_by(nsim) |> 
  # ggplot() + 
  # aes(x = EST_YEAR, y = EXPCATCHWT, group = nsim) + 
  # geom_line(alpha = 0.3, col = gray(0.5)) + 
  # theme_minimal()

# save the data
saveRDS(fall_simdat, here("sdmtmb", "sumflounder", "data", "post-check", "fall_simdat.rds"))


## CALCULATE STRATIFIED MEANS ####
# calculate the stratied mean abundance index for each scenario's replicate and year 

# read in stratified mean functions
source(here("R", "StratMeanFXs_v2.R")) 

### WITH WIND  ####
FallMu_ww <- fall_simdat 
  group_by(nsim) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(.)))

### save the data
saveRDS(FallMu_ww, here("sdmtmb", "sumflounder", "data", "post-check", "fall_sim-ww-means.rds"))

### WITHOUT WIND (WIND PRECLUDED) ####
FallMu_wow <- fall_simdat |> 
  filter(AREA == "OUTSIDE") |>
  group_by(nsim) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(.)))

### save the data
saveRDS(FallMu_wow, here("sdmtmb", "sumflounder", "data", "post-check", "fall_sim-wow-means.rds"))


## FIT LINEAR REGRESSIONS ####
# estimate the simulated change in abundance index over time for each scenario

### WITH WIND  ####
fall_ww_lms <- FallMu_ww |>
  select(nsim, stratmu) |> 
  mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) |>  
  mutate(coef = map(model, broom::tidy, conf.int = TRUE)) |> 
  unnest(coef) |>
  select(nsim, term, estimate, conf.low, conf.high) %>%
  filter(term == "EST_YEAR") |> 
  mutate(SEASON = "FALL", 
         TYPE = "With Wind Included")

### save the data 
saveRDS(fall_ww_lms, file = here("sdmtmb", "sumflounder", "data", "post-check", "fall_ww_slopes.rds"))


### WITHOUT WIND (WIND PRECLUDED) ####
fall_wow_lms <- FallMu_wow |>
  select(nsim, stratmu) |>
  mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) |> 
  mutate(coef = map(model, broom::tidy, conf.int = TRUE)) |>
  unnest(coef) |>
  select(nsim, term, estimate, conf.low, conf.high) %>%
  filter(term == "EST_YEAR") |> 
  mutate(SEASON = "FALL", 
         TYPE = "With Wind Precluded")

### save the data
saveRDS(fall_wow_lms, file = here("sdmtmb", "sumflounder", "data", "post-check", "fall_wow_slopes.rds"))

