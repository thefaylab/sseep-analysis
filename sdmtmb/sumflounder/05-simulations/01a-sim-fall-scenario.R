### created: 08/23/2023
### last updated: 

# 01a -  Simulate Fall Scenarios  ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
library(svMisc)

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"
#source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
set.seed(123)


### LOAD DATA ####
# read in fall data from model fitting
#sf_fall <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds"))

# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

# predictions from the best fit model created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "fall_predictions.rds"))

# data frame from model fit 
moddat <- fall_mod$data


#### DATA SETUP #### 
# set the future years that will be simulated 
future_years <- c(2022:2026)

# get a dataframe containing the values for the random effects fields for the fitted values of the data points
fit_preds <- predict(fall_mod)

# pull polynomial coefficients from model fit.
x_mat <- poly(moddat$AVGDEPTH, 2)
depth_coefs <- attr(x_mat,"coefs")  


# create empty base scenario storage vectors 
sim_base_tows <- data.frame() # for observations at future tow locations
sim_base_grid <- data.frame() # for observations over entire grid in future years

# create empty increased scenario storage vectors 
sim_increase_tows <- data.frame() # for observations at future tow locations
sim_increase_grid <- data.frame() # for observations over entire grid in future years

# create empty decreased scenario storage vectors 
sim_decrease_tows <- data.frame() # for observations at future tow locations
sim_decrease_grid <- data.frame() # for observations over entire grid in future years


#start.time <- Sys.time()


#p5.2 <- 
  profvis::profvis(
# LOOP #### 
for (i in 1:5){
  #progress(i)
### DATA WRANGLE ####
# randomly select 5 years from the historical time period, 2020 is missing for the original observations and should not be included as an option
resampled_years <- sample(c(2009:2019,2021), size = 5, replace = TRUE)

# use locations from the resampled years as the observations in future years
# nested loop to filter each resampled year individually and bind together into empty storage filter for instances where a year is replaced and resampled
future_preds <- data.frame() # empty storage dataframe for filtered tow locations

for(t in seq_along(resampled_years)){
  filtered_yr <- fit_preds |> 
    filter(EST_YEAR %in% resampled_years[t]) |> 
    mutate(EST_YEAR = future_years[t])
  
  future_preds <- bind_rows(future_preds, filtered_yr) 

}

# extract unique tow information for binding later 
future_tows <- future_preds|>
  dplyr::select(X, Y, STRATUM, CRUISE6, STATION, EST_YEAR, AREA, AREA_CODE, SEASON, AVGDEPTH)

# extract the spatial effect estimates for each tow location for the future years 
#future_omegas <- future_preds$omega_s #future_tows |> 
  # select(TOWID, omega_s) |> 
  # mutate(TOWID = paste(TOWID, seq(nrow(future_tows)),sep=".")) |>
  # column_to_rownames(var = "TOWID")
  

# filter the response and spatial predictions made across the grid cell here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R"), for the future years. 
grid_preds <- fall_preds |> 
  filter(EST_YEAR %in% future_years)

# extract uniqueu grid information for binding later
grid_info <- grid_preds |> 
  dplyr::select(!c(est, est_non_rf, est_rf, omega_s, epsilon_st))

# extract the spatial effect estimates in each cell for the future years
#grid_omegas <- grid_preds$omega_s #|>
  # select(CELLID, omega_s) |>
  # column_to_rownames(var = "CELLID")

### SIMULATION DATA ####
# bind the future tow locations and the future grid cells to create an overall location dataframe where new response values will simulated at
data <- bind_rows(future_preds, grid_preds) |> 
  rename(omegas = omega_s)

# bind the spatial effect estimates from the future years and the grid to fix the spatial random effects
#omegas <- append(future_omegas, grid_omegas) |> as.matrix()

# create a mesh from the binded simulation data 
mesh <- make_mesh(data, xy_cols = c("X", "Y"), cutoff = 10)
#plot(mesh)

## BASE SCENARIO ####
base.sim <- sdmTMB_simulate(
  formula = ~ 1 + poly(AVGDEPTH, 2, coefs = depth_coefs) + as.factor(AREA) + omegas,
  data = data,
  mesh = mesh,
  family = tweedie(link = "log"),
  #spatial = "on",
  time = "EST_YEAR",
  #spatiotemporal = "iid",
  B = c(-0.66, -41.8, -25.4, 0.09, 1), # coefficient estimates 
  range = 74.5, 
  #rho = 0.158, # AR1 correlation
  sigma_O = 0,#1.33
  sigma_E = 0.959,
  phi = 1.73, # dispersion parameter
  tweedie_p = 1.26)#, # tweedie power parameter 
  #fixed_re = list(omega_s = omegas, epsilon_st = NULL, zeta_s = NULL))

### BASE SIMULATION DATA WRANGLING ####
# extract the simulated data pertaining to the future tow locations
base_towdat <- right_join(base.sim, future_tows, by = c("X", "Y", "EST_YEAR")) |> 
  rename(EXPCATCHWT = observed) |> 
  mutate(SVSPP = 103, 
         rep = i)

# add to simulated tow dataframe
sim_base_tows <- bind_rows(sim_base_tows, base_towdat)

# extract the simulated data pertaining to the future grid cells
base_griddat <- right_join(base.sim, grid_info, by = c("X", "Y", "EST_YEAR")) |>
  rename(EXPCATCHWT = observed) |> 
  mutate(rep = i)
# sim.future.grid <-
#   left_join(base_griddat, future_grid, by = c("X", "Y", "EST_YEAR", "cell")) |> rename(pred.est = est)
# head(sim.future.grid |> select (X, Y, cell, EST_YEAR, mu, eta, pred.est, omegas))

# add to simulated grid dataframe
sim_base_grid <- bind_rows(sim_base_grid, base_griddat)

## INCREASE SCENARIO ####
inc.sim <- sdmTMB_simulate(
  formula = ~ 1 + poly(AVGDEPTH, 2, coefs = depth_coefs) + as.factor(AREA) + omegas,
  #EXPCATCHWT ~ poly(AVGDEPTH, 2) + as.factor(AREA) + omegas,
  data = data,
  mesh = mesh,
  family = tweedie(link = "log"),
  #spatial = "on",
  time = "EST_YEAR",
  #spatiotemporal = "iid", 
  B = c(-0.66, -41.8, -25.4, (0.09+log(2)), 1), # coefficient estimates; increase scenario = double of estimated wind coefficient (0.09); additive -> log(pred.effect*2) = log(pred.effect) + log(2)
  range = 74.5, 
  #rho = 0.158, # AR1 correlation
  sigma_O = 0,#1.33,
  sigma_E = 0.959,
  phi = 1.73, # dispersion parameter
  tweedie_p = 1.26)#, # tweedie power parameter 
  #fixed_re = list(omega_s = omegas, epsilon_st = NULL, zeta_s = NULL))


### INCREASE SIMULATION DATA WRANGLING ####
# extract the simulated data pertaining to the future tow locations
inc_towdat <- right_join(inc.sim, future_tows, by = c("X", "Y", "EST_YEAR")) |> 
  rename(EXPCATCHWT = observed) |> 
  mutate(SVSPP = 103, 
         rep = i)

# add to simulated tow dataframe
sim_increase_tows <- bind_rows(sim_increase_tows, inc_towdat)

# extract the simulated data pertaining to the future grid cells
inc_griddat <- right_join(inc.sim, grid_info, by = c("X", "Y", "EST_YEAR")) |>
  rename(EXPCATCHWT = observed) |> 
  mutate(rep = i)

# add to simulated grid dataframe
sim_increase_grid <- bind_rows(sim_increase_grid, inc_griddat)

## DECREASE SCENARIO ####
dec.sim <- sdmTMB_simulate(
  formula = ~ 1 + poly(AVGDEPTH, 2, coefs = depth_coefs) + as.factor(AREA) + omegas,
  #EXPCATCHWT ~ poly(AVGDEPTH, 2) + as.factor(AREA) + omegas,
  data = data,
  mesh = mesh,
  family = tweedie(link = "log"),
  #spatial = "on", 
  time = "EST_YEAR",
  #spatiotemporal = "iid",
  B = c(-0.66, -41.8, -25.4, (0.09+log(1/2)), 1), # coefficient estimates; reduction = half of estimated wind coefficient (0.09); subtractive -> log(pred.effect/2) = log(pred.effect) - log(2)
  range = 74.5, 
  #rho = 0.158, # AR1 correlation
  sigma_O = 0,#1.33,
  sigma_E = 0.959,
  phi = 1.73, # dispersion parameter
  tweedie_p = 1.26)#, # tweedie power parameter 
  #fixed_re = list(omega_s = omegas, epsilon_st = NULL, zeta_s = NULL))

### DECREASE SIMULATION DATA WRANGLING ####
# extract the simulated data pertaining to the future tow locations
dec_towdat <- right_join(dec.sim, future_tows, by = c("X", "Y", "EST_YEAR")) |> 
  rename(EXPCATCHWT = observed) |> 
  mutate(SVSPP = 103, 
         rep = i)

# add to simulated tow dataframe
sim_decrease_tows <- bind_rows(sim_decrease_tows, dec_towdat)

# extract the simulated data pertaining to the future grid cells
dec_griddat <- right_join(dec.sim, grid_info, by = c("X", "Y", "EST_YEAR")) |>
  rename(EXPCATCHWT = observed) |> 
  mutate(rep = i)

# add to simulated grid dataframe
sim_decrease_grid <- bind_rows(sim_decrease_grid, dec_griddat)

#if(i == 1000) cat("Done!")

}
)
#end.time <- Sys.time()
#end.time - start.time # 5.059795 hours

# SAVE THE DATA #### 
# base simulation data 
saveRDS(sim_base_tows, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseTows.rds"))
saveRDS(sim_base_grid, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseGrid.rds"))

# increased simulation data 
saveRDS(sim_increase_tows, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseTows.rds"))
saveRDS(sim_increase_grid, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseGrid.rds"))

# reduced simulation data 
saveRDS(sim_decrease_tows, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseTows.rds"))
saveRDS(sim_decrease_grid, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseGrid.rds"))

#check 5 years in each replicate
# years <- sim_decrease_tows |> group_by(rep) |> summarise(years = length(unique(EST_YEAR)))