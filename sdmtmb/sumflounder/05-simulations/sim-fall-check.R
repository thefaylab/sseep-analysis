### created: 08/23/2023
### last updated:

# Check Fall Simulations  ####


## OBJECTIVE ####
#


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB)

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"
#source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
set.seed(123)


# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

# predictions from the best fit model created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "fall_predictions.rds"))

# data frame from model fit 
moddat <- fall_mod$data

# set the future years that will be simulated
future_years <- c(2022:2026)


fit_preds <- predict(fall_mod)
# pull polynomial coefficients from model fit.
x_mat <- poly(moddat$AVGDEPTH, 2)
depth_coefs <- attr(x_mat,"coefs")  


# get a dataframe containing the values for the random effects fields for the fitted values of the data points
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
  select(X, Y, STRATUM, CRUISE6, STATION, EST_YEAR, AREA, AREA_CODE, SEASON, AVGDEPTH)#, TOWID)



# filter the response and spatial predictions made across the grid cell here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R"), for the future years.
grid_preds <- fall_preds |>
  filter(EST_YEAR %in% future_years) #|>



# extract uniqueu grid information for binding later
# grid_info <- grid_preds |>
#   select(!c(est, est_non_rf, est_rf, omega_s, epsilon_st))



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
  #EXPCATCHWT~
    formula = ~
    1 +
    poly(AVGDEPTH, 2, coefs = depth_coefs) +
    as.factor(AREA) +
    omegas,
  data = data,
  mesh = mesh,
  family = tweedie(link = "log"),
  #spatial = "on",
  time = "EST_YEAR",
  #spatiotemporal = "iid",
  B = c(#0,
        -0.66,
        #0,
        -41.8,
        #0,
        -25.4,
        #0,
        0.09,
        1), # coefficient estimates
  range = 74.5,
  #rho = 0.158, # AR1 correlation
  sigma_O = 0,#1.33
  sigma_E = 0,#0.959,
  phi = 1.73, # dispersion parameter
  tweedie_p = 1.26)#, # tweedie power parameter
#fixed_re = list(omega_s = omegas, epsilon_st = NULL, zeta_s = NULL))


base_griddat <- right_join(base.sim, grid_preds, by = c("X", "Y", "EST_YEAR")) #|>
  #rename(EXPCATCHWT = observed)# |>
#mutate(rep = i)
# sim.future.grid <- left_join(base_griddat, , by = c("X", "Y", "EST_YEAR", "cell")) |> rename(pred.est = est)
head(base_griddat |>  select(X,Y,cell,EST_YEAR, mu, eta, observed, est, omegas))


inc.sim <- sdmTMB_simulate(
  formula = ~ 1 + poly(AVGDEPTH, 2, coefs = depth_coefs) + as.factor(AREA) + omegas,
  #EXPCATCHWT ~ poly(AVGDEPTH, 2) + as.factor(AREA) + omegas,
  data = data,
  mesh = mesh,
  family = tweedie(link = "log"),
  #spatial = "on",
  time = "EST_YEAR",
  #spatiotemporal = "iid", 
  B = c(-0.66, 
        -41.8, 
        -25.4, 
        0.09+log(2), 
        1), # coefficient estimates; increase scenario = double of estimated wind coefficient (0.09); additive -> log(pred.effect*2) = log(pred.effect) + log(2)
  range = 74.5, 
  #rho = 0.158, # AR1 correlation
  sigma_O = 0,#1.33,
  sigma_E = 0,#0.959,
  phi = 1.73, # dispersion parameter
  tweedie_p = 1.26)

inc_griddat <- right_join(inc.sim, grid_preds, by = c("X", "Y", "EST_YEAR")) 
wind_inc_grid <- inc_griddat |> 
  filter(AREA == "WIND")
head(wind_inc_grid |>  select(X,Y,cell,EST_YEAR, mu, eta, observed,  omegas, est))



dec.sim <- sdmTMB_simulate(
  formula = ~ 1 + poly(AVGDEPTH, 2, coefs = depth_coefs) + as.factor(AREA) + omegas,
  #EXPCATCHWT ~ poly(AVGDEPTH, 2) + as.factor(AREA) + omegas,
  data = data,
  mesh = mesh,
  family = tweedie(link = "log"),
  #spatial = "on", 
  time = "EST_YEAR",
  #spatiotemporal = "iid",
  B = c(-0.66, -41.8, -25.4, (0.09-log(2)), 1), # coefficient estimates; reduction = half of estimated wind coefficient (0.09); subtractive -> log(pred.effect/2) = log(pred.effect) - log(2)
  range = 74.5, 
  #rho = 0.158, # AR1 correlation
  sigma_O = 0,#1.33,
  sigma_E = 0,#0.959,
  phi = 1.73, # dispersion parameter
  tweedie_p = 1.26)

dec_griddat <- right_join(dec.sim, grid_preds, by = c("X", "Y", "EST_YEAR"))
wind_dec_grid <- dec_griddat |> 
  filter(AREA == "WIND")

head(wind_dec_grid |>  select(X,Y,cell,EST_YEAR, mu, eta, observed, omegas, est))
