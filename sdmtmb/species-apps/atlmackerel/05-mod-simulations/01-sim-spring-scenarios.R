### created: 01/04/2024
### last updated: 

# 01 -  Simulate Spring Scenarios  ####


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

atlmackerel.dat <- here("sdmtmb", "atlmackerel", "data")
#scenario.check <- here("sdmtmb", "sumflounder", "data", "simulations", "scenario-checking")
# sseep.dir <- "../sseep-analysis"
#source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
set.seed(123)




### LOAD DATA ####
# read in fall data from model fitting
#sf_fall <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds"))

# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01b-spring-forecasts.R")
spring_mod <- readRDS(here(atlmackerel.dat, "spring_mod.rds")) 
spr_ran_pars <- tidy(spring_mod, "ran_pars")
spr_pars <- get_pars(spring_mod)

# predictions from the best fit model created here("sdmtmb", "sumflounder", "03-mod-predictions", "01b-spring-forecasts.R")
spring_preds <- readRDS(file = here(atlmackerel.dat, "atlmackerel_mod_preds.rds")) |> 
  mutate(EST_YEAR = YEAR, 
         AREA = as.character(AREA))

# data frame from model fit 
moddat <- spring_mod$data |> 
  mutate(EST_YEAR = YEAR, 
         AREA = as.character(AREA))


#### DATA SETUP #### 
# set the future years that will be simulated 
#future_years <- c(2022:2026)

# get a dataframe containing the values for the random effects fields for the fitted values of the data points
#fit_preds <- predict(spring_mod)

# pull polynomial coefficients from model fit.
x_mat <- poly(moddat$AVGDEPTH, 4)
depth_coefs <- attr(x_mat,"coefs")  

# create empty base scenario storage vectors 
# sim_base_tows <- data.frame() # for observations at future tow locations
# sim_base_grid <- data.frame() # for observations over entire grid in future years
# 
# # create empty increased scenario storage vectors 
# sim_increase_tows <- data.frame() # for observations at future tow locations
# sim_increase_grid <- data.frame() # for observations over entire grid in future years
# 
# # create empty decreased scenario storage vectors 
# sim_decrease_tows <- data.frame() # for observations at future tow locations
# sim_decrease_grid <- data.frame() # for observations over entire grid in future years
# 
# 
# start.time <- Sys.time()



# LOOP #### 
# for (i in seq(1:1000)){
#   progress(i)
### DATA WRANGLE ####
# randomly select 5 years from the historical time period.
#resampled_years <- sample(unique(moddat$EST_YEAR), size = 1, replace = TRUE)

# use locations from the resampled years as the observations in future years
# nested loop to filter each resampled year individually and bind together into empty storage filter for instances where a year is replaced and resampled
#future_preds <- data.frame() # empty storage dataframe for filtered tow locations

#for(t in seq_along(resampled_years)){
 # filtered_yr <- spring_preds |> 
    filter(EST_YEAR == resampled_years) #%in% resampled_years[t]) |> 
    #mutate(EST_YEAR = future_years[t])
  
  #future_preds <- bind_rows(future_preds, filtered_yr)
#}


# extract unique tow information for binding later 
#future_tows
tows <- spring_preds |> #filtered_yr |> #future_preds |> 
  dplyr::select(X, Y, STRATUM, CRUISE6, STATION, EST_YEAR, AREA, AREA_CODE, SEASON, AVGDEPTH, omega_s, EXPCATCHWT) |> 
  rename(omegas = omega_s)

# extract the spatial effect estimates for each tow location for the future years 
# future_omegas <- future_preds$omega_s

# filter the response and spatial predictions made across the grid cell here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R"), for the future years. 
# grid_preds <- spring_preds |> 
#   filter(EST_YEAR %in% future_years) 
# 
# # extract uniqueu grid information for binding later
# grid_info <- grid_preds |>
#   dplyr::select(!c(est, est_non_rf, est_rf, omega_s, epsilon_st))

# extract the spatial effect estimates in each cell for the future years
# grid_omegas <- grid_preds$omega_s

### SIMULATION DATA ####
# bind the future tow locations and the future grid cells to create an overall location dataframe where new response values will simulated at
# data <- bind_rows(future_preds, grid_preds) |> 
#   rename(omegas = omega_s)

# bind the spatial effect estimates from the future years and the grid to fix the spatial random effects
# omegas <- append(future_omegas, grid_omegas)

# create a mesh from the binded simulation data 
mesh <- make_mesh(tows, #data, 
                  xy_cols = c("X", "Y"), cutoff = 10)
#plot(mesh)

spr_coefs <- tidy(spring_mod) #|>
 #filter(term %in% c("poly(AVGDEPTH, 4)1", "poly(AVGDEPTH, 4)2", "poly(AVGDEPTH, 4)3", "poly(AVGDEPTH, 4)4", "EST_YEAR2010"))

## BASE SCENARIO ####
base.sim <- sdmTMB_simulate(
  #EXPCATCHWT ~ poly(AVGDEPTH, 2) + as.factor(AREA) + omegas,
  formula = ~poly(AVGDEPTH, 4, coefs = depth_coefs) + as.factor(EST_YEAR) + as.factor(AREA) + omegas,
  data = tows, #data,
  mesh = mesh,
  family = tweedie(link = "log"),
  #spatial = "on",
  time = "EST_YEAR",
  #spatiotemporal = "iid",
  B = c(spr_coefs$estimate,1,1), # coefficient estimates 
  range = spr_ran_pars$estimate[spr_ran_pars$term == "range"], 
  #rho = 0.158, # AR1 correlation
  sigma_O = 0, #spr_ran_pars$estimate[spr_ran_pars$term == "sigma_O"], #1.15,
  sigma_E = spr_ran_pars$estimate[spr_ran_pars$term == "sigma_E"],
  phi = exp(spr_pars$ln_phi), # dispersion parameter
  tweedie_p = spr_ran_pars$estimate[spr_ran_pars$term == "tweedie_p"])#, # tweedie power parameter 
  #fixed_re = list(omegas))

### BASE SIMULATION DATA WRANGLING ####
# extract the simulated data pertaining to the future tow locations
base_towdat <- right_join(base.sim, tows, by = c("X", "Y", "EST_YEAR")) |>
  select(!c(EXPCATCHWT, omegas.y)) |> 
  mutate(SVSPP = 103) |> 
  rename(EXPCATCHWT = observed)
# 
# # add to simulated tow dataframe
# sim_base_tows <- bind_rows(sim_base_tows, base_towdat)
# 
# # extract the simulated data pertaining to the future grid cells
# base_griddat <- right_join(base.sim, grid_info, by = c("X", "Y", "EST_YEAR")) |>
#   rename(EXPCATCHWT = observed) |> 
#   mutate(rep = i)
# 
# # add to simulated grid dataframe
# sim_base_grid <- bind_rows(sim_base_grid, base_griddat)

## INCREASE SCENARIO ####
inc.sim <- sdmTMB_simulate(
  #EXPCATCHWT ~ poly(AVGDEPTH, 2) + as.factor(AREA) + omegas,
  formula = ~poly(AVGDEPTH, 4, coefs = depth_coefs) + as.factor(EST_YEAR) + as.factor(AREA) + omegas,
  data = tows,
  mesh = mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  #spatial = "on", 
  #spatiotemporal = "iid",
  B = c(spr_coefs$estimate,2,1), # coefficient estimates 
  range = spr_ran_pars$estimate[spr_ran_pars$term == "range"], 
  #rho = 0.158, # AR1 correlation
  sigma_O = 0, #spr_ran_pars$estimate[spr_ran_pars$term == "sigma_O"], #sigma_O = 0 to fix based on the covariate in the formula
  sigma_E = spr_ran_pars$estimate[spr_ran_pars$term == "sigma_E"],
  phi = exp(spr_pars$ln_phi), # dispersion parameter
  tweedie_p = spr_ran_pars$estimate[spr_ran_pars$term == "tweedie_p"])#, # tweedie power parameter 
  #fixed_re = list(omegas)) 


### INCREASE SIMULATION DATA WRANGLING ####
# extract the simulated data pertaining to the future tow locations
incc_towdat <- right_join(inc.sim, tows, by = c("X", "Y", "EST_YEAR")) |>
  select(!c(EXPCATCHWT, omegas.y)) |> 
  mutate(SVSPP = 103) |>  
  rename(EXPCATCHWT = observed)
# 
# # add to simulated tow dataframe
# sim_increase_tows <- bind_rows(sim_increase_tows, inc_towdat)
# 
# # extract the simulated data pertaining to the future grid cells
# inc_griddat <- right_join(inc.sim, grid_info, by = c("X", "Y", "EST_YEAR")) |>
#   rename(EXPCATCHWT = observed) |> 
#   mutate(rep = i)
# 
# # add to simulated grid dataframe
# sim_increase_grid <- bind_rows(sim_increase_grid, inc_griddat)

## DECREASE SCENARIO ####
dec.sim <- sdmTMB_simulate(
  #EXPCATCHWT ~ poly(AVGDEPTH, 2) + as.factor(AREA) + omegas,
  formula = ~poly(AVGDEPTH, 4, coefs = depth_coefs) + as.factor(EST_YEAR) + as.factor(AREA) + omegas,
  data = tows,
  mesh = mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  #spatial = "on", 
  #spatiotemporal = "iid",
  B = c(spr_coefs$estimate,0.5,1), # coefficient estimates 
  range = spr_ran_pars$estimate[spr_ran_pars$term == "range"], 
  #rho = 0.158, # AR1 correlation
  sigma_O = 0,#spr_ran_pars$estimate[spr_ran_pars$term == "sigma_O"], #1.15,
  sigma_E = spr_ran_pars$estimate[spr_ran_pars$term == "sigma_E"],
  phi = exp(spr_pars$ln_phi), # dispersion parameter
  tweedie_p = spr_ran_pars$estimate[spr_ran_pars$term == "tweedie_p"])#, # tweedie power parameter 
  #fixed_re = list(omegas))

### DECREASE SIMULATION DATA WRANGLING ####
# extract the simulated data pertaining to the future tow locations
dec_towdat <- right_join(dec.sim, tows, by = c("X", "Y", "EST_YEAR")) |>
  select(!c(EXPCATCHWT, omegas.y)) |> 
  mutate(SVSPP = 103) |>  
  rename(EXPCATCHWT = observed) #|>
  # mutate(SVSPP = 103) #,
         # rep = i)
# 
# # add to simulated tow dataframe
# sim_decrease_tows <- bind_rows(sim_decrease_tows, dec_towdat)
# 
# # extract the simulated data pertaining to the future grid cells
# dec_griddat <- right_join(dec.sim, grid_info, by = c("X", "Y", "EST_YEAR")) |>
#   rename(EXPCATCHWT = observed) |> 
#   mutate(rep = i)
# 
# # add to simulated grid dataframe
# sim_decrease_grid <- bind_rows(sim_decrease_grid, dec_griddat)
# 
# if(i == 1000) cat("Done!")
# 
# }
#  
# end.time <- Sys.time()
# end.time - start.time # 5.6 hours

# SAVE THE DATA #### 
# # base simulation data 
# saveRDS(sim_base_tows, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseTows.rds"))
# saveRDS(sim_base_grid, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseGrid.rds"))
# 
# # increased simulation data 
# saveRDS(sim_increase_tows, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseTows.rds"))
# saveRDS(sim_increase_grid, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseGrid.rds"))
# 
# # reduced simulation data 
# saveRDS(sim_decrease_tows, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseTows.rds"))
# saveRDS(sim_decrease_grid, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseGrid.rds"))

#check 5 years in each replicate
# years <- sim_decrease_tows |> group_by(rep) |> summarise(years = length(unique(EST_YEAR)))