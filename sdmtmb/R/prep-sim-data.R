### created: 02/21/2024
### updated: 

# PREPARE DATA FOR SIMULATIONS  ####

## OBJECTIVE ####


### LOAD PACKAGES #### 
library(here)
# library(infer)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
source(here("sdmtmb", "R", "sim_prep_functions.R"))


### ENVIRONMENT SET UP ####
set.seed(72346)
# set season and species to be simulated
season <- "spring"

species <- "atlmackerel"

# number of simulations 
nsims <- 1:1000

# set file locations of where to read in data and save outputs
dat.files <- here("sdmtmb", species, "data")
sim.dat <- here("sdmtmb", species, "data", "simulations", season)
plot.files <- here("sdmtmb", species, "plots", "simulations")

# generate a set random numbers to set seed with each iteration for reproducibility
seed <- sample.int(1e6, length(nsims))


### LOAD DATA ####
# fitted sdmTMB model 
mod <- readRDS(here(dat.files, str_c(season, "_mod.rds", sep = "")))

# depth relationship coefficients used in model fitting
depth_coefs <- readRDS(here(dat.files, str_c(season, "_depth_coefs.rds", sep = "")))
# depth_coefs <- readRDS(here("sdmtmb", "atlmackerel", "data", "simulations", "sim-checking", "depth_coefs.rds"))


# model predictions 
mod_preds <- readRDS(here(dat.files, str_c(season, "_mod_preds.rds", sep = ""))) |> 
  # readRDS(here(dat.files, "atlmackerel_mod_preds.rds")) |>
  select(X, Y, YEAR, SEASON, TOWID, STATION, CRUISE6, STRATUM, AVGDEPTH, AREA, AREA_CODE,  omega_s) |>  # select important tow information
  mutate(AREA = as.character(AREA), # transform factor into character
         AREA_CODE = ifelse(AREA == "OUTSIDE", 0, AREA_CODE)) |> # transform area_code to match the simulation output field 
  rename(fix_omegas = omega_s, # rename spatial predictions 
         EST_YEAR = YEAR) # rename year to match formula and model fit

# grid predictions 
grid_preds <- readRDS(here(dat.files, str_c(season,"_grid_preds.rds", sep = ""))) |> 
  # readRDS(here(dat.files, "atlmackerel_grid_preds.rds")) |> 
  dplyr::select(!c(est, est_non_rf, est_rf, epsilon_st)) |> 
  # convert factors back to integers and characters
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)), 
         AREA = as.character(AREA), 
         AREA_CODE = ifelse(AREA == "OUTSIDE", 0, AREA_CODE)) |> 
  rename(fix_omegas = omega_s)

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
# strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))


#### DATA WRANGLE #### 
# extract the number of depth coefficients 
n_depth_coefs <- length(depth_coefs$alpha)

# identify the unique years to sample 
years <- unique(mod_preds$EST_YEAR) |> sort()

# extract fitted model coefficients for data simulation
mod_coefs <- tidy(mod) |> 
  dplyr::select(!std.error)

# extract the random effect parameters for data simulation
ran_pars <- tidy(mod, "ran_pars")

# save the data 
saveRDS(ran_pars, here(dat.files, str_c(season,  "mod_ran_pars.rds", sep = "_")))


## DATA PREPARATION ####
### FOR SAMPLING ####
# number of years to project
nyears <- 1:5

# sample the unique years from the survey for the size of nyears, dictate the number of iterations to sample years, and set the seed with each iteration to allow for reproducibility. 
year_samps <- map2(nsims, seed, ~sample_years(nyears, years, seed = .y))

# save the data
saveRDS(year_samps, here(sim.dat, str_c(season, species, length(nsims), "year_samps.rds", sep = "_")))

# sample the tow locations based on the sampled years, and set the seed with each iteration to allow for reproducibility.
future_locs <- map2(year_samps, seed, ~survey_locations(.x, mod_preds, seed = .y))

# save the data
saveRDS(future_locs, here(sim.dat, str_c(season, species, length(nsims), "future_locs.rds", sep = "_")))

### FOR SIMULATING####
# create a list item containing the future survey locations replicated by the number of nsims 
# sim_locs <- map(nsims, ~list(data = mod_preds))
sim_locs <- future_locs |> map(~list(data = .))

# save the data
saveRDS(sim_locs, here(sim.dat, str_c(season, species, length(nsims), "surv_locs.rds", sep = "_")))

# create a list item containing the NE US grid replicated by the number of nsims 
# sim_grid <- map(nsims, ~list(data = grid_preds))
future_grids <- map2(year_samps, seed, ~grid_locations(.x, grid_preds, seed = .y)) |> map(~list(data = .))

# save the data
saveRDS(future_grids, here(sim.dat, str_c(season, species, length(nsims), "surv_grid.rds", sep = "_")))

# create mesh for survey locations 
# mesh <- make_mesh(mod_preds, xy_cols = c("X", "Y"), cutoff = 10) 
# # create a list item containing the survey location mesh replicated by the number of nsims
# surv_meshs <- map(nsims, ~list(mesh = mesh))

# surv_meshs <- map(surv_locs, ~make_mesh(.x, xy_cols = c("X", "Y"), cutoff = 10)) |> map(~list(mesh = .))
surv_meshs <- map(sim_locs, ~make_mesh(.$data, xy_cols = c("X", "Y"), cutoff = 10)) |> map(~list(mesh = .)) # this takes an hour for 1000 meshs

# save the data
saveRDS(surv_meshs, here(sim.dat, str_c(season, species, length(nsims), "surv_meshs.rds", sep = "_")))

# create mesh for the grid locations 
# mesh <- make_mesh(grid_preds, xy_cols = c("X", "Y"), cutoff = 10) 
# # create a list item containing the grid mesh replicated by the number of nsims
# grid_meshs <- map(nsims, ~list(mesh = mesh))
grid_meshs <- map(future_grids, ~make_mesh(.$data, xy_cols = c("X", "Y"), cutoff = 15)) |> map(~list(mesh = .)) # this takes an hour for 1000 meshs

# save the data
saveRDS(grid_meshs, here(sim.dat, str_c(season, species, length(nsims), "grid_meshs.rds", sep = "_")))

# create a list item containing the depth coefficients replicated by the number of nsims
# depth_coefs_list <- map(nsims, ~list(depth_coefs = depth_coefs))

# save the data
# saveRDS(depth_coefs_list, here(sim.dat, str_c(season, species, length(nsims), "depth_coefs.rds", sep = "_")))

#### NO WIND EFFECT ####
# extract the parameter coefficients 
coefs <- map(year_samps, ~prepare_coefs(year_samps = ., year_data = years, mod_coefs, n_depth_coefs, wind_coef = TRUE, treatment = (-tail(mod_coefs$estimate,1)))) |> 
  map(~list(b = .))

# create a list object where each item contains the survey locations, the mesh, and the parameter coefficients  
surv_data <- map2(sim_locs, surv_meshs, ~append(.x, .y)) |> 
  # map2(depth_coefs_list, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

# create a list object where each item contains the grid locations, the mesh, and the parameter coefficients
grid_data <- map2(future_grids, grid_meshs, ~append(.x, .y)) |> 
  # map2(depth_coefs_list, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

### save the data 
saveRDS(coefs, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "coefs.rds", sep = "_")))
saveRDS(surv_data, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "surv-data.rds", sep = "_")))
saveRDS(grid_data, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "grid-data.rds", sep = "_")))



#### BASE SCENARIO ####
# extract the parameter coefficients 
coefs <- map(year_samps, ~prepare_coefs(year_samps = ., year_data = years, mod_coefs, n_depth_coefs, wind_coef = FALSE, treatment = 0)) |> 
  map(~list(b = .))

# create a list object where each item contains the survey locations, the mesh, and the parameter coefficients  
surv_data <- map2(sim_locs, surv_meshs, ~append(.x, .y)) |> 
  # map2(depth_coefs_list, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

# create a list object where each item contains the grid locations, the mesh, and the parameter coefficients
grid_data <- map2(future_grids, grid_meshs, ~append(.x, .y)) |> 
  # map2(depth_coefs_list, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

### save the data 
saveRDS(coefs, here(sim.dat, str_c(season, species, "baseline", length(nsims), "coefs.rds", sep = "_")))
saveRDS(surv_data, here(sim.dat, str_c(season, species, "baseline", length(nsims), "surv-data.rds", sep = "_")))
saveRDS(grid_data, here(sim.dat, str_c(season, species, "baseline", length(nsims), "grid-data.rds", sep = "_")))



#### ENHANCED SCENARIO ####
# extract the parameter coefficients 
coefs <- map(year_samps, ~prepare_coefs(year_samps = ., year_data = years, mod_coefs, n_depth_coefs, wind_coef = FALSE, treatment = (log(2)))) |> 
  map(~list(b = .))

# create a list object where each item contains the survey locations, the mesh, and the parameter coefficients  
surv_data <- map2(sim_locs, surv_meshs, ~append(.x, .y)) |> 
  # map2(depth_coefs_list, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

# create a list object where each item contains the grid locations, the mesh, and the parameter coefficients
grid_data <- map2(future_grids, grid_meshs, ~append(.x, .y)) |> 
  # map2(depth_coefs_list, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

### save the data 
saveRDS(coefs, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "coefs.rds", sep = "_")))
saveRDS(surv_data, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "surv-data.rds", sep = "_")))
saveRDS(grid_data, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "grid-data.rds", sep = "_")))


#### REDUCED SCENARIO ####
# extract the parameter coefficients 
coefs <- map(year_samps, ~prepare_coefs(year_samps = ., year_data = years, mod_coefs, n_depth_coefs, wind_coef = FALSE, treatment = (-log(2)))) |> 
  map(~list(b = .))

# create a list object where each item contains the survey locations, the mesh, and the parameter coefficients  
surv_data <- map2(sim_locs, surv_meshs, ~append(.x, .y)) |> 
  # map2(depth_coefs_list, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

# create a list object where each item contains the grid locations, the mesh, and the parameter coefficients
grid_data <- map2(future_grids, grid_meshs, ~append(.x, .y)) |> 
  # map2(depth_coefs_list, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

### save the data 
saveRDS(coefs, here(sim.dat, str_c(season, species, "reduced", length(nsims), "coefs.rds", sep = "_")))
saveRDS(surv_data, here(sim.dat, str_c(season, species, "reduced", length(nsims), "surv-data.rds", sep = "_")))
saveRDS(grid_data, here(sim.dat, str_c(season, species, "reduced", length(nsims), "grid-data.rds", sep = "_")))
