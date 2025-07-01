### created: 06/04/2024
### updated: 

# 07 - Prepare datasets for simulation ####

## OBJECTIVE ####


### Load Packages #### 
library(here)
library(tictoc)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
source(here("sdmtmb", "functions", "sim_prep_functions.R"))

### Environment set-up ####
set.seed(72346)
# set season and species to be simulated
season <- "fall"

species <- "sumflounder"

# set file locations of where to read in data and save outputs
dat.files <- here("data", "rds", "sdmtmb", species)
sim.dat <- here("data", "rds", "sdmtmb", species, "simulations", season)

### Load Data ####
# fitted sdmTMB model 
mod <- readRDS(here(dat.files, str_c(season, "mod.rds", sep = "_")))

# depth relationship coefficients used in model fitting
depth_coefs <- readRDS(here(dat.files, str_c(season, "depth_coefs.rds", sep = "_")))

# model estimated coefficients for covariates 
mod_coefs <- readRDS(here(dat.files, str_c(season, "mod-ests.rds", sep = "_")))

# model estimated random parameters 
ran_pars <- readRDS(here(dat.files, str_c(season, "mod_ran-pars.rds", sep = "_")))

# model predictions 
mod_preds <- readRDS(here(dat.files, str_c(season, "_mod_preds.rds", sep = ""))) |> 
  select(X, Y, EST_YEAR, SEASON, TOWID, STATION, CRUISE6, STRATUM, AVGDEPTH, AREA, AREA_CODE,  omega_s) |> 
  mutate(AREA = as.character(AREA), 
         AREA_CODE = ifelse(AREA == "OUTSIDE", 0, AREA_CODE),
         EST_YEAR = as.integer(as.character(EST_YEAR))) |>
  rename(fix_omegas = omega_s)

# grid predictions 
grid_preds <- readRDS(here(dat.files, str_c(season,"_grid_preds.rds", sep = ""))) |> 
  dplyr::select(!c(est, est_non_rf, est_rf, epsilon_st)) |> 
  # convert factors back to integers and characters
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)), 
         AREA = as.character(AREA), 
         AREA_CODE = ifelse(AREA == "OUTSIDE", 0, AREA_CODE)) |> 
  rename(fix_omegas = omega_s)

#### Data Wrangle #### 
# number of depth coefficients used in model fitting
n_depth_coefs <- length(depth_coefs)

# set the years to sample 
years <- unique(mod_preds$EST_YEAR) |> sort()


## Data Parameters ####
# number of simulations 
nsims <- 1:1000

# number of years to project
nyears <- 1:5

# mesh cutoff - should be the same as the cutoff distance used to fit the model
cutoff <- 10

# generate a set random numbers to set seed with each iteration for reproducibility
seed <- sample.int(1e6, length(nsims))

## Prepare Locations ####
# sample years to serve as future years 
year_samps <- map2(nsims, seed, ~sample_years(nyears, years, seed = .y))

# tic()
# extract future survey locations based on sampled years
future_locs <- map2(year_samps, seed, ~survey_locations(.x, mod_preds, seed = .y)) 
future_locs_list <- future_locs |> map(~list(data = .))
# toc() # 30 sec

# tic()
# create mesh for survey locations 
surv_meshs <- map(future_locs_list, ~make_mesh(.$data, xy_cols = c("X", "Y"), cutoff = cutoff)) |> 
  map(~list(mesh = .))
# toc() # 62 sec

# tic()
# extract future grid locations based on sampled years
new_grids <- map2(year_samps, seed, ~grid_locations(.x, grid_preds, seed = .y)) 
new_grids_list <- new_grids |> map(~list(data = .))
# toc() # 30 sec

# tic()
# create mesh for distribution grid 
grid_meshs <- map(new_grids_list, ~make_mesh(.x$data, xy_cols = c("X", "Y"), cutoff = cutoff)) |> 
  map(~list(mesh = .))
# toc() # 110 sec

incl_tow_summary_list <- split(data, factor(data$SIM)) |> 
  map(~group_by(., SCENARIO, EST_YEAR, STRATUM) |> 
        filter(EXPCATCHWT >0) |> 
        summarise(towct = length(unique(TOWID))) |> 
        filter(towct == 1)) 
incl_tow_summary_df <- list_rbind(incl_tow_summary_list, names_to = "SIM") |> 
  mutate(SIM = as.integer(SIM)) |> 
  group_by(SIM, SCENARIO) |> 
  nest()
saveRDS(incl_tow_summary_list, here("data", "rds", "sdmtmb", "atlmackerel", "simulations", "spring", "spring_atlmackerel_future_incl_tow_summary_list.rds"))

precl_tow_summary_list <- split(data, factor(data$SIM)) |> 
  map(~group_by(., SCENARIO, EST_YEAR, STRATUM) |> 
        filter(AREA == "OUTSIDE", EXPCATCHWT > 0) |> 
        summarise(towct = length(unique(TOWID))) |> 
        filter(towct == 1)) 
precl_tow_summary_df <- list_rbind(precl_tow_summary_list, names_to = "SIM") |> 
  mutate(SIM = as.integer(SIM)) |> 
  group_by(SIM, SCENARIO) |> 
  nest() 
saveRDS(precl_tow_summary_df, here("data", "rds", "sdmtmb", "atlmackerel", "simulations", "spring", "spring_atlmackerel_future_precl_tow_summary_df.rds"))

### Save data ####
save.dat <- list(year_samps, 
                 future_locs,
                 future_locs_list,
                 surv_meshs, 
                 new_grids, 
                 new_grids_list,
                 grid_meshs)

names(save.dat) <- c(str_c(season, species, "year_samps.rds", sep = "_"),
                     str_c(season, species, "future_locs.rds", sep = "_"),
                     str_c(season, species, "future_locs_list.rds", sep = "_"),
                     str_c(season, species, "future_surv_mesh.rds", sep = "_"),
                     str_c(season, species, "future_grids.rds", sep = "_"),
                     str_c(season, species, "future_grids_list.rds", sep = "_"),
                     str_c(season, species, "future_grid_mesh.rds", sep = "_"))

pmap(list(save.dat, names(save.dat)), ~saveRDS(.x, here(sim.dat, .y)))

## Prepare Data ####
# wind coefficient - if factored area was included in original model fitting 
wind_coef <- FALSE                            # FIXME as necessary

### treatments  
no_wind_eff <- (-tail(mod_coefs$estimate,1)) # FIXME if present in original model fitting
baseline <- 0                                # FIXME as necessary
enhanced <- log(2)                           # FIXME as necessary
reduced <- (-log(2))                         # FIXME as necessary

### No wind effect ####
# create the data to emulate if there was no wind effect on biomass catch rates 
# skip if `wind_coef == FALSE` above.
coefs <- map(year_samps, ~prepare_coefs(year_samps = ., year_data = years, mod_coefs, n_depth_coefs, wind_coef = wind_coef, treatment = no_wind_eff)) |> 
  map(~list(b = .))

# append locations and mesh 
data <- map2(future_locs_list, surv_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

grid_data <- map2(new_grids_list, grid_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

#### Save data ####
save.dat <- list(coefs,
                 data, 
                 grid_data)

names(save.dat) <- c(str_c(season, species, "no-wind_sim-coefs.rds", sep = "_"),
                     str_c(season, species, "no-wind_sim-locs-dat.rds", sep = "_"), 
                     str_c(season, species, "no-wind_sim-grid-dat.rds", sep = "_"))

pmap(list(save.dat, names(save.dat)), ~saveRDS(.x, here(sim.dat, .y)))

### Baseline ####
# parameter coefficients 
coefs <- map(year_samps, ~prepare_coefs(year_samps = ., year_data = years, mod_coefs, n_depth_coefs, wind_coef = wind_coef, treatment = baseline)) |> 
  map(~list(b = .))

# append locations and mesh 
data <- map2(future_locs_list, surv_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

grid_data <- map2(new_grids_list, grid_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

#### Save data ####
save.dat <- list(coefs,
                 data, 
                 grid_data)

names(save.dat) <- c(str_c(season, species, "baseline_sim-coefs.rds", sep = "_"),
                     str_c(season, species, "baseline_sim-locs-dat.rds", sep = "_"), 
                     str_c(season, species, "baseline_sim-grid-dat.rds", sep = "_"))

pmap(list(save.dat, names(save.dat)), ~saveRDS(.x, here(sim.dat, .y)))

### Enhanced #### 
# parameter coefficients 
coefs <- map(year_samps, ~prepare_coefs(year_samps = ., year_data = years, mod_coefs, n_depth_coefs, wind_coef = wind_coef, treatment = enhanced)) |> 
  map(~list(b = .))

# append locations and mesh 
data <- map2(future_locs_list, surv_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

grid_data <- map2(new_grids_list, grid_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

#### Save data ####
save.dat <- list(coefs,
                 data, 
                 grid_data)

names(save.dat) <- c(str_c(season, species, "enhanced_sim-coefs.rds", sep = "_"),
                     str_c(season, species, "enhanced_sim-locs-dat.rds", sep = "_"), 
                     str_c(season, species, "enhanced_sim-grid-dat.rds", sep = "_"))

pmap(list(save.dat, names(save.dat)), ~saveRDS(.x, here(sim.dat, .y)))

### Reduced ####
# parameter coefficients 
coefs <- map(year_samps, ~prepare_coefs(year_samps = ., year_data = years, mod_coefs, n_depth_coefs, wind_coef = wind_coef, treatment = reduced)) |> 
  map(~list(b = .))

# append locations and mesh 
data <- map2(future_locs_list, surv_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

grid_data <- map2(new_grids_list, grid_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

#### Save data ####
save.dat <- list(coefs,
                 data, 
                 grid_data)

names(save.dat) <- c(str_c(season, species, "reduced_sim-coefs.rds", sep = "_"),
                     str_c(season, species, "reduced_sim-locs-dat.rds", sep = "_"), 
                     str_c(season, species, "reduced_sim-grid-dat.rds", sep = "_"))

pmap(list(save.dat, names(save.dat)), ~saveRDS(.x, here(sim.dat, .y)))
