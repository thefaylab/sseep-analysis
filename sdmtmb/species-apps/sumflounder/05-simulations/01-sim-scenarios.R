### created: 02/01/2024
### updated: 

# ####

## OBJECTIVE ####


### LOAD PACKAGES #### 
library(here)
# library(infer)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
source(here("R", "StratMeanFXs_v2.R"))
source(here("sdmtmb", "R", "sim_prep_functions.R"))

### ENVIRONMENT SET UP ####
set.seed(72346)
# set season and species to be simulated
season <- "fall"

species <- "sumflounder"

# number of simulations 
nsims <- 1:5

# set file locations of where to read in data and save outputs
#dat.file <- here("data", species)
dat.files <- here("sdmtmb", species, "data")
plot.files <- here("sdmtmb", species, "plots", "simulations")
test.dat <- here("sdmtmb", species, "data", "simulations", "min-sims")

# generate a set random numbers to set seed with each iteration for reproducibility
seed <- sample.int(1e6, length(nsims))


### LOAD DATA ####
# model fitting data created here()
# data <- readRDS(here(dat.files, str_c(str_c(species, season, sep = "_"), ".rds", sep="")))
                     
# fitted sdmTMB model 
mod <- readRDS(here(dat.files, str_c(season, "_mod.rds", sep = "")))

# depth relationship coefficients used in model fitting
depth_coefs <- readRDS(here(dat.files, str_c(season, "_depth_coefs.rds", sep = "")))


# model predictions 
mod_preds <- readRDS(here(dat.files, str_c(season, "_mod_preds.rds", sep = ""))) |> 
  select(X, Y, YEAR, SEASON, TOWID, STATION, CRUISE6, STRATUM, AVGDEPTH, AREA, AREA_CODE,  omega_s) |> 
  mutate(AREA = as.character(AREA), 
         AREA_CODE = ifelse(AREA == "OUTSIDE", 0, AREA_CODE)) |>
  rename(fix_omegas = omega_s, 
         EST_YEAR = YEAR)

# grid predictions 
grid_preds <- readRDS(here(dat.files, str_c(season,"_grid_preds.rds", sep = ""))) |> 
  dplyr::select(!c(est, est_non_rf, est_rf, epsilon_st)) |> 
  # convert factors back to integers and characters
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)), 
         AREA = as.character(AREA), 
         AREA_CODE = ifelse(AREA == "OUTSIDE", 0, AREA_CODE)) |> 
  rename(fix_omegas = omega_s)



#### DATA WRANGLE #### 
n_depth_coefs <- length(depth_coefs)
# set the years to sample 
years <- unique(mod_preds$EST_YEAR) |> sort()

# fitted model coefficients for data simulation
mod_coefs <- tidy(mod) |> 
  dplyr::select(!std.error)
# names <- c("DEPTH_COEF", "DEPTH_COEF", 2009:2016, 2018:2019, 2021, "WIND_COEF")
# mod_coefs$term <- names
ran_pars <- tidy(mod, "ran_pars")



## SIMULATION ####

### SET UP ####
# number of years to project
nyears <- 1:5

# 
year_samps <- map2(nsims, seed, ~sample_years(nyears, years, seed = .y))

# extract grid and survey locations 
future_locs <- map2(year_samps, seed, ~survey_locations(.x, mod_preds, seed = .y))
# surv_locs_list <- surv_locs |> map(~list(data = .))
sim_locs <- map(nsims, ~list(data = mod_preds))


# new_grids <- map2(year_samps, seed, ~grid_locations(.x, grid_preds, seed = .y)) 
# new_grids_list <- new_grids |> map(~list(data = .))


# create mesh for survey locations 
# surv_meshs <- map(surv_locs, ~make_mesh(.x, xy_cols = c("X", "Y"), cutoff = 10)) |> map(~list(mesh = .))
surv_meshs <- map(sim_locs, ~make_mesh(.$data, xy_cols = c("X", "Y"), cutoff = 10)) |> map(~list(mesh = .))

#grid_meshs <- map(new_grids_list, ~make_mesh(.x$data, xy_cols = c("X", "Y"), cutoff = 10)) |> map(~list(mesh = .))


### BASE SCENARIO ####
# parameter coefficients 
# coefs <- map(year_samps, ~prepare_coefs(., mod_coefs, n_depth_coefs, year_data = years, wind_coef = TRUE, treatment = 0)) |> 
#   map(~list(b = .))
coefs <- map(nsims, ~prepare_coefs(mod_coefs, n_depth_coefs, wind_coef = TRUE, treatment = 0)) |> 
  map(~list(b = .))

# append locations and mesh 
# data <- map2(surv_locs_list, surv_meshs, ~append(.x, .y)) |> 
#   map2(coefs, ~append(.x, .y))
data <- map2(sim_locs, surv_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))
grid_data <- map2(new_grids_list, grid_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

sim.base.tows <- map2(data, seed, ~sdmTMB_simulate(
  formula = ~#1 + 
    poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) +
    fix_omegas - 1,
  data = .x$data,
  mesh = .x$mesh,
  family = tweedie(),
  #spatial = "on",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = ran_pars$estimate[1], 
  sigma_O = 0,
  sigma_E = ran_pars$estimate[4],
  phi = ran_pars$estimate[2],
  tweedie_p = ran_pars$estimate[5],
  seed = .y)
)

base.tows.df <- map(sim.base.tows, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~right_join(.x, .y, by = join_by(x == X, y == Y, est_year == EST_YEAR, as_factor_area_wind == AREA_CODE)) |> rename(AREA_CODE = as_factor_area_wind, EST_YEAR = est_year)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "Baseline", 
         season = season)

sim.base.grid <- map2(grid_data, seed, ~sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) +
    fix_omegas,
  data = .x$data,
  mesh = .x$mesh,
  family = tweedie(),
  #spatial = "on",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = ran_pars$estimate[1], 
  sigma_O = 0,
  sigma_E = ran_pars$estimate[4],
  phi = ran_pars$estimate[2],
  tweedie_p = ran_pars$estimate[5],
  seed = .y)
)

base.grid.df <- map(sim.base.grid, ~dplyr::select(., c(EST_YEAR, X, Y, observed))) |>
  map2(new_grids, ~merge(.x, .y, by = c("X", "Y", "EST_YEAR")) |> group_by(future_year) |> distinct(X, Y, cell, EST_YEAR, STRATUM , .keep_all = TRUE)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "Baseline", 
         season = season)

### save the data 
saveRDS(sim.base.tows, here(test.dat, str_c(season, species, "baseline", length(nsims), "sims.rds", sep = "_")))


### ENHANCED SCENARIO ####
# parameter coefficients 
# coefs <- map(year_samps, ~prepare_coefs(., mod_coefs, n_depth_coefs, year_data = years, wind_coef = TRUE, treatment = (log(2)))) |> 
#   map(~list(b = .))
coefs <- map(nsims, ~prepare_coefs(mod_coefs, n_depth_coefs, wind_coef = TRUE, treatment = (log(2)))) |> 
  map(~list(b = .))


# append locations and mesh 
# data <- map2(surv_locs_list, surv_meshs, ~append(.x, .y)) |> 
#   map2(coefs, ~append(.x, .y))
data <- map2(surv_locs, surv_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))
grid_data <- map2(new_grids_list, grid_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))

sim.inc.tows <- map2(data, seed, ~sdmTMB_simulate(
  formula = ~#1 + 
    poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) +
    fix_omegas - 1,
  data = .x$data,
  mesh = .x$mesh,
  family = tweedie(),
  #spatial = "on",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = ran_pars$estimate[1], 
  sigma_O = 0,
  sigma_E = ran_pars$estimate[4],
  phi = ran_pars$estimate[2],
  tweedie_p = ran_pars$estimate[5],
  seed = .y)
)

inc.tows.df <- map(sim.inc.tows, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~right_join(.x, .y, by = join_by(x == X, y == Y, est_year == EST_YEAR, as_factor_area_wind == AREA_CODE)) |> rename(AREA_CODE = as_factor_area_wind, EST_YEAR = est_year)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "Enhanced", 
         season = season)

sim.inc.grid <- map2(grid_data, seed, ~sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) +
    fix_omegas,
  data = .x$data,
  mesh = .x$mesh,
  family = tweedie(),
  #spatial = "on",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = ran_pars$estimate[1], 
  sigma_O = 0,
  sigma_E = ran_pars$estimate[4],
  phi = ran_pars$estimate[2],
  tweedie_p = ran_pars$estimate[5],
  seed = .y)
)

inc.grid.df <- map(sim.inc.grid, ~dplyr::select(., c(EST_YEAR, X, Y, observed))) |>
  map2(new_grids, ~merge(.x, .y, by = c("X", "Y", "EST_YEAR")) |> group_by(future_year) |> distinct(X, Y, cell, EST_YEAR, STRATUM , .keep_all = TRUE)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "Enhanced", 
         season = season)

### save the data 
saveRDS(sim.inc.tows, here(test.dat, str_c(season, species, "enhanced", length(nsims), "sims.rds", sep = "_")))


### REDUCED SCENARIO ####
# parameter coefficients 
# coefs <- map(year_samps, ~prepare_coefs(., mod_coefs, n_depth_coefs, year_data = years, wind_coef = TRUE, treatment = (-log(2)))) |> 
#   map(~list(b = .))
coefs <- map(nsims, ~prepare_coefs(mod_coefs, n_depth_coefs, wind_coef = TRUE, treatment = (-log(2)))) |> 
  map(~list(b = .))

# append locations and mesh 
# data <- map2(surv_locs_list, surv_meshs, ~append(.x, .y)) |> 
#   map2(coefs, ~append(.x, .y))
data <- map2(surv_locs, surv_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))
grid_data <- map2(new_grids_list, grid_meshs, ~append(.x, .y)) |> 
  map2(coefs, ~append(.x, .y))


sim.dec.tows <- map2(data, seed, ~sdmTMB_simulate(
  formula = ~#1 + 
    poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) +
    fix_omegas - 1,
  data = .x$data,
  mesh = .x$mesh,
  family = tweedie(),
  #spatial = "on",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = ran_pars$estimate[1], 
  sigma_O = 0,
  sigma_E = ran_pars$estimate[4],
  phi = ran_pars$estimate[2],
  tweedie_p = ran_pars$estimate[5],
  seed = .y)
)

dec.tows.df <- map(sim.dec.tows, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~right_join(.x, .y, by = join_by(x == X, y == Y, est_year == EST_YEAR, as_factor_area_wind == AREA_CODE)) |> rename(AREA_CODE = as_factor_area_wind, EST_YEAR = est_year)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "Reduced", 
         season = season)

sim.dec.grid <- map2(grid_data, seed, ~sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) +
    fix_omegas,
  data = .x$data,
  mesh = .x$mesh,
  family = tweedie(),
  #spatial = "on",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = ran_pars$estimate[1], 
  sigma_O = 0,
  sigma_E = ran_pars$estimate[4],
  phi = ran_pars$estimate[2],
  tweedie_p = ran_pars$estimate[5],
  seed = .y)
)

dec.grid.df <- map(sim.dec.grid, ~dplyr::select(., c(EST_YEAR, X, Y, observed))) |>
  map2(new_grids, ~merge(.x, .y, by = c("X", "Y", "EST_YEAR")) |> group_by(future_year) |> distinct(X, Y, cell, EST_YEAR, STRATUM , .keep_all = TRUE)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "Reduced", 
         season = season)

### save the data 
saveRDS(sim.dec.tows, here(test.dat, str_c(season, species, "reduced", length(nsims), "sims.rds", sep = "_")))



