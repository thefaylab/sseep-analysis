### created: 06/04/2024
### updated: 10/06/2024

# 08 - Simulate scenarios ####

## OBJECTIVE ####


### Load Packages #### 
library(here)
library(tictoc)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 

### Environment Set-Up ####
set.seed(72346)
# set season and species to be simulated
season <- "spring"

species <- "sumflounder"

# set file locations of where to read in data and save outputs
dat.files <- here("data", "rds", "sdmtmb", species)
sim.dat <- here("data", "rds", "sdmtmb", species, "simulations", season)

# number of simulations 
nsims <- 1:1000

# generate a set random numbers to set seed with each iteration for reproducibility
seed <- sample.int(1e6, length(nsims))

# type of scenario to simulate - c("no-wind", "baseline", "enhanced", "reduced")
scenario <- "baseline"

# naming convention
id <- str_c(season, species, scenario, length(nsims), sep = "_")

### Load Data ####
# depth relationship coefficients used in model fitting
depth_coefs <- readRDS(here(dat.files, str_c(season, "depth_coefs.rds", sep = "_")))

# number of depth coefficients used in model fitting
n_depth_coefs <- length(depth_coefs$alpha)

# sampled locations to serve as future survey locations
future_locs <- readRDS(here(sim.dat, str_c(season, species, "future_locs.rds", sep = "_")))


# data prepared for simulation including future survey locations, mesh, and coefficients
data <- readRDS(here(sim.dat, str_c(season, species, scenario, "sim-locs-dat.rds", sep = "_")))


# model estimated random parameters 
ran_pars <- readRDS(here(dat.files, str_c(season, "mod_ran-pars.rds", sep = "_")))


## Simulate data ####
tic()
sim.tows <- map2(data, seed, ~sdmTMB_simulate(
  formula = ~#1 + 
    poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) +
    fix_omegas - 1,
  data = .x$data,
  mesh = .x$mesh,
  family = tweedie(),                             # FIXME if delta model
  #spatial = "on",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = ran_pars$estimate[1],                   # FIXME if independent ranges
  sigma_O = 0,
  sigma_E = ran_pars$estimate[4],
  phi = ran_pars$estimate[2],
  tweedie_p = ran_pars$estimate[5],
  seed = .y)
)
toc()

### Data wrangle ####
tows.df <- map(sim.tows, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~bind_cols(.x, .y) |> janitor::clean_names("all_caps")) |>  
  map2_dfr(nsims, ~mutate(.x, SIM = .y)) |> 
  mutate(SCENARIO = scenario, 
         SEASON = season) |> 
  rename(EXPCATCHWT = OBSERVED) |> 
  select(!c(X_2, Y_2, EST_YEAR_2))



## Save the data #### 
data.save <- list(sim.tows, 
                  tows.df)
  
names(data.save) <- c(str_c(id, "sim-tows.rds", sep = "_"),
                      str_c(id, "sim-tows-df.rds", sep = "_"))

pmap(list(data.save, names(data.save)), ~saveRDS(.x, here(sim.dat, scenario, .y)))




