### created: 
### updated: 

# 09 - Simulate grid scenarios ####

## OBJECTIVE ####


### Load Packages #### 
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 

### Environment Set-Up ####
set.seed(72346)
# set season and species to be simulated
season <- "fall"

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

### Load Data ####
# depth relationship coefficients used in model fitting
depth_coefs <- readRDS(here(dat.files, str_c(season, "depth_coefs.rds", sep = "_")))

# number of depth coefficients used in model fitting
n_depth_coefs <- length(depth_coefs)

# sampled locations to serve as future survey locations
future_locs <- readRDS(here(sim.dat, str_c(season, species, "future_locs.rds", sep = "_")))

# sampled locations to serve as future survey locations
future_grids <- readRDS(here(sim.dat, str_c(season, species, "future_grids.rds", sep = "_")))


# data prepared for simulation including future grid locations, mesh, and coefficients
grid_data <- readRDS(here(sim.dat, str_c(season, species, scenario, "sim-grid-dat.rds", sep = "_")))


## Simulate data ####
sim.grids <- map2(grid_data, seed, ~sdmTMB_simulate(
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
  range = ran_pars$estimate[1],                   # FIXME if independent ranges
  sigma_O = 0,
  sigma_E = ran_pars$estimate[4],
  phi = ran_pars$estimate[2],
  tweedie_p = ran_pars$estimate[5],
  seed = .y)
)

### Data wrangle ####
grids.df <- map(sim.grids[1:5], ~dplyr::select(., c(EST_YEAR, X, Y, observed))) |>
  map2(future_grids[1:5], ~merge(.x, .y, by = c("X", "Y", "EST_YEAR")) |> group_by(future_year) |> distinct(X, Y, cell, EST_YEAR, STRATUM , .keep_all = TRUE)) |> 
  map2_dfr(nsims[1:5], ~mutate(.x, sim = .y)) |> 
  mutate(scenario = scenario, 
         season = season) |> 
  select(!c(x_2, y_2, est_year_2))

### save the data 
data.save <- list(sim.grids, 
                  grids.df)
  
names(data.save) <- c(str_c(season, species, scenario, length(nsims), "sim-grids.rds", sep = "_"), 
                      str_c(season, species, scenario, length(nsims), "sim-grids-df.rds", sep = "_")
                      )

pmap(list(data.save, names(data.save)), ~saveRDS(.x, here(sim.dat, .y)))




