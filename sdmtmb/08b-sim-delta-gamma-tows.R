### created: 1m/06/2024
### updated: 

# 08 - Simulate delta gamma scenarios ####

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

species <- "atlmackerel"

# set file locations of where to read in data and save outputs
dat.files <- here("data", "rds", "sdmtmb", species)
sim.dat <- here("data", "rds", "sdmtmb", species, "simulations", season)

# number of simulations 
nsims <- 1:1000

# generate a set random numbers to set seed with each iteration for reproducibility
seed <- sample.int(1e6, length(nsims))

# type of scenario to simulate - c("no-wind", "baseline", "enhanced", "reduced")
scenario <- "reduced"

# naming convention
id <- str_c(season, species, scenario, length(nsims), sep = "_")

### Load Data ####
# depth relationship coefficients used in model fitting
depth_coefs <- readRDS(here(dat.files, str_c(season, "depth_coefs.rds", sep = "_")))

# number of depth coefficients used in model fitting
n_depth_coefs <- length(depth_coefs$alpha)

# sampled locations to serve as future survey locations
future_locs <- readRDS(here(sim.dat, str_c(season, species, "future_locs.rds", sep = "_")))

#### Model 1 ####
# data prepared for simulation including future survey locations, mesh, and coefficients
data1 <- readRDS(here(sim.dat, "mod1", str_c(season, species, scenario, "sim-locs-dat.rds", sep = "_")))

# model estimated random parameters 
ran_pars1 <- readRDS(here(dat.files, str_c(season, "dpg-mod1_ran-pars.rds", sep = "_")))

#### Model 2 ####
# data prepared for simulation including future survey locations, mesh, and coefficients
data2 <- readRDS(here(sim.dat, "mod2", str_c(season, species, scenario, "sim-locs-dat.rds", sep = "_")))

# model estimated random parameters 
ran_pars2 <- readRDS(here(dat.files, str_c(season, "dpg-mod2_ran-pars.rds", sep = "_")))

## Simulate data ####
### Model 1 ####
tic()
sim.binom <- map2(data1, seed, ~sdmTMB_simulate(
  formula = ~#1 + 
    poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) +
    fix_omegas - 1,
  data = .x$data,
  mesh = .x$mesh,
  family = binomial(),                            
  #spatial = "on",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = c(ran_pars1$estimate[1:2]),                   
  sigma_O = 0,
  sigma_E = ran_pars1$estimate[4],
  #phi = ran_pars1$estimate[2],
  #tweedie_p = ran_pars1$estimate[5],
  seed = .y)
)
toc()

### Model 2 ####
tic()
sim.gamma <- map2(data2, seed, ~sdmTMB_simulate(
  formula = ~#1 + 
    poly(AVGDEPTH, n_depth_coefs, coefs = depth_coefs) +
    as.factor(EST_YEAR) +
    as.factor(AREA) - 1, # +
    #fix_omegas - 1,
  data = .x$data,
  mesh = .x$mesh,
  family = Gamma(link = "log"),                             
  spatial = "off",
  time = "EST_YEAR",
  spatiotemporal = "IID",
  B = .x$b,
  range = ran_pars2$estimate[1],                  
  #sigma_O = 0,
  sigma_E = ran_pars2$estimate[3],
  phi = ran_pars2$estimate[2],
  #tweedie_p = ran_pars2$estimate[5],
  seed = .y)
)
toc()

### Data wrangle ####
binom.tows <- map(sim.binom, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~bind_cols(.x, .y) |> janitor::clean_names("all_caps")) |>  
  map2_dfr(nsims, ~mutate(.x, SIM = .y)) |> 
  mutate(SCENARIO = scenario, 
         SEASON = season) |> 
  rename(BINOM_OBS = OBSERVED) |> 
  select(!c(X_2, Y_2, EST_YEAR_2))

gamma.tows <- map(sim.gamma, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~bind_cols(.x, .y) |> janitor::clean_names("all_caps")) |>  
  map2_dfr(nsims, ~mutate(.x, SIM = .y)) |> 
  mutate(SCENARIO = scenario, 
         SEASON = season) |> 
  rename(GAMMA_OBS = OBSERVED) |> 
  select(!c(X_2, Y_2, EST_YEAR_2))


tows.df <- left_join(binom.tows, gamma.tows, by = c("EST_YEAR", "X", "Y", "STATION", "CRUISE6", "STRATUM", "AVGDEPTH", "AREA", "FUTURE_YEAR", "TOWID", "SIM")) |> 
  mutate(EXPCATCHWT = BINOM_OBS * GAMMA_OBS) |> 
  select(!c(FIX_OMEGAS.x, FIX_OMEGAS.y, AS_FACTOR_AREA_WIND.x, AS_FACTOR_AREA_WIND.y, SCENARIO.y, AREA_CODE.y, SEASON.y)) |> 
  rename(SEASON = SEASON.x, 
         SCENARIO = SCENARIO.x, 
         AREA_CODE = AREA_CODE.x)
  

## Save the data ####
data.save <- list(sim.binom,
                  sim.gamma, 
                  binom.tows, 
                  gamma.tows,
                  tows.df)
  
names(data.save) <- c(str_c(id, "sim-binom.rds", sep = "_"),
  str_c(id, "sim-gamma.rds", sep = "_"), 
  str_c(id, "binom-tows.rds", sep = "_"),
  str_c(id, "gamma-tows.rds", sep = "_"), 
  str_c(id, "sim-tows-df.rds", sep = "_"))

pmap(list(data.save, names(data.save)), ~saveRDS(.x, here(sim.dat, scenario, .y)))




