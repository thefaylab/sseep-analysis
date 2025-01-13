### created: 09/22/2024
### updated: 

# 10 - Calculate simulated stratified means ####

## OBJECTIVE ####


### Load Packages #### 
library(here)
library(tictoc)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
source(here("R", "StratMeanFXs_v2.R"))

### Environment Set-Up ####
# set season and species to be simulated
season <- "spring"

species <- "atlmackerel"

# type of scenario simulated - c("no-wind", "baseline", "enhanced", "reduced")
scenario <- "baseline"

# number of simulations 
nsims <- 1:1000

# set file locations of where to read in data and save outputs
dat.files <- here("data", "rds", "sdmtmb", species)
sim.dat <- here("data", "rds", "sdmtmb", species, "simulations", season, scenario)

### Load Data ####
id <- str_c(season, species, scenario, length(nsims), sep = "_")

# simulated dataframe from here("sdmtmb", "08-sim-tow-scenarios.R")
data <- readRDS(here(sim.dat, str_c(id, "sim-tows-df.rds", sep = "_")))

# load the active bottom trawl survey strata and their relative area weights 
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

## Calculate Stratified Mean ####
sim_stratmeans <- data |> 
  group_by(FUTURE_YEAR, SIM, SCENARIO, SEASON) |> 
  nest() |> 
  mutate(incl_stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Included")), 
         precl_stratmu = map(data, ~filter(., AREA == "OUTSIDE") |> stratified.mean(strata) |> mutate(effort = "With Wind Precluded")))

# extract precluded indices only
sim_precluded <- sim_stratmeans |> 
  select(!c(data, incl_stratmu)) |> 
  unnest(cols = precl_stratmu)

# extract included indices only 
sim_included <- sim_stratmeans |> 
  select(!c(data, precl_stratmu)) |> 
  unnest(cols = incl_stratmu)

## Bind Data ####
# long dataframe for plotting and linear regression models
stratmeans_rows <- bind_rows(sim_included, sim_precluded) |> janitor::clean_names("all_caps")

# wide dataframe for error statistics 
stratmeans_cols <- left_join(sim_included, sim_precluded, by = c("SIM", "FUTURE_YEAR", "SEASON", "SCENARIO")) |> janitor::clean_names("all_caps")

## ESTIMATE POPULATION TRENDS ####
stratmu_linreg <- stratmeans_rows |> 
  group_by(EFFORT, SCENARIO, SEASON, SIM) |> 
  nest() |> 
  mutate(model = map(data, ~lm(STRATMU~FUTURE_YEAR, data = .)), 
         coefs = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(coefs) |>
  select(SIM, SCENARIO, EFFORT, term, estimate, statistic, p.value, conf.low, conf.high) |> 
  filter(term == "FUTURE_YEAR")

# wrangle to calculate MRE and MARE
precl_linreg <- stratmu_linreg |> 
  filter(EFFORT == "With Wind Precluded")
incl_linreg <- stratmu_linreg |> 
  filter(EFFORT == "With Wind Included")
linreg_cols <- left_join(incl_linreg, precl_linreg, by = c("SIM", "SCENARIO", "SEASON")) 


## CALCULATE RELATIVE PERCENT DIFFERENCE ####
# between stratified means
mudiff <- stratmeans_cols |> 
  group_by(SIM, SCENARIO, SEASON) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = STRATMU_X, observed = STRATMU_Y)), # calculate the relative errors 
         mudiff = map(errors, ~mean.diff(.))) |> # calculate the mean percent difference
  dplyr::select(!c(data, errors)) |> 
  unnest(cols = mudiff)

# between cvs
cvdiff <-  stratmeans_cols |> 
  group_by(SIM, SCENARIO, SEASON) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = CV_X, observed = CV_Y)), # calculate the relative errors 
         cv.diff = map(errors, ~mean.diff(.))) |> # calculate the mean percent difference
  dplyr::select(!c(data, errors)) |> 
  unnest(cols = cv.diff)

# between slopes
linreg_diff <- linreg_cols |> 
  group_by(SIM, SCENARIO, SEASON) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = estimate.x, observed = estimate.y)), # calculate the relative errors 
         slope.diff = map(errors, ~mean.diff(.))) |> # calculate the mean percent difference
  dplyr::select(!c(data, errors)) |> 
  unnest(cols = slope.diff)

## Save the data ####
data.save <- list(sim_included, 
                  sim_precluded,
                  stratmeans_rows,
                  stratmeans_cols,
                  stratmu_linreg,
                  linreg_cols,
                  mudiff, 
                  cvdiff,
                  linreg_diff)
names(data.save) <- c(str_c(id, "sim.included.stratmu.rds", sep = "_"), 
                      str_c(id, "sim.precluded.stratmu.rds", sep = "_"),
                      str_c(id, "sim.stratmu_rows.rds", sep = "_"),
                      str_c(id, "sim.stratmu_cols.rds", sep = "_"),
                      str_c(id, "sim.stratmu.slopes.rds", sep = "_"),
                      str_c(id, "sim.stratmu.slopes_cols.rds", sep = "_"),
                      str_c(id, "sim.stratmu_diff.rds", sep = "_"), 
                      str_c(id, "sim.cv_diff.rds", sep = "_"),
                      str_c(id, "sim.slope_diff.rds", sep = "_"))

pmap(list(data.save, names(data.save)), ~saveRDS(.x, here(sim.dat, .y)))
