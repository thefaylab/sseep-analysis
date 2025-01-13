### created: 05/03/2024
### last updated: 10/31/2024

# 08 - DIAGNOSTICS: POSTERIOR PREDICTIVE CHECK ####

## OBJECTIVE ####
# simulate new data with fitted model 
# calculate abundance index and change in abundance index over time for simulated data and model predicted data


### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(infer)
library(nationalparkcolors)
library(patchwork)
set.seed(123)
theme_set(theme_bw())

### Environment Set Up ####
# season 
season <- "fall"

# species
species <- "sumflounder"

species_name <- "summer flounder"

### File locations ####
dat.files <- here("data", "rds", "sdmtmb",  species) 
post.check <- here(dat.files, "post-check") 
plot.files <- here("outputs", "sdmtmb", species, "plots", "post-check")

### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, "mods", season, str_c("m12_", season, ".rds", sep = ""))) ## FIXME as needed
# mod <- readRDS(here(dat.files, "mods", "dpg", "no-dep-out", str_c("m8_", season, ".rds", sep = "")))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

### Data wrangle ####
# extract model fitting data and convert factors 
moddat <- mod$data |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)), 
         AREA = as.character(AREA))

# extract model predictions
preds <- predict(mod) |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR))) |> 
  select(!EXPCATCHWT) |> 
  mutate(EXPCATCHWT = exp(est))

## SIMULATE DATASETS ####
simdat <- simulate(mod, nsim = 1000, re_form = NULL, type = "mle-mvn") # simulate using estimated random effects

# create unique column names 
colnames(simdat) <- paste0("sim", seq(1:1000))

### Bind datasets for calculations ####
simdat <- moddat |> 
  select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) |>
  bind_cols(simdat) |>
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") 


## CALCULATE STRATIFIED MEANS
source(here("R", "StratMeanFXs_v2.R")) # stratified mean function

### Model predicted estimates ####
preds_stratmus <- preds |> 
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu_sq = map(data, ~stratified.mean(., strata) |> # calculate status quo indices
                            mutate(effort = "With Wind Included",
                                   season = str_to_title(season), 
                                   EST_YEAR = EST_YEAR)), 
         stratmu_precl = map(data , ~filter(., AREA == "OUTSIDE") |> # calculate precluded indices
                               stratified.mean(strata) |> 
                               mutate(effort = "With Wind Precluded",
                                      season = str_to_title(season),
                                      EST_YEAR = EST_YEAR))) |> 
  arrange(EST_YEAR)

### Simulated estimates ####
# calculate the stratified mean abundance index for each scenario's replicate and year 
sim_stratmus <- simdat |> 
  group_by(EST_YEAR, nsim) |> 
  nest() |>
  mutate(stratmu_sq = map(data, ~stratified.mean(., strata) |> # calculate status quo indices
                            mutate(effort = "With Wind Included",
                                   season = str_to_title(season), 
                                   EST_YEAR = EST_YEAR, 
                                   nsim = nsim)), 
         stratmu_precl = map(data , ~filter(., AREA == "OUTSIDE") |> # calculate precluded indices
                               stratified.mean(strata) |> 
                               mutate(effort = "With Wind Precluded",
                                      season = str_to_title(season), 
                                      EST_YEAR = EST_YEAR, 
                                      nsim = nsim))) 



## BIND STRATIFIED MEANS ####
### Model predicted means #### 
pred_strat_rows <- bind_rows(preds_stratmus$stratmu_sq, preds_stratmus$stratmu_precl)

pred_strat_cols <- preds_stratmus |>
  select(!data) |> 
  unnest(cols = c(stratmu_sq, stratmu_precl), names_repair = "unique") |> 
  janitor::clean_names()

### Simulated means ####
sim_strat_rows <- bind_rows(sim_stratmus$stratmu_sq, sim_stratmus$stratmu_precl)

sim_strat_cols <- sim_stratmus |>
  select(!data) |> 
  unnest(cols = c(stratmu_sq, stratmu_precl), names_repair = "unique") |> 
  janitor::clean_names()

## FIT LINEAR REGRESSIONS ####
### Model predicted trends ####
# estimate the change in model predicted abundance index over time for each scenario
pred_slopes <- pred_strat_rows |> 
  group_by(effort) |> 
  nest() |> 
  mutate(mods = map(data, ~lm(stratmu~EST_YEAR, data = .)), # fit models
         coefs = map(mods, broom::tidy, conf.int = TRUE), # extract coefficients 
         season = str_to_title(season)) |> 
  unnest(cols = coefs) |> 
  select(!c(data, mods)) |> 
  filter(term == "EST_YEAR") # extract the slopes


### Simulated trends ####
# estimate the simulated change in abundance index over time for each scenario
sim_slopes <- sim_strat_rows |>
  group_by(nsim, effort) |> 
  nest() |> 
  #select(nsim, stratmu, SEASON, TYPE) |> 
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)), # fit models
         coef = map(model, broom::tidy, conf.int = TRUE),  # extract coefficients
         season = str_to_title(season)) |> 
  unnest(cols = coef) |>
  select(nsim, term, estimate, conf.low, conf.high, season, effort) |>
  filter(term == "EST_YEAR") # extract the slopes


## CALCULATE ERRORS #### 
map(preds_stratmus, ~calc.errors(., .x$stratmu_sq, .x$stratmu_precl))


## SAVE THE DATA ####
data <- list("post-check_simdat.rds" = simdat,
             "preds_stratmus.rds" = preds_stratmus,
             "sim_stratmus.rds" = sim_stratmus,
             "preds_strat_rows.rds" = pred_strat_rows, 
             "sim_strat_rows.rds" = sim_strat_rows, 
             "pred_slopes.rds"= pred_slopes, 
             "post-check_sim-slopes.rds" = sim_slopes) 

pmap(list(data, names(data)), ~saveRDS(.x, here(post.check, str_c(season, .y, sep = "_"))))
