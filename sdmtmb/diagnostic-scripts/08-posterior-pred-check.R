### created: 05/03/2024
### last updated: 05/10/2025

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
mod <- readRDS(here(dat.files, "mods", season, str_c("m10_", season, ".rds", sep = ""))) ## FIXME as needed
# mod <- readRDS(here(dat.files, "mods", "dpg", "no-dep-out", str_c("m8_", season, ".rds", sep = "")))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

# strata and year combinations that will be removed during the stratified mean calculations because only one tow was conducted. created from `05b-spatial-filter-data.R` here("tidy-data").
one_tow_strata <- readRDS(here("data", "rds", "one_tow_strata.rds")) |> 
  filter(SEASON == str_to_upper(season))

### Data wrangle ####
# create data frames of year and strata combinations where only one tow occurred
### Status quo effort 
remove_incl_strata <- one_tow_strata |>
  select(!precl_strata) |> unnest(cols = incl_strata)

### Wind-precluded effort 
remove_precl_strata <- one_tow_strata |>
  select(!incl_strata) |> unnest(cols = precl_strata)

# extract model fitting data, convert factors, and remove strata with only one tow before randomly resampling  
moddat <- mod$data |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)), 
         AREA = as.character(AREA)) #|>
  # anti_join(remove_incl_strata, by = c("SEASON", "EST_YEAR", "STRATUM"))

# extract model predictions
preds <- predict(mod) |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR))) |> 
  select(!EXPCATCHWT) |> 
  mutate(EXPCATCHWT = exp(est))
  # mutate(EXPCATCHWT = exp(est1 + est2)) # for delta gamma models

## SIMULATE DATASETS ####
simdat <- simulate(mod, nsim = 1000, re_form = NULL, type = "mle-mvn") # simulate using estimated random effects

# create unique column names 
colnames(simdat) <- paste0("sim", seq(1:1000))

### Bind datasets for calculations ####
simdat <- moddat |> 
  select(SVSPP, TOWID, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) |>
  bind_cols(simdat) |>
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") 


## CALCULATE STRATIFIED MEANS
source(here("R", "StratMeanFXs_v2.R")) # stratified mean function

### Estimates for data used to fit the model ####
### Status quo observations
incl_moddat <- moddat |> group_by(EST_YEAR, SEASON, STRATUM) |> 
  anti_join(remove_incl_strata, by = c("EST_YEAR", "SEASON", "STRATUM")) 

### for atlantic mackerel 
# incl_mod_indiv <- incl_moddat |>
#   group_by(STRATUM, EST_YEAR, SEASON) |>
#   summarise(towct = length(unique(TOWID))) |>
#   filter(towct == 1)
# 
# incl_moddat <- anti_join(incl_moddat, incl_mod_indiv, by = c("EST_YEAR", "SEASON", "STRATUM"))

incl_moddat_stratmus <- incl_moddat |> 
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu_sq = map(data, ~stratified.mean(., strata) |>
                            mutate(effort = "Status quo survey effort"))) |> 
  select(!data) |> 
  unnest(cols = stratmu_sq)

### Wind-precluded observations
precl_moddat <- moddat |> group_by(EST_YEAR, SEASON, STRATUM) |> 
  filter(AREA == "OUTSIDE") |>
  anti_join(remove_precl_strata, by = c("EST_YEAR", "SEASON", "STRATUM"))

### for atlantic mackerel 
# precl_moddat_indiv <- precl_moddat |> 
#   group_by(STRATUM, EST_YEAR, SEASON) |>
#   summarise(towct = length(unique(TOWID))) |> 
#   filter(towct == 1)
# 
# precl_moddat <- anti_join(precl_moddat, precl_moddat_indiv, by = c("EST_YEAR", "SEASON", "STRATUM"))

precl_moddat_stratmus <- precl_moddat |> 
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu_precl = map(data, ~stratified.mean(., strata) |>
                               mutate(effort = "Wind-precluded survey effort"))) |> 
  select(!data) |> 
  unnest(cols = stratmu_precl)

### Model predicted estimates ####
### Status quo predictions 
incl_preds <- preds |> group_by(EST_YEAR, SEASON, STRATUM) |> 
  anti_join(remove_incl_strata, by = c("EST_YEAR", "SEASON", "STRATUM")) 

### for atlantic mackerel 
# incl_individual <- incl_preds |>
#   group_by(STRATUM, EST_YEAR, SEASON) |>
#   summarise(towct = length(unique(TOWID))) |>
#   filter(towct == 1)
# 
# incl_preds <- anti_join(incl_preds, incl_individual, by = c("EST_YEAR", "SEASON", "STRATUM"))
  
incl_preds_stratmus <- incl_preds |> 
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu_sq = map(data, ~stratified.mean(., strata) |>
                            mutate(effort = "Status quo survey effort"))) |> 
  select(!data) |> 
  unnest(cols = stratmu_sq)

### Wind-precluded predictions
precl_preds <- preds |> group_by(EST_YEAR, SEASON, STRATUM) |> 
  filter(AREA == "OUTSIDE") |>
  anti_join(remove_precl_strata, by = c("EST_YEAR", "SEASON", "STRATUM"))

### for atlantic mackerel 
# precl_individual <- precl_preds |> 
#   group_by(STRATUM, EST_YEAR, SEASON) |>
#   summarise(towct = length(unique(TOWID))) |> 
#   filter(towct == 1)
# 
# precl_preds <- anti_join(precl_preds, precl_individual, by = c("EST_YEAR", "SEASON", "STRATUM"))

precl_preds_stratmus <- precl_preds |> 
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu_precl = map(data, ~stratified.mean(., strata) |>
                            mutate(effort = "Wind-precluded survey effort"))) |> 
  select(!data) |> 
  unnest(cols = stratmu_precl)

### Simulated estimates ####
# calculate the stratified mean abundance index for each scenario's replicate and year 

### Status quo survey effort 
incl_simdat <- anti_join(simdat, remove_incl_strata, by = c("SEASON", "EST_YEAR", "STRATUM")) #|> 
  #for atlantic mackerel only
  # anti_join(incl_individual, by = c("SEASON", "EST_YEAR", "STRATUM"))

incl_stratmus <- incl_simdat |>
  group_by(EST_YEAR, nsim) |> 
  nest() |>
  mutate(stratmu_sq = map(data, ~stratified.mean(., strata) |>
                            mutate(effort = "Status quo survey effort"))) |> 
  select(!data) |> 
  unnest(cols = stratmu_sq)
  # mutate(stratmu_sq = map(data, ~stratified.mean(., strata) |> # calculate status quo indices
  #                           mutate(effort = "With Wind Included",
  #                                  season = str_to_title(season), 
  #                                  EST_YEAR = EST_YEAR, 
  #                                  nsim = nsim)), 
  #        stratmu_precl = map(data , ~filter(., AREA == "OUTSIDE") |> # calculate precluded indices
  #                              stratified.mean(strata) |> 
  #                              mutate(effort = "With Wind Precluded",
  #                                     season = str_to_title(season), 
  #                                     EST_YEAR = EST_YEAR, 
  #                                     nsim = nsim))) 

### Status quo survey effort 
precl_simdat <- simdat |> 
  filter(AREA == "OUTSIDE") |>
  anti_join(remove_precl_strata, by = c("SEASON", "EST_YEAR", "STRATUM")) #|> 
  #for atlantic mackerel only
  # anti_join(precl_individual, by = c("SEASON", "EST_YEAR", "STRATUM"))

precl_stratmus <- precl_simdat |> 
  group_by(EST_YEAR, nsim) |> 
  nest() |>
  mutate(stratmu_precl = map(data, ~stratified.mean(., strata) |>
                            mutate(effort = "Wind-precluded survey effort"))) |> 
  select(!data) |> 
  unnest(cols = stratmu_precl)

## BIND STRATIFIED MEANS ####
### Model fitting means #### 
moddat_strat_rows <- bind_rows(incl_moddat_stratmus, precl_moddat_stratmus)

moddat_strat_cols <- left_join(incl_moddat_stratmus, precl_moddat_stratmus, by = c("EST_YEAR"))

### Model predicted means #### 
pred_strat_rows <- bind_rows(incl_preds_stratmus, precl_preds_stratmus)

pred_strat_cols <- left_join(incl_preds_stratmus, precl_preds_stratmus, by = c("EST_YEAR"))


### Simulated means ####
sim_strat_rows <- bind_rows(incl_stratmus, precl_stratmus)

sim_strat_cols <- left_join(incl_stratmus, precl_stratmus, by = c("EST_YEAR", "nsim"))


## FIT LINEAR REGRESSIONS ####
### Model fitting trends ####
# estimate the change in model predicted abundance index over time for each scenario
moddat_slopes <- moddat_strat_rows |> 
  group_by(effort) |> 
  nest() |> 
  mutate(mods = map(data, ~lm(stratmu~EST_YEAR, data = .)), # fit models
         coefs = map(mods, broom::tidy, conf.int = TRUE), # extract coefficients 
         season = str_to_title(season)) |> 
  unnest(cols = coefs) |> 
  select(!c(data, mods)) |> 
  filter(term == "EST_YEAR") # extract the slopes

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
# map(preds_stratmus, ~calc.errors(., .x$stratmu_sq, .x$stratmu_precl))


## SAVE THE DATA ####
data <- list("post-check_simdat.rds" = simdat,
             "post-check_incl_simdat.rds" = incl_simdat,
             "post-check_precl_simdat.rds" = precl_simdat,
             "precl_stratmus.rds" = precl_stratmus,
             "incl_stratmus.rds" = incl_stratmus,
             # "sim_stratmus.rds" = sim_stratmus,
             "pred_strat_cols.rds" = pred_strat_cols,
             "pred_strat_rows.rds" = pred_strat_rows,
             "sim_strat_rows.rds" = sim_strat_rows, 
             "sim_strat_cols.rds" = sim_strat_cols,
             "pred_slopes.rds"= pred_slopes, 
             "post-check_sim-slopes.rds" = sim_slopes, 
             "post-check_moddat.rds" = moddat,
             "post-check_moddat_slopes.rds" = moddat_slopes,
             "post-check_moddat_strat_rows.rds" = moddat_strat_rows,
"post-check_precl_moddat.rds" = precl_moddat,
"precl_stratmus_moddat.rds" = precl_stratmus_moddat,
"incl_stratmus_moddat.rds" = incl_stratmus_moddat,
"post-check_incl_moddat.rds" = incl_moddat)


pmap(list(data, names(data)), ~saveRDS(.x, here(post.check, "042025", str_c(season, .y, sep = "_"))))
