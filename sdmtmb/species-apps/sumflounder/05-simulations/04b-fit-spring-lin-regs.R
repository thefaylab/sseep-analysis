### created: 08/17/2023
### last updated: 

# 04a - FIT LINEAR REGRESSIONS: FALL SCENARIOS ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"
source(here("R", "StratMeanFXs_v2.R"))
#set.seed(123)

### LOAD DATA ####
# created here("sdmtmb", "sumflounder", "05-simulations", "02a-calculate-fall-sim-stratmu.R"))
base_stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseStratmu.rds"))

inc_stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseStratmu.rds"))

dec_stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseStratmu.rds"))

obs_stratmu <- readRDS(here("data", "sumflounder", "sf_stratmu.rds")) |> filter(EST_YEAR %in% c(2016:2021), SEASON == "SPRING")

### DATA WRANGLE ####
obs_stratmu <- map_dfr(seq(1:1000), ~obs_stratmu |>mutate(rep = .x))

obs_inc.stratmu <- obs_stratmu |> 
  filter(TYPE == "With Wind Included") |>
  dplyr::select(!c(SEASON, COMNAME, SCINAME))

obs_precl.stratmu <- obs_stratmu |> 
  filter(TYPE == "With Wind Precluded") |> 
  dplyr::select(!c(SEASON, COMNAME, SCINAME))

## FIT LINEAR REGRESSIONS ####
### Base Scenario ####
# base_lms <- base_stratmu |> 
#   mutate(incl.model = map(incl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
#          precl.model = map(precl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
#          incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE),
#          precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE))
# 
# saveRDS(base_lms, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseLMs.rds"))
#### With Wind Included ####
base_incl.lm <- base_stratmu |>
  dplyr::select(rep, incl.stratmu) |> 
  unnest(incl.stratmu) |> 
  mutate(TYPE = "With Wind Included") |>
  bind_rows(obs_inc.stratmu) |> 
  group_by(rep) |> 
  nest() |>
  mutate(incl.model = map(data, ~lm(stratmu~EST_YEAR, data = .x)),
         incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE)) 

saveRDS(base_incl.lm, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseIncludedLMs.rds"))
  
base_incl.slopes <- base_incl.lm |>
  dplyr::select(rep, incl.coefs) |>
  unnest(incl.coefs) |>
  filter(term == "EST_YEAR") |>
  mutate(TYPE = "With Wind Included")

saveRDS(base_incl.slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseIncludedSlopes.rds"))

#### With Wind Precluded ####
base_precl.lm <- base_stratmu |>
  dplyr::select(rep, precl.stratmu) |> 
  unnest(precl.stratmu) |> 
  mutate(TYPE = "With Wind Precluded") |>
  bind_rows(obs_precl.stratmu) |> 
  group_by(rep) |> 
  nest() |>
  mutate(precl.model = map(data, ~lm(stratmu~EST_YEAR, data = .x)),
         precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE)) 

saveRDS(base_precl.lm, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BasePrecludedLMs.rds"))

base_precl.slopes <- base_precl.lm |>
  dplyr::select(rep, precl.coefs) |>
  unnest(precl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Precluded")

saveRDS(base_precl.slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BasePrecludedSlopes.rds"))

base_slopes <- bind_rows(base_incl.slopes, base_precl.slopes) |> 
  mutate(SCENARIO = "BASELINE")

saveRDS(base_slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseSlopes.rds"))


### Increase Scenario ####
#### With Wind Included ####
# inc_lms <- inc_stratmu |>
#   mutate(incl.model = map(incl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
#          precl.model = map(precl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)), 
#          incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE), 
#          precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE))
# 
# saveRDS(inc_lms, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseLM.rds"))

inc_incl.lm <- inc_stratmu |>
  dplyr::select(rep, incl.stratmu) |> 
  unnest(incl.stratmu) |> 
  mutate(TYPE = "With Wind Included") |>
  bind_rows(obs_inc.stratmu) |> 
  group_by(rep) |> 
  nest() |>
  mutate(incl.model = map(data, ~lm(stratmu~EST_YEAR, data = .x)),
         incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE)) 

saveRDS(inc_incl.lm, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseIncludedLMs.rds"))

inc_incl.slopes <- inc_incl.lm |>
  dplyr::select(rep, incl.coefs) |>
  unnest(incl.coefs) |>
  filter(term == "EST_YEAR") |>
  mutate(TYPE = "With Wind Included")

saveRDS(inc_incl.slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseIncludedSlopes.rds"))

#### With Wind Precluded ####
inc_precl.lm <- inc_stratmu |>
  dplyr::select(rep, precl.stratmu) |> 
  unnest(precl.stratmu) |> 
  mutate(TYPE = "With Wind Precluded") |>
  bind_rows(obs_precl.stratmu) |> 
  group_by(rep) |> 
  nest() |>
  mutate(precl.model = map(data, ~lm(stratmu~EST_YEAR, data = .x)),
         precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE)) 

saveRDS(inc_precl.lm, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreasePrecludedLMs.rds"))

inc_precl.slopes <- inc_precl.lm |>
  dplyr::select(rep, precl.coefs) |>
  unnest(precl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Precluded")

saveRDS(inc_precl.slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreasePrecludedSlopes.rds"))

inc_slopes <- bind_rows(inc_incl.slopes, inc_precl.slopes) |> 
  mutate(SCENARIO = "ENHANCED")

saveRDS(inc_slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseSlopes.rds"))


### Decrease Scenario ####
# dec_lms <- dec_stratmu |> 
#   mutate(incl.model = map(incl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
#          precl.model = map(precl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
#          incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE), 
#          precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE)) 
#          
# 
# saveRDS(dec_lms, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseLM.rds"))
#### With Wind Included ####
dec_incl.lm <- dec_stratmu |>
  dplyr::select(rep, incl.stratmu) |> 
  unnest(incl.stratmu) |> 
  mutate(TYPE = "With Wind Included") |>
  bind_rows(obs_inc.stratmu) |> 
  group_by(rep) |> 
  nest() |>
  mutate(incl.model = map(data, ~lm(stratmu~EST_YEAR, data = .x)),
         incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE)) 

saveRDS(dec_incl.lm, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseIncludedLMs.rds"))

dec_incl.slopes <- dec_incl.lm |>
  dplyr::select(rep, incl.coefs) |>
  unnest(incl.coefs) |>
  filter(term == "EST_YEAR") |>
  mutate(TYPE = "With Wind Included")

saveRDS(dec_incl.slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseIncludedSlopes.rds"))

#### With Wind Precluded ####
dec_precl.lm <- dec_stratmu |>
  dplyr::select(rep, precl.stratmu) |> 
  unnest(precl.stratmu) |> 
  mutate(TYPE = "With Wind Precluded") |>
  bind_rows(obs_precl.stratmu) |> 
  group_by(rep) |> 
  nest() |>
  mutate(precl.model = map(data, ~lm(stratmu~EST_YEAR, data = .x)),
         precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE)) 

saveRDS(dec_precl.lm, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreasePrecludedLMs.rds"))

dec_precl.slopes <- dec_precl.lm |>
  dplyr::select(rep, precl.coefs) |>
  unnest(precl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Precluded")

saveRDS(dec_precl.slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreasePrecludedSlopes.rds"))

dec_slopes <- bind_rows(dec_incl.slopes, dec_precl.slopes) |> 
  mutate(SCENARIO = "REDUCED")

saveRDS(dec_slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseSlopes.rds"))



all_slopes <- bind_rows(base_slopes, inc_slopes, dec_slopes) 
saveRDS(all_slopes, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_AllSlopes.rds"))
