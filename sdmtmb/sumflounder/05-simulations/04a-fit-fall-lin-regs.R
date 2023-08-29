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
base_stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseStratmu.rds"))

inc_stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseStratmu.rds"))

dec_stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseStratmu.rds"))


## FIT LINEAR REGRESSIONS ####
### Base Scenario ####
base_lms <- base_stratmu |> 
  mutate(incl.model = map(incl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
         precl.model = map(precl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
         incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE),
         precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE))

saveRDS(base_lms, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseLMs.rds"))

base_incl.slopes <- base_lms |>
  dplyr::select(rep, incl.coefs) |>
  unnest(incl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Included")
  
base_precl.slopes <- base_lms |>
  dplyr::select(rep, precl.coefs) |>
  unnest(precl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Precluded")

base_slopes <- bind_rows(base_incl.slopes, base_precl.slopes) |> 
  mutate(SCENARIO = "BASELINE")

saveRDS(base_slopes, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseSlopes.rds"))


### Increase Scenario ####
inc_lms <- inc_stratmu |>
  mutate(incl.model = map(incl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
         precl.model = map(precl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)), 
         incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE), 
         precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE))

saveRDS(inc_lms, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseLM.rds"))

inc_incl.slopes <- inc_lms |>
  dplyr::select(rep, incl.coefs) |>
  unnest(incl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Included") 

inc_precl.slopes <- inc_lms |>
  dplyr::select(rep, precl.coefs) |>
  unnest(precl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Precluded")

inc_slopes <- bind_rows(inc_incl.slopes, inc_precl.slopes) |> 
  mutate(SCENARIO = "ENHANCED")

saveRDS(inc_slopes, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseSlopes.rds"))


### Decrease Scenario ####
dec_lms <- dec_stratmu |> 
  mutate(incl.model = map(incl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
         precl.model = map(precl.stratmu, ~lm(stratmu~EST_YEAR, data = .x)),
         incl.coefs = map(incl.model, broom::tidy, conf.int = TRUE), 
         precl.coefs = map(precl.model, broom::tidy, conf.int = TRUE)) 
         

saveRDS(dec_lms, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseLM.rds"))

dec_incl.slopes <- dec_lms |>
  dplyr::select(rep, incl.coefs) |>
  unnest(incl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Included") 

dec_precl.slopes <- dec_lms |>
  dplyr::select(rep, precl.coefs) |>
  unnest(precl.coefs) |>
  filter(term == "EST_YEAR") |> 
  mutate(TYPE = "With Wind Precluded")

dec_slopes <- bind_rows(dec_incl.slopes, dec_precl.slopes) |> 
  mutate(SCENARIO = "REDUCED")

saveRDS(dec_slopes, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseSlopes.rds"))



all_slopes <- bind_rows(base_slopes, inc_slopes, dec_slopes) 
saveRDS(all_slopes, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_AllSlopes.rds"))
