### created: 08/23/2023
### last updated: 

# 02b -  Calculate Stratified Means for Spring Scenarios ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"
source(here("R", "StratMeanFXs_v2.R"))
#set.seed(123)


### LOAD DATA ####
# base simulation data 
sim_base_tows <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseTows.rds"))
# saveRDS(sim_base_grid, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseGrid.rds"))

# increased simulation data 
sim_increase_tows <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "0829-spring", "SprSimFuture_IncreaseTows.rds"))
# saveRDS(sim_increase_grid, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseGrid.rds"))

# reduced simulation data 
sim_decrease_tows <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseTows.rds"))
# saveRDS(sim_decrease_grid, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseGrid.rds"))


## CALCULATE STRATIFIED MEANS #### 
### Base Scenario ####
#start.time <- Sys.time()
base_stratmu <- sim_base_tows |>
  group_by(rep) |> 
  nest() |> 
  mutate(precluded.data = map(data, ~filter(., AREA == "OUTSIDE")), 
         incl.stratmu = map(data, ~stratified.mean(.)), 
         precl.stratmu = map(precluded.data, ~stratified.mean(.)))
#end.time <- Sys.time()
#end.time - start.time #11 min

saveRDS(base_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseStratmu.rds"))

base_incl.stratmu <- base_stratmu |> 
  dplyr::select(rep, incl.stratmu) |> 
  unnest(incl.stratmu) |> 
  mutate(TYPE = "With Wind Included") 

base_precl.stratmu <- base_stratmu |> 
  dplyr::select(rep, precl.stratmu) |> 
  unnest(precl.stratmu) |> 
  mutate(TYPE = "With Wind Precluded")

base_all.stratmu <- bind_rows(base_incl.stratmu, base_precl.stratmu)

saveRDS(base_all.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseStratmuBinded.rds"))

### Increased Scenario ####
# start.time <- Sys.time()
inc_stratmu <- sim_increase_tows |>
  group_by(rep) |> 
  nest() |> 
  mutate(precluded.data = map(data, ~filter(., AREA == "OUTSIDE")), 
         incl.stratmu = map(data, ~stratified.mean(.)), 
         precl.stratmu = map(precluded.data, ~stratified.mean(.)))
# end.time <- Sys.time()
# end.time - start.time

saveRDS(inc_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseStratmu.rds"))

inc_incl.stratmu <- inc_stratmu |> 
  dplyr::select(rep, incl.stratmu) |> 
  unnest(incl.stratmu) |> 
  mutate(TYPE = "With Wind Included") 

inc_precl.stratmu <- inc_stratmu |> 
  dplyr::select(rep, precl.stratmu) |> 
  unnest(precl.stratmu) |> 
  mutate(TYPE = "With Wind Precluded")

inc_all.stratmu <- bind_rows(inc_incl.stratmu, inc_precl.stratmu)

saveRDS(inc_all.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseStratmuBinded.rds"))

### Decreased Scenario ####
# start.time <- Sys.time()
dec_stratmu <- sim_decrease_tows |>
  group_by(rep) |> 
  nest() |> 
  mutate(precluded.data = map(data, ~filter(., AREA == "OUTSIDE")), 
         incl.stratmu = map(data, ~stratified.mean(.)), 
         precl.stratmu = map(precluded.data, ~stratified.mean(.)))
# end.time <- Sys.time()
# end.time - start.time

saveRDS(dec_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseStratmu.rds"))

dec_incl.stratmu <- dec_stratmu |> 
  dplyr::select(rep, incl.stratmu) |> 
  unnest(incl.stratmu) |> 
  mutate(TYPE = "With Wind Included") 

dec_precl.stratmu <- dec_stratmu |> 
  dplyr::select(rep, precl.stratmu) |> 
  unnest(precl.stratmu) |> 
  mutate(TYPE = "With Wind Precluded")

dec_all.stratmu <- bind_rows(dec_incl.stratmu, dec_precl.stratmu)

saveRDS(dec_all.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseStratmuBinded.rds"))


