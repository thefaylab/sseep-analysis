### created: 08/23/2023
### last updated: 

# 02a -  Calculate Stratified Means for Fall Scenarios ####


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
sim_base_tows <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseTows.rds"))
# saveRDS(sim_base_grid, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseGrid.rds"))

# increased simulation data 
sim_increase_tows <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseTows.rds"))
# saveRDS(sim_increase_grid, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseGrid.rds"))

# reduced simulation data 
sim_decrease_tows <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseTows.rds"))
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

saveRDS(base_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseStratmu.rds"))

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

saveRDS(inc_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseStratmu.rds"))

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

saveRDS(dec_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseStratmu.rds"))


