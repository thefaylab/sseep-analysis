fall.increase_sims <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "fall_increase_sim.rds"))


## CALCULATE STRATIFIED MEAN ####
fall.increase_incl.stratmu <- fall.increase_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(fall.increase_precl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimIncrease_incl-stratmu.rds"))

fall.increase_precl.stratmu <- fall.increase_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(increase_precl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimInc_precl-stratmu.rds"))

fall.decrease_sims <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "fall_decrease_sim.rds"))

fall.decrease_incl.stratmu <- fall.decrease_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(decrease_incl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimDec_incl-stratmu.rds"))

fall.decrease_stratmu <- fall.decrease_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(decrease_stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimDec_precl-stratmu.rds"))

spr.increase_sims <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "spring_increase_sim.rds"))


## CALCULATE STRATIFIED MEAN ####
# With Wind Included
spr.increase_incl.stratmu <- spr.increase_sims |> 
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(spr.increase_incl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimIncrease_incl-stratmu.rds"))

# With Wind Precluded
spr.increase_precl.stratmu <- spr.increase_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(spr.increase_precl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimIncrease_precl-stratmu.rds"))

spr.decrease_sims <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "spring_decrease_sim.rds"))


## CALCULATE STRATIFIED MEAN ####
# With Wind Included
spr.decrease_incl.stratmu <- spr.decrease_sims |> 
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(spr.decrease_incl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimDec_incl-stratmu.rds"))

# With Wind Precluded
spr.decrease_precl.stratmu <- spr.decrease_sims |> 
  filter(AREA == "OUTSIDE") |>
  group_by(rep) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(.)))  

#saveRDS(spr.decrease_precl.stratmu, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimDec_precl-stratmu.rds"))
