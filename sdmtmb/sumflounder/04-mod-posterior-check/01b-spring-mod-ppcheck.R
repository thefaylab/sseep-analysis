### created: 12/10/2022
### last updated: 12/28/2023

# 01b - SPRING POSTERIOR PREDICTIVE CHECK: SIMULATE CHANGES IN ABUNDANCE INDICES OVER TIME ####

## OBJECTIVE ####
# simulate new data with fitted model 
# bind to summer flounder survey data 
# calculate abundance index and change in abundance index over time


### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
set.seed(154326)

post.check.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data/post-check"

### LOAD DATA ####
# best fit model , and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01b-spring-forecasts.R")
spring_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mod.rds"))


# extract fitted data 
spring_moddat <- spring_mod$data |> 
  mutate(EST_YEAR = YEAR, 
         AREA = as.character(AREA))


## SIMULATE DATASETS ####
sim_spring <- simulate(spring_mod, nsim = 1000)

# create unique column names for each simulated dataset
colnames(sim_spring) <- paste0("sim", seq(1:1000))


### BIND DATASETS FOR CALCULATIONS ####
# bind summer flounder data with simulated data from model
spr_simdat <- spring_moddat |> 
  select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) |>
  bind_cols(sim_spring) |>
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") 

### save the data
saveRDS(spr_simdat, here(post.check.dat, "spr_simdat.rds"))


## CALCULATE STRATIFIED MEANS ###
# calculate the stratied mean abundance index for each scenario's replicate and year 

# read in stratified mean functions
source(here("R", "StratMeanFXs_v2.R")) 

### WITH WIND INCLUDED ####
SprMu_ww <- spr_simdat |>
  mutate(TYPE = "With Wind Included") |> 
  group_by(nsim, SEASON, TYPE) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(.)))

### save the data
saveRDS(SprMu_ww, here(post.check.dat, "spr_sim-ww-means.rds"))

### WITHOUT WIND (WIND PRECLUDED) ####
SprMu_wow <- spr_simdat  |> 
  filter(AREA == "OUTSIDE") |>
  mutate(TYPE = "With Wind Precluded") |> 
  group_by(nsim, SEASON, TYPE) |> 
  nest() |>
  mutate(stratmu = map(data, ~stratified.mean(.)))

### save the data
saveRDS(SprMu_wow, here(post.check.dat, "spr_sim-wow-means.rds"))

## BIND STRATIFIED MEANS ####
spring_stratmu <- bind_rows(SprMu_ww, SprMu_wow)

### save the data
saveRDS(spring_stratmu, here(post.check.dat, "spring_sim-stratmu.rds"))

## FIT LINEAR REGRESSIONS ####
# estimate the simulated change in abundance index over time for each scenario

spring_lms <- spring_stratmu |>
  select(nsim, stratmu, SEASON, TYPE) |> 
  mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) |>  
  mutate(coef = map(model, broom::tidy, conf.int = TRUE)) |> 
  unnest(coef) |>
  select(nsim, term, estimate, conf.low, conf.high, SEASON, TYPE) %>%
  filter(term == "EST_YEAR") 

### save the data 
saveRDS(spring_lms, file = here(post.check.dat, "spring_slopes.rds"))

### WITH WIND  ####
# spr_ww_lms <- SprMu_ww |>
#   select(nsim, stratmu, SEASON, TYPE) |> 
#   mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) |>  
#   mutate(coef = map(model, broom::tidy, conf.int = TRUE)) |> 
#   unnest(coef) |>
#   select(nsim, term, estimate, conf.low, conf.high) |>
#   filter(term == "EST_YEAR") #|> 
#   # mutate(SEASON = "SPRING", 
#   #        TYPE = "With Wind Included")
# 
# ### save the data 
# saveRDS(spr_ww_lms, file = here("sdmtmb", "sumflounder", "data", "post-check", "spring_ww_slopes.rds"))
# 
# 
# ### WITHOUT WIND (WIND PRECLUDED) ####
# spr_wow_lms <- SprMu_wow |>
#   select(nsim, stratmu, SEASON, TYPE) |> 
#   mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) |>    mutate(coef = map(model, broom::tidy, conf.int = TRUE)) |> 
#   unnest(coef) |> 
#   select(nsim, term, estimate, conf.low, conf.high) |>
#   filter(term == "EST_YEAR") #|> 
#   # mutate(SEASON = "SPRING", 
#   #        TYPE = "With Wind Precluded")
# 
# ### save the data
# saveRDS(spr_wow_lms, file = here("sdmtmb", "sumflounder", "data", "post-check", "spring_wow_slopes.rds"))

