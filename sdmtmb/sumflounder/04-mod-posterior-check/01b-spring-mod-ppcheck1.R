### created: 12/10/2022
### last updated: 08/14/2023

# 01b - SPRING POSTERIOR PREDICTIVE CHECK: ####

###################
#### OBJECTIVE ####
###################
# simulate new data with fitted model and bind to summer flounder survey data 
####################


#### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)

#### LOAD DATA ####
# read in fall data for model fitting
# sf_fall21 <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds")) |> 
#   filter(EST_YEAR == 2021)
# # read in spring data for model fitting
# sf_spring21 <- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds")) |> 
#   filter(EST_YEAR == 2021)

# best fit model created in `02a-fit-models.R` here("sdmtmb")
# fall_mod <- readRDS(here(sdmtmb.dir, "mar-536-project", "data", "fall_mod.rds"))

spring_mod <- readRDS(here(sdmtmb.dir, "mar-536-project", "data", "spring_mod.rds"))


# extract fitted data 
fall_moddat <- fall_mod$data

# spring_moddat <- spring_mod$data



#### SIMULATE DATASETS ####
set.seed(154326)

# fall_surv_extrayr <- sf_fall21[sample(nrow(sf_fall21), size = 6),] |> 
#   mutate(EST_YEAR = c(2020, 2022:2026))

# sim_fall <- simulate(fall_mod, nsim = 1000)
# colnames(sim_fall) <- paste0("sim", seq(1:1000))
# saveRDS(sim_fall, here("sdmtmb", "data", "fall-sims.rds"))

sim_spring <- simulate(spring_mod, nsim = 1000)
colnames(sim_spring) <- paste0("sim", seq(1:1000))
saveRDS(sim_spring, here("sdmtmb", "data", "spring-sims.rds"))

#sim1_m1 <- simulate(m1, nsim = 2)
# dim(s)
# s[1:3,1:4]


#### BIND DATASETS FOR CALCULATIONS ####
# bind summer flounder data with simulated data from m6 model
# fall_simdat <- fall_moddat |> 
#   select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) |>
#   bind_cols(sim_fall) 

# fall_simdat |> 
#   pivot_longer(cols = c(sim1:sim1000), 
#                names_to = "nsim", 
#                values_to = "EXPCATCHWT")# |> 
  # group_by(nsim) |> 
  # ggplot() + 
  # aes(x = EST_YEAR, y = EXPCATCHWT, group = nsim) + 
  # geom_line(alpha = 0.3, col = gray(0.5)) + 
  # theme_minimal()


# save the data
# saveRDS(fall_simdat, here("sdmtmb", "data", "fall_simdat.rds"))

# bind summer flounder data with simulated data from m6 model
spr_simdat <- spring_moddat |> 
  select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) |>
  bind_cols(sim_spring) 

# save the data
saveRDS(spr_simdat, here("sdmtmb", "data", "spr_simdat.rds"))


### STRATIFIED MEAN ###
strata_wts <- readRDS(here("data", "rds", "strata_wts.rds"))

# calculate total survey area for use in future calculations  
BTSArea <- as.integer(sum(strata$Area_SqNm))

source(here("R", "StratMeanFXs_v2.R")) #removed SEASON group for the purposes of the resampling

#### CALCULATE WITH WIND INCLUDED ####
# FallSimMu_ww <- fall_simdat |> 
#   pivot_longer(cols = c(sim1:sim1000), 
#                names_to = "nsim", 
#                values_to = "EXPCATCHWT") |> 
#   group_by(nsim) |> 
#   nest() |>
#   mutate(stratmu = map(data, ~strata.mean(.)))
# saveRDS(FallSimMu_ww, here("sdmtmb", "data", "fall_sim-ww-means.rds"))

SprSimMu_ww <- spr_simdat |> 
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") |> 
  group_by(nsim) |> 
  nest() |>
  mutate(stratmu = map(data, ~strata.mean(.)))
saveRDS(SprSimMu_ww, here("sdmtmb", "data", "spr_sim-ww-means.rds"))

#### CALCULATE WITHOUT WIND (WIND PRECLUDED) ####
# FallSimMu_wow <- fall_simdat |> 
#   filter(AREA == "OUTSIDE") |>
#   pivot_longer(cols = c(sim1:sim1000), 
#                names_to = "nsim", 
#                values_to = "EXPCATCHWT") |> 
#   group_by(nsim) |> 
#   nest() |>
#   mutate(stratmu = map(data, ~strata.mean(.)))
# saveRDS(FallSimMu_wow, here("sdmtmb", "data", "sfall_sim-wow-means.rds"))

SprSimMu_wow <- spr_simdat |> 
  filter(AREA == "OUTSIDE") |>
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") |> 
  group_by(nsim) |> 
  nest() |>
  mutate(stratmu = map(data, ~strata.mean(.)))
saveRDS(SprSimMu_wow, here("sdmtmb", "data", "spr_sim-wow-means.rds"))



# fall_simdat <- fall_simdat |>
#   dplyr::select(-EXPCATCHWT) |>
#   rename(EXPCATCHWT = sim1)
# 
# test <- strata.mean(fall_simdat) |>
#   mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
#          TYPE = "Wind Included Simulated")
# 
# stratmeans <- readRDS(here(sseep.dir, "data", "rds", "strat-mu_all.rds")) |> 
#   filter(SVSPP == 103) |>
#     group_by(EST_YEAR, TYPE, SEASON) |>
#     mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#            lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#            upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#     mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#            lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#            upper = ifelse(is.nan(upper), 0, upper)) # if the upper quantile is NaN, replace with 0
# 
# bind_test <- stratmeans |>
#   ungroup() |>
#   filter(SEASON == "FALL", TYPE == "With Wind Included") |>
#   dplyr::select(EST_YEAR, SVSPP, stratmu, stratvar, sdlog, lower, upper, TYPE) |>
#   bind_rows(test) 
# 
# ggplot(bind_test) +
#   aes(x = EST_YEAR, y = stratmu, color = TYPE) +
#   geom_pointrange(aes(ymin=lower, ymax = upper), position =  position_dodge2(width=0.4))
#   facet_wrap(vars(year), scales = "free_y") 
#   