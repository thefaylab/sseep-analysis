### created: 07/24/2023
### last updated: 08/23/2023

# 01a -  PREPARE FALL DATA FOR SIMULATIONS ####


## OBJECTIVE ####
# create a dataframe with tow locations for future 5 years to be used for simulations
# create a mesh of these future year locations for simulations


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) #GF: sdmTMB dependency in this script `make_mesh()`

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"
#source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
set.seed(123)


## LOAD DATA ####
# best fit model 15a, and refit without REML for predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
# fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

# fall_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "fall_predictions.rds"))

#spring_mod <- readRDS(here(sdmtmb.dir, "mar-536-project", "data", "spring_mod.rds"))
# stratmeans <- readRDS(here(sseep.dir, "data", "rds", "strat-mu_all.rds")) |>
#   filter(SVSPP == 103, SEASON == "FALL") |> 
#   group_by(EST_YEAR, TYPE, SEASON) %>%
#   mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) %>% # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper), 
#          METHOD = "Observed") |>
#   select(-c(COMNAME, SCINAME))

# read in fall data from model fitting
sf_fall <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds"))
# 
# sf_fall21 <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds")) |>
#   filter(EST_YEAR == 2021)

# sf_fall|> group_by(EST_YEAR) |> mutate(tow = paste(STRATUM,CRUISE6,STATION,sep="_")) |> summarise(tow = length(tow)) |> filter(EST_YEAR != 2017) |> summary()
# GF: note that this finds the number of rows in each data frame, not the number of unique tows, BUT there appears to be only 1 row per tow so OK.
# GF: if the variable 'tow' is supposed to be a unique id for each tow then `tow=paste(STRATUM,CRUISE6,STATION,sep="_")` would do this. Current code is creating an identical string for each element of tow.

## CREATE FUTURE YEAR DATASET ####
# extract model fitting data
# moddat <- fall_mod$data |>
#   filter(EST_YEAR %)

# extract the most recent year of survey data 
# last_yr_dat <- sf_fall |>
#   filter(EST_YEAR == 2021)

# set the future years that will be simulated 
future_years <- c(2022:2026)

# replicate the most recent year of data for the number of future years to be simulated and add a "replicate" column
# replicate_data <- map_dfr(seq_along(future_years), ~sf_fall21 |> mutate(rep = .x)) 
# creates a dataset with nrows for each of the future years.

# resample the data to create a new dataset for each replicate 
# future_data <- replicate_data |>
#   group_by(rep) |>
#   nest() |> 
#   mutate(resamp = map(data, ~replicate_data[sample(nrow(sf_fall21), size = 125, replace = TRUE),]))|> # size = the number of tows that occurred in the last year of the time series  
#   select(rep, resamp) |>
#   mutate(rep = rep+2021) |> #case_when(
#     #rep ==1 ~ 2020, 
#     #rep > 1 ~ rep+2020)) |> 
#   rename(rep_yr = rep) |>
#   unnest(cols = c(resamp)) |> 
#   select(-c(EST_YEAR, rep)) |>
#   rename(EST_YEAR = rep_yr)
# this code is not consistent with the explanation for the choice of tow locations in future years
# which was that you fixed the locations at the most recent observed year
# you are sampling (with replacement) stations from the full time series
# this results in having multiple tows at the same station in a given future year
# more importantly, it destroys the random stratified sampling assumptions
# suggest sampling with replacement from historical years for the future years sampled stations (i.e. future years are one of the historical years)

# randomly select 5 years from the historical time period, 2020 is missing for the original observations and should not be included as an option
resampled_years <- sample(c(2009:2019,2021), size = 5, replace = TRUE)

saveRDS(resampled_years, here("sdmtmb", "sumflounder", "data", "simulations", "resampled_years.rds"))

# use locations from the resampled years as the observations in future years
future_data <- sf_fall |> 
  filter(EST_YEAR %in% resampled_years) |>
  group_by(EST_YEAR) |> 
  mutate(EST_YEAR = case_when( # replace the historical year with the future year, based on its resampled value
    EST_YEAR == resampled_years[1] ~ 2022, 
    EST_YEAR == resampled_years[2] ~ 2023,
    EST_YEAR == resampled_years[3] ~ 2024,
    EST_YEAR == resampled_years[4] ~ 2025,
    EST_YEAR == resampled_years[5] ~ 2026,
  ))

# save the data
saveRDS(future_data, here("sdmtmb", "sumflounder", "data", "simulations", "fall_future_data.rds"))
  
#sf_fall_dat <- bind_rows(sf_fall, resample_data)
## MAKE MESH ####
future_mesh <- make_mesh(future_data, xy_cols = c("X", "Y"), cutoff = 10)
plot(future_mesh)

saveRDS(future_mesh, here("sdmtmb", "sumflounder", "data", "simulations", "fall_future_mesh.rds"))


#####
# fall_info <- future_data |>
#   ungroup() |>
#   dplyr::select(STATION, STRATUM, CRUISE6, AREA)

#fall_omegas <- mean(fall_preds$omega_s)
#fixed_re <-  list(, epsilon_st = NULL, zeta_s = NULL)


# # SIMULATE FALL ###
# ## base ###
# x <- sdmTMB_simulate(
#   formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
#   data = sf_fall_dat,
#   mesh = fall_mesh,
#   family = tweedie(link = "log"),
#   time = "EST_YEAR",
#   B = c(-0.66, -41.23, -25.36, 0.09), # coefficient estimates 
#   range = 73.45, 
#   rho = 0.158, # AR1 correlation
#   sigma_O = 1.31,
#   sigma_E = 0.971,
#   phi = 1.72, # dispersion parameter
#   tweedie_p = 1.26, # tweedie power parameter 
#   fixed_re = fixed_re,
#   seed = 42,
#   extra_time = fall_extra_years)
# 
# fall_simdat <- bind_cols(x, fall_info) |> 
#   rename(EXPCATCHWT = observed) |>
#   mutate(SVSPP = 103)
# saveRDS(fall_simdat, here("sdmtmb", "data", "simdat_fall.rds"))
# 
# ## INCREASE ###
# inc_x <- sdmTMB_simulate(
#   formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
#   data = sf_fall_dat,
#   mesh = fall_mesh,
#   family = tweedie(link = "log"),
#   time = "EST_YEAR",
#   B = c(-0.66, -41.23, -25.36, log(4)), # coefficient estimates; adjust wind by expected increase - quadruple biomass
#   range = 73.45, 
#   rho = 0.158, # AR1 correlation
#   sigma_O = 1.31,
#   sigma_E = 0.971,
#   phi = 1.72, # dispersion parameter; in log space
#   tweedie_p = 1.26, # tweedie power parameter 
#   fixed_re = fixed_re,
#   seed = 42,
#   extra_time = fall_extra_years)
# 
# 
# fall_simdat_inc <- bind_cols(inc_x, fall_info) |> 
#   rename(EXPCATCHWT = observed) |>
#   mutate(SVSPP = 103)
# saveRDS(fall_simdat_inc, here("sdmtmb", "data", "simdat_fall_inc.rds"))
# 
# 
# ## 10x###
# dec_x <- sdmTMB_simulate(
#   formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
#   data = sf_fall_dat,
#   mesh = fall_mesh,
#   family = tweedie(link = "log"),
#   time = "EST_YEAR",
#   B = c(-0.66, -41.23, -25.36, log(0.0545)), # coefficient estimates; adjust wind by expected reduction - 5% biomass
#   range = 73.45, 
#   rho = 0.158, # AR1 correlation
#   sigma_O = 1.31,
#   sigma_E = 0.971,
#   phi = 1.72, # dispersion parameter
#   tweedie_p = 1.26, # tweedie power parameter 
#   fixed_re = fixed_re,
#   seed = 42,
#   extra_time = fall_extra_years)
# 
# 
# fall_simdat_dec <- bind_cols(dec_x, fall_info) |> 
#   rename(EXPCATCHWT = observed) |>
#   mutate(SVSPP = 103)
# saveRDS(fall_simdat_dec, here("sdmtmb", "data", "simdat_fall_dec.rds"))
# 
# 
# # STRATIFIED MEAN ###
# ## base ###
# FallMu_ww <- strata.mean(fall_simdat) |>
#   mutate(SEASON = "FALL",
#          sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
#          TYPE = "With Wind Included", 
#          METHOD =  "Simulated", 
#          SCENARIO = "Baseline")
# 
# FallMu_wow <- strata.mean(fall_simdat |> filter(AREA == "OUTSIDE")) |>
#   mutate(SEASON = "FALL",
#          sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
#          TYPE = "With Wind Precluded", 
#          METHOD =  "Simulated", 
#          SCENARIO = "Baseline")
# 
# 
# fall_sim_mu <- bind_rows(FallMu_wow, FallMu_ww)
# saveRDS(fall_sim_mu, here("sdmtmb", "data", "fall_sim_mu.rds"))
# 
# ggplot() + 
#   #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
#   geom_pointrange(data = fall_sim_mu, aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
#   facet_wrap(vars(SEASON), scales = "free_y") +
#   #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
#   labs(title = "Baseline", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
#   ylim(0,NA) +
#   theme_bw() +
#   theme(legend.position="bottom",
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 90, hjust = -1),
#         axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
#         axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))
# 
# ## INCREASE ###
# FallMu_ww_inc <- strata.mean(fall_simdat_inc) |>
#   mutate(SEASON = "FALL",
#          sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
#          TYPE = "With Wind Included", 
#          METHOD =  "Simulated", 
#          SCENARIO = "Surplus")
# 
# FallMu_wow_inc <- strata.mean(fall_simdat_inc |> filter(AREA == "OUTSIDE")) |>
#   mutate(SEASON = "FALL",
#          sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
#          TYPE = "With Wind Precluded", 
#          METHOD =  "Simulated", 
#          SCENARIO = "Surplus")
# 
# 
# fall_simmu_inc <- bind_rows(FallMu_wow_inc, FallMu_ww_inc)
# saveRDS(fall_simmu_inc, here("sdmtmb", "data", "fall_simmu_inc.rds"))
# 
# ggplot() + 
#   #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
#   geom_pointrange(data = fall_simmu_inc, aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
#   facet_wrap(vars(SEASON), scales = "free_y") +
#   #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
#   labs(title = "Surplus", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
#   ylim(0,NA) +
#   theme_bw() +
#   theme(legend.position="bottom",
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 90, hjust = -1),
#         axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
#         axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))
# 
# ## DECREASE ###
# FallMu_ww_dec <- strata.mean(fall_simdat_dec) |>
#   mutate(SEASON = "FALL",
#          sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
#          TYPE = "With Wind Included", 
#          METHOD =  "Simulated", 
#          SCENARIO = "Reduction")
# 
# FallMu_wow_dec <- strata.mean(fall_simdat_dec |> filter(AREA == "OUTSIDE")) |>
#   mutate(SEASON = "FALL",
#          sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
#          lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
#          upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
#   mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
#          lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
#          upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
#          TYPE = "With Wind Precluded", 
#          METHOD =  "Simulated", 
#          SCENARIO = "Reduction")
# 
# 
# fall_simmu_dec <- bind_rows(FallMu_wow_dec, FallMu_ww_dec)
# saveRDS(fall_simmu_dec, here("sdmtmb", "data", "fall_simmu_dec.rds"))
# 
# ggplot() + 
#   #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
#   geom_pointrange(data = fall_simmu_dec, aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
#   facet_wrap(vars(SEASON), scales = "free_y") +
#   #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
#   labs(title = "Reduction", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
#   ylim(0,NA) +
#   theme_bw() +
#   theme(legend.position="bottom",
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 90, hjust = -1),
#         axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
#         axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))
# 
# fall_sim_scenarios <-  bind_rows(fall_sim_mu, fall_simmu_inc, fall_simmu_dec)
# saveRDS(fall_sim_scenarios, here("sdmtmb", "data", "fall_sim_scenarios.rds"))
