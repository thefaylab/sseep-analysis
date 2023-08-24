
sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"
source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
# OBJECTIVE ####

## LOAD DATA ####
set.seed(123)
fall_mod <- readRDS(here(sdmtmb.dir, "mar-536-project", "data", "fall_mod.rds"))
#spring_mod <- readRDS(here(sdmtmb.dir, "mar-536-project", "data", "spring_mod.rds"))
stratmeans <- readRDS(here(sseep.dir, "data", "rds", "strat-mu_all.rds")) |>
  filter(SVSPP == 103, SEASON == "FALL") |> 
  group_by(EST_YEAR, TYPE, SEASON) %>%
  mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) %>% # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper), 
         METHOD = "Observed") |>
  select(-c(COMNAME, SCINAME))

# read in fall data from model fitting
sf_fall <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds"))

sf_fall21 <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds")) |> 
  filter(EST_YEAR == 2021)


## DATA WRANGLE ####
fall_extra_years <- c(2020, 2022:2026)

forecast_data <- map_dfr(seq_along(fall_extra_years), ~sf_fall21 |> mutate(rep = .x)) 

resample_data <- forecast_data |>
  group_by(rep) |>
  nest() |> 
  mutate(resamp = map(data, ~forecast_data[sample(nrow(sf_fall21), size = 125, replace = TRUE),]))|>
  select(rep, resamp) |>
  mutate(rep = case_when(
    rep ==1 ~ 2020, 
    rep > 1 ~ rep+2020)) |> 
  rename(rep_yr = rep) |>
  unnest(cols = c(resamp)) |> 
  select(-c(EST_YEAR, rep)) |>
  rename(EST_YEAR = rep_yr)
  
sf_fall_dat <- bind_rows(sf_fall, resample_data)

fall_mesh <- make_mesh(sf_fall_dat, xy_cols = c("X", "Y"), cutoff = 10)

fall_info <- sf_fall_dat |>
  dplyr::select(STATION, STRATUM, CRUISE6, AREA)

fall_omegas <- fall_preds$omega_s


# SIMULATE FALL ####
## base ####
x <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = sf_fall_dat,
  mesh = fall_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.66, -41.23, -25.36, 0.09), # coefficient estimates 
  range = 73.45, 
  rho = 0.158, # AR1 correlation
  sigma_O = 1.31,
  sigma_E = 0.971,
  phi = 1.72, # dispersion parameter
  tweedie_p = 1.26, # tweedie power parameter 
  fixed_re = list(0.353, epsilon_st = NULL, zeta_s = NULL),
  seed = 42,
  extra_time = fall_extra_years)

simdat <- bind_cols(x, fall_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103)
saveRDS(simdat, here("sdmtmb", "data", "simdat_fall.rds"))

## 5x ####
x5 <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = sf_fall_dat,
  mesh = fall_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.66, -41.23, -25.36, 0.45), # coefficient estimates 
  range = 73.45, 
  rho = 0.158, # AR1 correlation
  sigma_O = 1.31,
  sigma_E = 0.971,
  phi = 1.72, # dispersion parameter
  tweedie_p = 1.26, # tweedie power parameter 
  seed = 42,
  extra_time = fall_extra_years)


fall_simdat5x <- bind_cols(x5, fall_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103)
saveRDS(fall_simdat5x, here("sdmtmb", "data", "simdat_fall_5x.rds"))


## 10x####
x10 <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = sf_fall_dat,
  mesh = fall_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.66, -41.23, -25.36, 0.9), # coefficient estimates 
  range = 73.45, 
  rho = 0.158, # AR1 correlation
  sigma_O = 1.31,
  sigma_E = 0.971,
  phi = 1.72, # dispersion parameter
  tweedie_p = 1.26, # tweedie power parameter 
  seed = 42,
  extra_time = fall_extra_years)


fall_simdat_10x <- bind_cols(x10, fall_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103)
saveRDS(fall_simdat_10x, here("sdmtmb", "data", "simdat_fall_10x.rds"))


# STRATIFIED MEAN ####
## base ####
FallMu_ww <- strata.mean(simdat) |>
  mutate(SEASON = "FALL",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Included", 
         METHOD =  "Simulated", 
         SCENARIO = "Base")

FallMu_wow <- strata.mean(simdat |> filter(AREA == "OUTSIDE")) |>
  mutate(SEASON = "FALL",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Precluded", 
         METHOD =  "Simulated", 
         SCENARIO = "Base")


fall_sim_mu <- bind_rows(FallMu_wow, FallMu_ww)
saveRDS(fall_sim_mu, here("sdmtmb", "data", "fall_sim_mu.rds"))

ggplot() + 
  #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
  geom_pointrange(data = fall_sim_mu, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(SEASON), scales = "free_y") +
  #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
  labs(title = "Base", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
  ylim(0,NA) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = -1),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

## 5x ####
FallMu_ww_5x <- strata.mean(fall_simdat5x) |>
  mutate(SEASON = "FALL",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Included", 
         METHOD =  "Simulated", 
         SCENARIO = "5x")

FallMu_wow_5x <- strata.mean(fall_simdat5x |> filter(AREA == "OUTSIDE")) |>
  mutate(SEASON = "FALL",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Precluded", 
         METHOD =  "Simulated", 
         SCENARIO = "5x")


fall_sim_mu5x <- bind_rows(FallMu_wow_5x, FallMu_ww_5x)
saveRDS(fall_sim_mu5x, here("sdmtmb", "data", "fall_sim_mu5x.rds"))

ggplot() + 
  #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
  geom_pointrange(data = fall_sim_mu5x, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(SEASON), scales = "free_y") +
  #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
  labs(title = "5x", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
  ylim(0,NA) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = -1),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

## 10x ####
FallMu_ww_10x <- strata.mean(fall_simdat_10x) |>
  mutate(SEASON = "FALL",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Included", 
         METHOD =  "Simulated", 
         SCENARIO = "10x")

FallMu_wow_10x <- strata.mean(fall_simdat_10x |> filter(AREA == "OUTSIDE")) |>
  mutate(SEASON = "FALL",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Precluded", 
         METHOD =  "Simulated", 
         SCENARIO = "10x")


fall_sim_mu10x <- bind_rows(FallMu_wow_10x, FallMu_ww_10x)
saveRDS(fall_sim_mu10x, here("sdmtmb", "data", "fall_sim_mu10x.rds"))

ggplot() + 
  #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
  geom_pointrange(data = fall_sim_mu_10x, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(SEASON), scales = "free_y") +
  #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
  labs(title = "10x", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
  ylim(0,NA) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = -1),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

fall_sim_scenarios <-  bind_rows(fall_sim_mu, fall_sim_mu5x, fall_sim_mu10x)
saveRDS(fall_sim_scenarios, here("sdmtmb", "data", "fall_sim_scenarios.rds"))
