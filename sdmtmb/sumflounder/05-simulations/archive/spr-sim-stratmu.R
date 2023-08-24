
# OBJECTIVE ####

## LOAD DATA ####
set.seed(123)
spring_mod <- readRDS(here(sdmtmb.dir, "mar-536-project", "data", "spring_mod.rds"))

# read in fall data for model fitting
sf_spring<- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds"))

spring_preds <- readRDS(file = here(sdmtmb.dir, "data", "spring_projects.rds"))

stratmeans <- readRDS(here(sseep.dir, "data", "rds", "strat-mu_all.rds")) |>
  filter(SVSPP == 103, SEASON == "SPRING") |> 
  group_by(EST_YEAR, TYPE, SEASON) |>
  mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper), 
         METHOD = "Observed") |> 
  select(-c(COMNAME, SCINAME)) 

## DATA WRANGLE ####
spring_extra_years <- c(2022:2026)

sf_spr21 <- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds")) |> 
  filter(EST_YEAR == 2021)

forecast_data <- map_dfr(seq_along(spring_extra_years), ~sf_spr21 |> mutate(rep = .x)) 

resample_data <- forecast_data |>
  group_by(rep) |>
  nest() |> 
  mutate(resamp = map(data, ~forecast_data[sample(nrow(sf_spr21), size = 125, replace = TRUE),]))|>
  select(rep, resamp) |>
  mutate(rep = rep+2021) |> 
  rename(rep_yr = rep) |>
  unnest(cols = c(resamp)) |> 
  select(-c(EST_YEAR, rep)) |>
  rename(EST_YEAR = rep_yr)

sf_spr_dat <- bind_rows(sf_spring, resample_data)

spr_info <- sf_spr_dat |>
  dplyr::select(STATION, STRATUM, CRUISE6, AREA)

spr_mesh <- make_mesh(sf_spr_dat, xy_cols = c("X", "Y"), cutoff = 10)

spr_omegas <- mean(spring_preds$omega_s)
fixed_re <-  list(spr_omegas, epsilon_st = NULL, zeta_s = NULL)


# SIMULATE SPRIMG ####
# base ####
y <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = sf_spr_dat,
  mesh = spr_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.06, 7.66, -27.67, 0.34), # coefficient estimates 
  range = 96.2, 
  sigma_O = 1.15,
  sigma_E = 0.844,
  phi = 1.83, # dispersion parameter
  tweedie_p = 1.39, # tweedie power parameter 
  fixed_re = fixed_re,
  seed = 42,
  extra_time = spring_extra_years)

spr_simdat <- bind_cols(y, spr_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103)
saveRDS(simdat, here("sdmtmb", "data", "simdat_spr.rds"))

## INCREASE ####
inc_y <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = sf_spr_dat,
  mesh = spr_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.06, 7.66, -27.67, log(6)), # coefficient estimates; adjust wind by expected increase - quadruple biomass 
  range = 96.2, 
  sigma_O = 1.15,
  sigma_E = 0.844,
  phi = 1.83, # dispersion parameter
  tweedie_p = 1.39, # tweedie power parameter 
  fixed_re = fixed_re,
  seed = 42,
  extra_time = spring_extra_years)

spr_simdat_inc <- bind_cols(inc_y, spr_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103)
saveRDS(spr_simdat_inc, here("sdmtmb", "data", "simdat_spr_inc.rds"))

## DECREASE ####
dec_y <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = sf_spr_dat,
  mesh = spr_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.06, 7.66, -27.67, log(0.052)), # coefficient estimates; adjust wind by expected reduction - 5% biomass
  range = 96.2, 
  sigma_O = 1.15,
  sigma_E = 0.844,
  phi = 1.83, # dispersion parameter
  tweedie_p = 1.39, # tweedie power parameter 
  fixed_re = fixed_re,
  seed = 42,
  extra_time = spring_extra_years)

spr_simdat_dec <- bind_cols(dec_y, spr_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103)
saveRDS(spr_simdat_dec, here("sdmtmb", "data", "simdat_spr_dec.rds"))



# STRATIFIED MEAN ####
## Base ####
SprMu_ww <- strata.mean(spr_simdat) |>
  mutate(SEASON = "SPRING",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Included", 
         METHOD =  "Simulated", 
         SCENARIO = "Baseline")

SprMu_wow <- strata.mean(spr_simdat |> filter(AREA == "OUTSIDE")) |>
  mutate(SEASON = "SPRING",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Precluded", 
         METHOD =  "Simulated", 
         SCENARIO = "Baseline")

spr_sim_mu <- bind_rows(SprMu_wow, SprMu_ww)
saveRDS(spr_sim_mu, here("sdmtmb", "data", "spr_sim_mu.rds"))

ggplot() + 
  #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
  geom_pointrange(data = spr_sim_mu, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
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

## INCREASE ####
SprMu_ww_inc <- strata.mean(spr_simdat_inc) |>
  mutate(SEASON = "SPRING",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Included", 
         METHOD =  "Simulated", 
         SCENARIO = "Surplus")

SprMu_wow_inc <- strata.mean(spr_simdat_inc |> filter(AREA == "OUTSIDE")) |>
  mutate(SEASON = "SPRING",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Precluded", 
         METHOD =  "Simulated", 
         SCENARIO = "Surplus")

spr_simmu_inc <- bind_rows(SprMu_wow_inc, SprMu_ww_inc)
saveRDS(spr_simmu_inc, here("sdmtmb", "data", "spr_simmu_inc.rds"))

ggplot() + 
  #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
  geom_pointrange(data = spr_simmu_inc, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(SEASON), scales = "free_y") +
  #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
  labs(title = "5xe", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
  ylim(0,NA) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = -1),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

## DECREASE ####
SprMu_ww_dec <- strata.mean(spr_simdat_dec) |>
  mutate(SEASON = "SPRING",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Included", 
         METHOD =  "Simulated", 
         SCENARIO = "Reduction")

SprMu_wow_dec <- strata.mean(spr_simdat_dec |> filter(AREA == "OUTSIDE")) |>
  mutate(SEASON = "SPRING",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Precluded", 
         METHOD =  "Simulated", 
         SCENARIO = "Reduction")

spr_simmu_dec <- bind_rows(SprMu_wow_dec, SprMu_ww_dec)
saveRDS(spr_simmu_dec, here("sdmtmb", "data", "spr_simmu_dec.rds"))

ggplot() + 
  #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
  geom_pointrange(data = spr_simmu_dec, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
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


spr_sim_scenarios <-  bind_rows(spr_sim_mu, spr_simmu_inc, spr_simmu_dec)
saveRDS(spr_sim_scenarios, here("sdmtmb", "data", "spr_sim_scenarios.rds"))
