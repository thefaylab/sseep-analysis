# FALL 

set.seed(123)

fall_extra_years <- c(2020, 2022:2026)

# read in fall data for model fitting
sf_fall <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds"))

sf_fall21 <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds")) |> 
  filter(EST_YEAR == 2021)

fall_surv_extrayr <- sf_fall21[sample(nrow(sf_fall21), size = 6),] |>
  mutate(EST_YEAR = c(2020, 2022:2026))

sf_fall_dat <- bind_rows(sf_fall, fall_surv_extrayr)


mesh <- make_mesh(sf_fall_dat, xy_cols = c("X", "Y"), cutoff = 10)

x <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = sf_fall_dat,
  mesh = mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.66, -41.23, -25.36, 0.09),
  range = 73.45,
  rho = 0.158,
  sigma_O = 1.31,
  sigma_E = 0.971,
  phi = 1.72,
  tweedie_p = 1.26,
  seed = 42,
  extra_time = fall_extra_years)


info <- sf_fall_dat |>
  dplyr::select(STATION, STRATUM, CRUISE6, AREA)

simdat <- bind_cols(x, info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103)
saveRDS(simdat, here("sdmtmb", "data", "simdat_fall.rds"))

FallMu_ww <- strata.mean(simdat) |>
  mutate(SEASON = "FALL",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Included", 
         METHOD =  "Simulated")

FallMu_wow <- strata.mean(simdat |> filter(AREA == "OUTSIDE")) |>
  mutate(SEASON = "FALL",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Precluded", 
         METHOD =  "Simulated")

stratmeans <- stratmeans |> mutate(METHOD = "Observed") |> select(-c(COMNAME, SCINAME)) |>
  filter(SEASON == "FALL")

fall_sim_mu <- bind_rows(fall_stratmeans, FallMu_wow, FallMu_ww)
saveRDS(fall_sim_mu, here("sdmtmb", "data", "fall_sim_mu.rds"))

ggplot() + 
  #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
  geom_pointrange(data = fall_sim_mu, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(SEASON), scales = "free_y") +
  #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
  labs(title = "A) FALL", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
  ylim(0,NA) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = -1),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))



plot_map(east_coast, simdat, column = EXPCATCHWT)

ggplot() +
  geom_point(data = simdat, mapping = aes(X*1000, Y*1000, color = EXPCATCHWT)) +
  geom_sf(data = east_coast) +
  scale_color_viridis_c(trans = "sqrt", option = "H") +
  facet_wrap(~EST_YEAR) + 
  labs(x = "Longitude", y = "Latitude")


## SPRING

spring_extra_years <- c(2022:2026)

# read in fall data for model fitting
sf_spring<- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds"))

sf_spr21 <- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds")) |> 
  filter(EST_YEAR == 2021)

spr_surv_extrayr <- sf_spr21[sample(nrow(sf_spr21), size = 5),] |>
  mutate(EST_YEAR = c(2022:2026))

sf_spr_dat <- bind_rows(sf_spring, spr_surv_extrayr)


spr_mesh <- make_mesh(sf_spr_dat, xy_cols = c("X", "Y"), cutoff = 10)

y <- sdmTMB_simulate(
  formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
  data = sf_spr_dat,
  mesh = spr_mesh,
  family = tweedie(link = "log"),
  time = "EST_YEAR",
  B = c(-0.06, 7.66, -27.67, 0.34),
  range = 96.2,
  sigma_O = 1.15,
  sigma_E = 0.844,
  phi = 1.83,
  tweedie_p = 1.39,
  seed = 42,
  extra_time = spring_extra_years)


spr_info <- sf_spr_dat |>
  dplyr::select(STATION, STRATUM, CRUISE6, AREA)

simdat_spr <- bind_cols(y, spr_info) |> 
  rename(EXPCATCHWT = observed) |>
  mutate(SVSPP = 103)

saveRDS(simdat_spr, here("sdmtmb", "data", "simdat_spr.rds"))

SprMu_ww <- strata.mean(simdat_spr) |>
  mutate(SEASON = "SPRING",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Included", 
         METHOD =  "Simulated")

SprMu_wow <- strata.mean(simdat_spr |> filter(AREA == "OUTSIDE")) |>
  mutate(SEASON = "SPRING",
         sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper),  # if the upper quantile is NaN, replace with 0)
         TYPE = "With Wind Precluded", 
         METHOD =  "Simulated")

spr_stratmeans <- stratmeans |>
  filter(SEASON == "SPRING")

spr_sim_mu <- bind_rows(spr_stratmeans, SprMu_wow, SprMu_ww)
saveRDS(spr_sim_mu, here("sdmtmb", "data", "spr_sim_mu.rds"))

ggplot() + 
  #geom_pointrange(data = stratmeans |> , aes(x = as.factor(EST_YEAR), y = stratmu, color = TYPE, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + #geom_point() +
  geom_pointrange(data = spr_sim_mu, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(SEASON), scales = "free_y") +
  #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") +
  labs(title = "B) Spring", x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", TYPE = "") +
  ylim(0,NA) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = -1),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

saveRDS(FallSimMu_ww, here("sdmtmb", "data", "fall_sim-ww-means.rds"))

plot_map(east_coast, simdat_spr, column = EXPCATCHWT)

ggplot() +
  geom_point(data = simdat_spr, mapping = aes(X*1000, Y*1000, color = EXPCATCHWT)) +
  geom_sf(data = east_coast) +
  scale_color_viridis_c(trans = "sqrt", option = "H") +
  facet_wrap(~EST_YEAR) + 
  labs(x = "Longitude", y = "Latitude")
