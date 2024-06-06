### created: 04/08/2024
### last updated: 05/28/2024

# 02c - FIT SPRING MODELS: NO DEPTH OUTLIERS ####

## OBJECTIVE ####
# fit various forms of models using prepared Atlantic mackerel data for the spring season
# include predictor for index standardization 
# estimate a coefficient for each year 
# include inside/outside wind area designation as part of the predictor
# stock assessment only uses the spring survey 
# removes observations at depths greater than 200 meters
# runs approx 1.5 hours

### LOAD PACKAGES ####
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(kableExtra)

here()
atlmack.dat <- here("sdmtmb", "atlmackerel", "data")
tweedie.mods <- here("sdmtmb", "atlmackerel", "data", "mods", "tweedie", "no-dep-out")
dpg.mods <- here("sdmtmb", "atlmackerel", "data", "mods", "dpg", "no-dep-out")

### LOAD DATA ####
# Atlantic mackerel data created in `01-prepare-data.R` 
am_spring <- readRDS(here(atlmack.dat, "atlmackerel_no-200-obs.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# spring mesh created in `01-prepare-data.R` 
spring_mesh <- readRDS(here(atlmack.dat, "spring_mesh_no-200-obs.rds"))

## TWEEDIE MODEL FITS ####
#### M1 ####
m1_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                      data = am_spring,
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID",
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE)

sanity(m1_spring)

saveRDS(m1_spring, file = here(tweedie.mods, "m1_spring.rds"))

#### M2 ####
# second order polynomial model fit with random effects
m2_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                       data = am_spring,
                       mesh = spring_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE)

sanity(m2_spring)

saveRDS(m2_spring, file = here(tweedie.mods, "m2_spring.rds"))


#### M3 ####
# third order polynomial model fit with random effects
m3_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 3) + EST_YEAR - 1,
                       data = am_spring,
                       mesh = spring_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE)

sanity(m3_spring)

saveRDS(m3_spring, file = here(tweedie.mods, "m3_spring.rds"))


#### M4 ####
# forth order polynomial model fit with random effects
m4_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                       data = am_spring,
                       mesh = spring_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE)

sanity(m4_spring)

saveRDS(m4_spring, file = here(tweedie.mods, "m4_spring.rds"))

#### M5 ####
# forth order polynomial model fit with random effects and independent ranges
m5_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                    data = am_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    share_range = FALSE,
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE)

sanity(m5_spring)

saveRDS(m5_spring, file = here(tweedie.mods, "m5_spring.rds"))

## DELTA/POISSON/GAMMA MODEL FITS ####
#### M1 ####
m1_spring_dpg <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                          data = am_spring,
                          mesh = spring_mesh,
                          family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                          spatial = "on", 
                          time = "EST_YEAR",
                          spatiotemporal = "IID",
                          control = sdmTMBcontrol(newton_loops = 1), 
                          silent = FALSE)

sanity(m1_spring_dpg)

saveRDS(m1_spring_dpg, file = here(dpg.mods, "m1_spring.rds"))


#### M2 ####
# second order polynomial model fit with random effects
m2_spring_dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                           data = am_spring,
                           mesh = spring_mesh,
                           family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                           spatial = "on", 
                           time = "EST_YEAR",
                           spatiotemporal = "IID",
                           control = sdmTMBcontrol(newton_loops = 1), 
                           silent = FALSE)

sanity(m2_spring_dpg)

saveRDS(m2_spring_dpg, file = here(dpg.mods, "m2_spring.rds"))


#### M3 ####
# third order polynomial model fit with random effects
m3_spring_dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 3) + EST_YEAR - 1,
                           data = am_spring,
                           mesh = spring_mesh,
                           family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                           spatial = "on", 
                           time = "EST_YEAR",
                           spatiotemporal = "IID",
                           control = sdmTMBcontrol(newton_loops = 1), 
                           silent = FALSE)

sanity(m3_spring_dpg)

saveRDS(m3_spring_dpg, file = here(dpg.mods, "m3_spring.rds"))


#### M4 ####
# forth order polynomial model fit with random effects
m4_spring_dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                           data = am_spring,
                           mesh = spring_mesh,
                           family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                           spatial = "on", 
                           time = "EST_YEAR",
                           spatiotemporal = "IID",
                           control = sdmTMBcontrol(newton_loops = 1), 
                           silent = FALSE)

sanity(m4_spring_dpg)

saveRDS(m4_spring_dpg, file = here(dpg.mods, "m4_spring.rds"))

#### M5 ####
# forth order polynomial model fit with random effects and independent ranges
m5_spring_dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                        data = am_spring,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        share_range = list(FALSE, FALSE),
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE)

sanity(m5_spring_dpg)

saveRDS(m5_spring_dpg, file = here(dpg.mods, "m5_spring.rds"))

#### M6 ####
# forth order polynomial model fit with random effects and independent ranges in model 2
m6_spring_dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                        data = am_spring,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        share_range = list(TRUE, FALSE),
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE)

sanity(m6_spring_dpg)

saveRDS(m6_spring_dpg, file = here(dpg.mods, "m6_spring.rds"))

#### M7 ####
# forth order polynomial model fit with random effects and independent ranges in model 1
m7_spring_dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                        data = am_spring,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        share_range = list(FALSE, TRUE),
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE)

sanity(m7_spring_dpg)

saveRDS(m7_spring_dpg, file = here(dpg.mods, "m7_spring.rds"))

#### M8 ####
# forth order polynomial model fit with random effects and independent ranges in model 1, but no spatial random effects in model 2
m8_spring_dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                        data = am_spring,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                        spatial = list("on", "off"),
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        share_range = list(FALSE, TRUE),
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE)

sanity(m8_spring_dpg)

saveRDS(m8_spring_dpg, file = here(dpg.mods, "m8_spring.rds"))
