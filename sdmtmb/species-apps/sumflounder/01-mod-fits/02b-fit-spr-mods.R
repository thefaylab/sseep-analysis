### created: 12/10/2022
### last updated: 04/16/2024

# 02b - FIT SPRING MODELS ####

## OBJECTIVE ####
# fit various forms of models using prepared summer flounder data for the spring season
# include predictor for index standardization 
# estimate a coefficient for each year 
# include inside/outside wind area designation as part of the predictor

## LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)

here()
dat.files <- here("data", "rds", "sdmtmb", species)
mods.dat <- here(dat.files, "mods")
season <- "spring"

## LOAD DATA ####
# summer flounder data 
sf_spring <- readRDS(here(dat.files, "sumflounder_spring.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# mesh 
spring_mesh <- readRDS(here(dat.files, "spring_mesh.rds"))

## MODEL FITS ####

### No random effect models ####
#### M1 ####
m1_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                  data = sf_spring, 
                  mesh = spring_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m1_spring)

# save the data
saveRDS(m1_spring, file = here(mods.dat, season, "m1_spring.rds"))

#### M2 ####
m2_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = sf_spring, 
                  mesh = spring_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m2_spring)

# save the data
saveRDS(m2_spring, file = here(mods.dat, season, "m2_spring.rds"))

#### M3 ####
m3_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = sf_spring, 
                  mesh = spring_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m3_spring)

# save the data
saveRDS(m3_spring, file = here(mods.dat, season,"m3_spring.rds"))

#### M4 ####
m4_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                  data = sf_spring, 
                  mesh = spring_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m4_spring)

# save the data
saveRDS(m4_spring, file = here(mods.dat, season, "m4_spring.rds"))

### Spatial Only Models ####
#### M5 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m5_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                  data = sf_spring, 
                  mesh = spring_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", # spatial covariance with depth
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m5_spring) 

# save the data
saveRDS(m5_spring, file = here(mods.dat, season, "m5_spring.rds"))

#### M6 ####
m6_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = sf_spring, 
                  mesh = spring_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m6_spring) 

# save the data
saveRDS(m6_spring, file = here(mods.dat, season, "m6_spring.rds"))

#### M7 ####
m7_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = sf_spring, 
                  mesh = spring_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m7_spring) 

# save the data
saveRDS(m7_spring, file = here(mods.dat, season, "m7_spring.rds"))

#### M8 ####
m8_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                  data = sf_spring, 
                  mesh = spring_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m8_spring) 

# save the data
saveRDS(m8_spring, file = here(mods.dat, season, "m8_spring.rds"))


### Spatiotemporal Models - IID Structure ####

#### M9 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m9_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                  data = sf_spring,
                  mesh = spring_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID", 
                  control = sdmTMBcontrol(newton_loops = 1),
                  silent = FALSE)

sanity(m9_spring)


### save the data
saveRDS(m9_spring, file = here(mods.dat, season, "m9_spring.rds"))

#### M10 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m10_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID", 
                   control = sdmTMBcontrol(newton_loops = 1),
                   silent = FALSE)

sanity(m10_spring)

### save the data
saveRDS(m10_spring, file = here(mods.dat, season, "m10_spring.rds"))


#### M11 ####
# logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m11_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
                     EST_YEAR + 
                     AREA-1,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   control = sdmTMBcontrol(newton_loops = 1), 
                   silent = FALSE) 

sanity(m11_spring)

### save the data
saveRDS(m11_spring, file = here(mods.dat, season, "m11_spring.rds"))

#### M12 ####
#  logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m12_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                     EST_YEAR + 
                     AREA-1,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   control = sdmTMBcontrol(newton_loops = 1), 
                   silent = FALSE) 

sanity(m12_spring)


### save the data
saveRDS(m12_spring, file = here(mods.dat, season, "m12_spring.rds"))
