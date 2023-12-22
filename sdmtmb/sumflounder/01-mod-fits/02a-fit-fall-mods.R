### created: 12/10/2022
### last updated: 12/07/2023

# 02a - FIT FALL MODELS ####

## OBJECTIVE ####
# fit various forms of models using prepared summer flounder data for the fall season
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


## LOAD DATA ####
# fall summer flounder data created in `01-prepare-data.R`
sf_fall <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# fall mesh created in `01-prepare-data.R` 
fall_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mesh.rds"))

## MODEL FITS ####

### No random effect models ####
#### M1 ####
m1_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
             silent = FALSE)

sanity(m1_fall)

# save the data
saveRDS(m1_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m1_fall.rds"))

#### M2 ####
m2_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m2_fall)

# save the data
saveRDS(m2_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m2_fall.rds"))

#### M3 ####
m3_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m3_fall)

# save the data
saveRDS(m3_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m3_fall.rds"))

#### M4 ####
m4_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m4_fall)

# save the data
saveRDS(m4_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m4_fall.rds"))

### Spatial Only Models ####
#### M5 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m5_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", # spatial covariance with depth
             control = sdmTMBcontrol(newton_loops = 1), 
             silent = FALSE)

sanity(m5_fall) 

# save the data
saveRDS(m5_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m5_fall.rds"))

#### M6 ####
m6_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m6_fall) 

# save the data
saveRDS(m6_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m6_fall.rds"))

#### M7 ####
m7_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m7_fall) 

# save the data
saveRDS(m7_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m7_fall.rds"))

#### M8 ####
m8_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m8_fall) 

# save the data
saveRDS(m8_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m8_fall.rds"))


### Spatiotemporal Models - IID Structure ####

#### M9 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m9_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             time = "EST_YEAR",
             spatiotemporal = "IID", 
             silent = FALSE)

sanity(m9_fall)


### save the data
saveRDS(m9_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m9_fall.rds"))

#### M10 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m10_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID", 
                  silent = FALSE)

sanity(m10_fall)

### save the data
saveRDS(m10_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m10_fall.rds"))


#### M11 ####
# logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m11_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
                    EST_YEAR + 
                    AREA-1,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID",
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE) 

sanity(m11_fall)

### save the data
saveRDS(m11_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m11_fall.rds"))

#### M12 ####
#  logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m12_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                    EST_YEAR + 
                    AREA-1,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID",
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE) 

sanity(m12_fall)


### save the data
saveRDS(m12_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m12_fall.rds"))

