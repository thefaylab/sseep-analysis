### created: 11/22/2023
### last updated: 12/09/2023

# 02a - FIT SPRING MODELS ####

## OBJECTIVE ####
# fit various forms of models using prepared scup data for the spring season
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
# scup data 
sf_spring <- readRDS(here("sdmtmb", "scup", "data", "scup_spring.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# mesh 
spring_mesh <- readRDS(here("sdmtmb", "scup", "data", "spring_mesh.rds"))

## MODEL FITS ####

### No random effect models ####
#### M1 ####
m1_spring_nb <- sdmTMB(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1,
                     data = sf_spring, 
                     mesh = spring_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "off", 
                     control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                     silent = FALSE)

sanity(m1_spring_nb)
tidy(m1_spring_nb)
tidy(m1_spring_nb, effects = "ran_pars")

#### M2 ####
m2_spring_nb <- sdmTMB(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                     data = sf_spring, 
                     mesh = spring_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "off", 
                     control = sdmTMBcontrol(newton_loops = 1), 
                     silent = FALSE)

sanity(m2_spring_nb)
tidy(m2_spring_nb)
tidy(m2_spring_nb, effects = "ran_pars")

### Spatial Only Models ####
# logistic regression of scup biomass (number) in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
# 
#### M3 ####
m3_spring_nb <- sdmTMB(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1,
                     data = sf_spring, 
                     mesh = spring_mesh,
                     family = nbinom2(link = "log"),  
                     spatial = "on", # spatial covariance with depth
                     control = sdmTMBcontrol(newton_loops = 1), 
                     silent = FALSE)

sanity(m3_spring_nb) 
tidy(m3_spring_nb)
tidy(m3_spring_nb, effects = "ran_pars")

#### M4 ####
m4_spring_nb <- sdmTMB(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                     data = sf_spring, 
                     mesh = spring_mesh,
                     family = nbinom2(link = "log"),  
                     spatial = "on", 
                     control = sdmTMBcontrol(newton_loops = 1), 
                     silent = FALSE)

sanity(m4_spring_nb) 
tidy(m4_spring_nb)
tidy(m4_spring_nb, effects = "ran_pars")

### Spatiotemporal Only Models ####
#logistic regression of scup biomass in tows as a function of AVGDEPTH with spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 

#### M5 ####
m5_spring_nb <- sdmTMB(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1, 
                     data = sf_spring,
                     mesh = spring_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "off", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     silent = FALSE)

sanity(m5_spring_nb)
tidy(m5_spring_nb)
tidy(m5_spring_nb, effects = "ran_pars")

#### M6 ####
m6_spring_nb <- sdmTMB(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1, 
                     data = sf_spring,
                     mesh = spring_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "off", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     silent = FALSE)

sanity(m6_spring_nb)
tidy(m6_spring_nb)
tidy(m6_spring_nb, effects = "ran_pars")

### Spatiotemporal + Spatial Models ####

#### M7 ####
#logistic regression of scup biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m7_spring_nb <- sdmTMB(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1, 
                     data = sf_spring,
                     mesh = spring_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     silent = FALSE)

sanity(m7_spring_nb)
tidy(m7_spring_nb)
tidy(m7_spring_nb, effects = "ran_pars")

#### M8 #### 
m8_spring_nb <- sdmTMB(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1, 
                     data = sf_spring,
                     mesh = spring_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     silent = FALSE)

sanity(m8_spring_nb)
tidy(m8_spring_nb)
tidy(m8_spring_nb, effects = "ran_pars")


### save the data
saveRDS(m1_spring_nb, file = here("sdmtmb", "scup", "data", "mods", "m1_spring_nb.rds"))
saveRDS(m2_spring_nb, file = here("sdmtmb", "scup", "data", "mods", "m2_spring_nb.rds"))
saveRDS(m3_spring_nb, file = here("sdmtmb", "scup", "data", "mods", "m3_spring_nb.rds"))
saveRDS(m4_spring_nb, file = here("sdmtmb", "scup", "data", "mods", "m4_spring_nb.rds"))
saveRDS(m5_spring_nb, file = here("sdmtmb", "scup", "data", "mods", "m5_spring_nb.rds"))
saveRDS(m6_spring_nb, file = here("sdmtmb", "scup", "data", "mods", "m6_spring_nb.rds"))
saveRDS(m7_spring_nb, file = here("sdmtmb", "scup", "data", "mods", "m7_spring_nb.rds"))
saveRDS(m8_spring_nb, file = here("sdmtmb", "scup", "data", "mods", "m8_spring_nb.rds"))
