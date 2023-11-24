### created: 11/24/2023
### last updated: 11/24/2023

# 03a - CROSS VALIDATE FALL MODELS ####

## OBJECTIVE ####
# cross validate models fit to historical scup data 

### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)
library(patchwork)
# library(marmap)
# library(raster)
set.seed(120)

here()

### LOAD DATA ####

sf_fall <- readRDS(here("sdmtmb", "scup", "data", "scup_fall.rds"))
fall_mesh <- readRDS(here("sdmtmb", "scup", "data", "fall_mesh.rds"))
clust <- sample(1:2, size = nrow(sf_fall), replace = T, prob = c(0.1, 0.9))


## MODEL CROSS VALIDATIONS ####
library(future)
plan(multisession)

### No random effect models ####
#### M1 ####
m1_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)


#### M2 ####
m2_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)


#### M3 ####
m3_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)


#### M4 ####
m4_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)


### Spatial Only Models ####
#### M5 ####
# logistic regression of scup biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m5_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", # spatial covariance with depth
                  control = sdmTMBcontrol(newton_loops = 1), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)

#### M6 ####
m6_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)


#### M7 ####
m7_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)
#### M8 ####
m8_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)


### Spatiotemporal Models - IID Structure ####

#### M9 ####
#logistic regression of scup biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m9_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID", 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE)

#### M10 ####
#logistic regression of scup biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m10_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID", 
                   fold_ids = clust,
                   k_folds = length(unique(clust)),
                   reml = TRUE)


#### M11 ####
# logistic regression of scup biomass catch rate as a function of depth, year, and inside or outside wind area. 
m11_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) +  EST_YEAR +  AREA-1,
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   control = sdmTMBcontrol(newton_loops = 1), 
                   fold_ids = clust,
                   k_folds = length(unique(clust)),
                   reml = TRUE)


#### M12 ####
#  logistic regression of scup biomass catch rate as a function of depth, year, and inside or outside wind area. 
m12_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA-1,
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   control = sdmTMBcontrol(newton_loops = 1), 
                   fold_ids = clust,
                   k_folds = length(unique(clust)),
                   reml = TRUE)



### save the data
saveRDS(m1_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m1-fall-cv.rds"))
saveRDS(m2_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m2-fall-cv.rds"))
saveRDS(m3_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m3-fall-cv.rds"))
saveRDS(m4_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m4-fall-cv.rds"))
saveRDS(m5_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m5-fall-cv.rds"))
saveRDS(m6_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m6-fall-cv.rds"))
saveRDS(m7_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m7-fall-cv.rds"))
saveRDS(m8_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m8-fall-cv.rds"))
saveRDS(m9_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m9-fall-cv.rds"))
saveRDS(m10_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m10-fall-cv.rds"))
saveRDS(m11_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m11-fall-cv.rds"))
saveRDS(m12_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m12-fall-cv.rds"))



