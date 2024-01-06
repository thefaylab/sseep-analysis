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
library(future.apply)
plan(multisession)


### No random effect models ####
#### M1 ####
m1_fall.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1,
                     data = sf_fall, 
                     mesh = fall_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "off", 
                     control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                     fold_ids = clust,
                     k_folds = length(unique(clust)))

#### M2 ####
m2_fall.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                     data = sf_fall, 
                     mesh = fall_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "off", 
                     control = sdmTMBcontrol(newton_loops = 1), 
                     fold_ids = clust,
                     k_folds = length(unique(clust)))


### Spatial Only Models ####
#### M3 ####
m3_fall.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1,
                     data = sf_fall, 
                     mesh = fall_mesh,
                     family = nbinom2(link = "log"),  
                     spatial = "on", # spatial covariance with depth
                     control = sdmTMBcontrol(newton_loops = 1), 
                     fold_ids = clust,
                     k_folds = length(unique(clust)))


#### M4 ####
m4_fall.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                     data = sf_fall, 
                     mesh = fall_mesh,
                     family = nbinom2(link = "log"),  
                     spatial = "on", 
                     control = sdmTMBcontrol(newton_loops = 1), 
                     fold_ids = clust,
                     k_folds = length(unique(clust)))


### Spatiotemporal Only Models ####
#### M5 ####
m5_fall.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1, 
                     data = sf_fall,
                     mesh = fall_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "off", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     fold_ids = clust,
                     k_folds = length(unique(clust)))



#### M6 ####
m6_fall.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1, 
                     data = sf_fall,
                     mesh = fall_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "off", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     fold_ids = clust,
                     k_folds = length(unique(clust)))

### Spatiotemporal + Spatial Models ####
#### M7 ####
m7_fall.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1, 
                     data = sf_fall,
                     mesh = fall_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     fold_ids = clust,
                     k_folds = length(unique(clust)))


#### M8 #### 
m8_fall.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1, 
                     data = sf_fall,
                     mesh = fall_mesh,
                     family = nbinom2(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     control = sdmTMBcontrol(newton_loops = 1), 
                     fold_ids = clust,
                     k_folds = length(unique(clust)))




### save the data
saveRDS(m1_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m1-fall-cv.rds"))
saveRDS(m2_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m2-fall-cv.rds"))
saveRDS(m3_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m3-fall-cv.rds"))
saveRDS(m4_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m4-fall-cv.rds"))
saveRDS(m5_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m5-fall-cv.rds"))
saveRDS(m6_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m6-fall-cv.rds"))
saveRDS(m7_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m7-fall-cv.rds"))
saveRDS(m8_fall.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m8-fall-cv.rds"))



