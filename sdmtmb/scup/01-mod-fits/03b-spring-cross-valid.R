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

sf_spring <- readRDS(here("sdmtmb", "scup", "data", "scup_spring.rds"))
spring_mesh <- readRDS(here("sdmtmb", "scup", "data", "spring_mesh.rds"))
clust <- sample(1:2, size = nrow(sf_spring), replace = T, prob = c(0.1, 0.9))


## MODEL CROSS VALIDATIONS ####
library(future)
library(future.apply)
plan(multisession)


### No random effect models ####
#### M1 ####
m1_spr.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1,
                        data = sf_spring, 
                        mesh = spring_mesh,
                        family = nbinom2(link = "log"), 
                        spatial = "off", 
                        control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                        fold_ids = clust,
                        k_folds = length(unique(clust)),
                        reml=TRUE)

#### M2 ####
m2_spr.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                        data = sf_spring, 
                        mesh = spring_mesh,
                        family = nbinom2(link = "log"), 
                        spatial = "off", 
                        control = sdmTMBcontrol(newton_loops = 5), 
                        fold_ids = clust,
                        k_folds = length(unique(clust)),
                       reml=TRUE)


### Spatial Only Models ####
#### M3 ####
m3_spr.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1,
                        data = sf_spring, 
                        mesh = spring_mesh,
                        family = nbinom2(link = "log"),  
                        spatial = "on", # spatial covariance with depth
                        control = sdmTMBcontrol(newton_loops = 1), 
                        fold_ids = clust,
                        k_folds = length(unique(clust)),
                       reml=TRUE)


#### M4 ####
m4_spr.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                        data = sf_spring, 
                        mesh = spring_mesh,
                        family = nbinom2(link = "log"),  
                        spatial = "on", 
                        control = sdmTMBcontrol(newton_loops = 1), 
                        fold_ids = clust,
                        k_folds = length(unique(clust)),
                        reml = TRUE)


### Spatiotemporal Only Models ####
#### M5 ####
m5_spr.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1, 
                        data = sf_spring,
                        mesh = spring_mesh,
                        family = nbinom2(link = "log"), 
                        spatial = "off", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID", 
                        fold_ids = clust,
                        k_folds = length(unique(clust)),
                        reml = TRUE)



#### M6 ####
m6_spr.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1, 
                        data = sf_spring,
                        mesh = spring_mesh,
                        family = nbinom2(link = "log"), 
                        spatial = "off", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID", 
                        fold_ids = clust,
                        k_folds = length(unique(clust)),
                        reml = TRUE)

### Spatiotemporal + Spatial Models ####
#### M7 ####
m7_spr.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR - 1, 
                        data = sf_spring,
                        mesh = spring_mesh,
                        family = nbinom2(link = "log"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID", 
                        fold_ids = clust,
                        k_folds = length(unique(clust)),
                        reml = TRUE)

#### M8 #### 
m8_spr.cv <- sdmTMB_cv(EXPCATCHNUM ~ s(AVGDEPTH) + EST_YEAR + AREA - 1, 
                        data = sf_spring,
                        mesh = spring_mesh,
                        family = nbinom2(link = "log"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID", 
                        fold_ids = clust,
                        k_folds = length(unique(clust)),
                        reml = TRUE)




### save the data
saveRDS(m1_spr.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m1-spring-cv.rds"))
saveRDS(m2_spr.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m2-spring-cv.rds"))
saveRDS(m3_spr.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m3-spring-cv.rds"))
saveRDS(m4_spr.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m4-spring-cv.rds"))
saveRDS(m5_spr.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m5-spring-cv.rds"))
saveRDS(m6_spr.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m6-spring-cv.rds"))
saveRDS(m7_spr.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m7-spring-cv.rds"))
saveRDS(m8_spr.cv, file = here("sdmtmb", "scup", "data", "cross-valid", "m8-spring-cv.rds"))
