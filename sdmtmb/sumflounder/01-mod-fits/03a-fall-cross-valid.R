### created: 12/10/2022
### last updated: 04/16/2024

# 03a - CROSS VALIDATE FALL MODELS ####

## OBJECTIVE ####
# cross validate models fit to historical summer flounder data 
# approx 2 hour run time

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
library(tictoc)
# library(marmap)
# library(raster)
set.seed(120)

here()
### Environment set up ####
# folder location based on season: c("fall", "spring")
season <- "fall"
# folder location based on data set used: c("all-dat", "no-bio-out")
dat <- "no-bio-out"

### File Locations ####
# species folder 
sumflounder.dat <- here("sdmtmb", "sumflounder", "data")

# model locations
cv.dat <- here("sdmtmb", "sumflounder", "data", "cross-valid")

### LOAD DATA ####
# fall summer flounder data created in `01-prepare-data.R`
sf_fall <- readRDS(here(sumflounder.dat, "sumflounder_fall.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# fall mesh created in `01-prepare-data.R` 
fall_mesh <- readRDS(here(sumflounder.dat, "fall_mesh.rds"))

# clust <- sample(1:2, size = nrow(sf_fall), replace = T, prob = c(0.1, 0.9))
# saveRDS(clust, here("sdmtmb", "sumflounder", "data", "cross-valid", "sumflounder-fall-clust.rds"))

## MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)
tic()
### NO RANDOM EFFECTS ####
#### M1 #### 
# logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), 
             # # fold_ids = clust,
             k_folds = 5)
             # reml = TRUE)

### save the data
saveRDS(m1.cv, file = here(cv.dat, season, dat, "m1-cv.rds"))


#### M2 ####
m2.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "off", 
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       # # fold_ids = clust,
                       k_folds = 5) 

### save the data
saveRDS(m2.cv, file = here(cv.dat, season, dat, "m2-cv.rds"))



#### M3 ####
m3.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "off", 
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       silent = FALSE,   #extra optimization to help with convergence 
                       # # fold_ids = clust,
                       k_folds = 5)

### save the data
saveRDS(m3.cv, file = here(cv.dat, season, dat, "m3-cv.rds"))


#### M4 ####
m4.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "off", 
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       silent = FALSE, 
                       # # fold_ids = clust,
                       k_folds = 5) 

# save the data
saveRDS(m4.cv, file = here(cv.dat, season, dat, "m4-cv.rds"))

### SPATIAL RANDOM EFFECTS ONLY ####
##### M5 ####
m5.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"),  
                       spatial = "on", # spatial covariance with depth
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       # fold_ids = clust,
                       k_folds = 5) 

### save the data
saveRDS(m5.cv, file = here(cv.dat, season, dat, "m5-cv.rds"))



#### M6 ####
m6.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"),  
                       spatial = "on", 
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       # fold_ids = clust,
                       k_folds = 5)

### save the data
saveRDS(m6.cv, file = here(cv.dat, season, dat, "m6-cv.rds"))


#### M7 ####
m7.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"),  
                       spatial = "on", 
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       # fold_ids = clust,
                       k_folds = 5) 

### save the data
saveRDS(m7.cv, file = here(cv.dat, season, dat, "m7-cv.rds"))


#### M8 ####
m8.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"),  
                       spatial = "on", 
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       # fold_ids = clust,
                       k_folds = 5) 



# m8a.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
#                      #as.factor(EST_YEAR) + 
#                      AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "IID", 
#                    extra_time = 2020L,
#                    #spatial_varying = ~0 + AREA, 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    # fold_ids = clust,
#                    k_folds = 5,
#                    reml = TRUE) 

#sanity(m8a_fall)
#`sigma_Z` is smaller than 0.01
#Consider omitting this part of the model

### save the data
saveRDS(m8.cv, file = here(cv.dat, season, dat, "m8-cv.rds"))
#saveRDS(m8a.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m8-fall-cv2.rds"))

### SPATIOTEMPORAL MODELS: IID ####
#### M9 ####
m9.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                       data = sf_fall,
                       mesh = fall_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID", 
                       silent = FALSE,
                       # fold_ids = clust,
                       k_folds = 5) 

### save the data
saveRDS(m9.cv, file = here(cv.dat, season, dat, "m9-cv.rds"))

#### M10 ####
m10.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                        data = sf_fall,
                        mesh = fall_mesh,
                        family = tweedie(link = "log"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID", 
                        silent = FALSE, 
                        # fold_ids = clust,
                        k_folds = 5) 

### save the data
saveRDS(m10.cv, file = here(cv.dat, season, dat, "m10-cv.rds"))

#### M11 ####
m11.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                          EST_YEAR + 
                          AREA-1,
                        data = sf_fall,
                        mesh = fall_mesh,
                        family = tweedie(link = "log"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE, 
                        # fold_ids = clust,
                        k_folds = 5) 


# m11a.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
#                       #as.factor(EST_YEAR) + 
#                       AREA,#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"), 
#                     spatial = "on", 
#                     time = "EST_YEAR",
#                     spatiotemporal = "AR1", 
#                     extra_time = 2020L,
#                     #spatial_varying = ~0 + AREA, 
#                     control = sdmTMBcontrol(newton_loops = 1), 
#                     # fold_ids = clust,
#                     k_folds = 5,
#                     reml = TRUE) 



### save the data
saveRDS(m11.cv, file = here(cv.dat, season, dat, "m11-cv.rds"))
#saveRDS(m11a.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m11-fall-cv2.rds"))


#### M12 ####
m12.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                          EST_YEAR + 
                          AREA-1,
                        data = sf_fall,
                        mesh = fall_mesh,
                        family = tweedie(link = "log"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE, 
                        # fold_ids = clust,
                        k_folds = 5) 



### save the data
saveRDS(m12.cv, file = here(cv.dat, season, dat, "m12-cv.rds"))

toc()
