### created: 12/10/2022
### last updated: 12/13/2023

# 03a - CROSS VALIDATE FALL MODELS ####

## OBJECTIVE ####
# cross validate models fit to historical summer flounder data 

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
# fall summer flounder data created in `01-prepare-data.R`
sf_fall <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# fall mesh created in `01-prepare-data.R` 
fall_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mesh.rds"))

clust <- sample(1:2, size = nrow(sf_fall), replace = T, prob = c(0.1, 0.9))
saveRDS(clust, here("sdmtmb", "sumflounder", "data", "cross-valid", "sumflounder-fall-clust.rds"))

## MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)

### NO RANDOM EFFECTS ####
#### M1 #### 
# logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), 
             fold_ids = clust,
             k_folds = length(unique(clust)),
             reml = TRUE)

### save the data
saveRDS(m1fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m1-fall-cv.rds"))


#### M2 ####
m2fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "off", 
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 

### save the data
saveRDS(m2fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m2-fall-cv.rds"))



#### M3 ####
m3fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "off", 
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       silent = FALSE,   #extra optimization to help with convergence 
                       fold_ids = clust,
                       k_folds = length(unique(clust)))

### save the data
saveRDS(m3fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m3-fall-cv.rds"))


#### M4 ####
m4fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "off", 
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       silent = FALSE, 
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 

# save the data
saveRDS(m4fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m4-fall-cv.rds"))

### SPATIAL RANDOM EFFECTS ONLY ####
##### M5 ####
m5fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"),  
                       spatial = "on", # spatial covariance with depth
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 

### save the data
saveRDS(m5fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m5-fall-cv.rds"))



#### M6 ####
m6fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"),  
                       spatial = "on", 
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       fold_ids = clust,
                       k_folds = length(unique(clust)))

### save the data
saveRDS(m6fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m6-fall-cv.rds"))


#### M7 ####
m7fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"),  
                       spatial = "on", 
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 

### save the data
saveRDS(m7fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m7-fall-cv.rds"))


#### M8 ####
m8fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                       data = sf_fall, 
                       mesh = fall_mesh,
                       family = tweedie(link = "log"),  
                       spatial = "on", 
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 



# m8afall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
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
#                    fold_ids = clust,
#                    k_folds = length(unique(clust)),
#                    reml = TRUE) 

#sanity(m8a_fall)
#`sigma_Z` is smaller than 0.01
#Consider omitting this part of the model

### save the data
saveRDS(m8fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m8-fall-cv.rds"))
#saveRDS(m8afall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m8-fall-cv2.rds"))

### SPATIOTEMPORAL MODELS: IID ####
#### M9 ####
m9fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                       data = sf_fall,
                       mesh = fall_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID", 
                       silent = FALSE,
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 

### save the data
saveRDS(m9fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m9-fall-cv.rds"))

#### M10 ####
m10fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                        data = sf_fall,
                        mesh = fall_mesh,
                        family = tweedie(link = "log"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID", 
                        silent = FALSE, 
                        fold_ids = clust,
                        k_folds = length(unique(clust))) 

### save the data
saveRDS(m10fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m10-fall-cv.rds"))

#### M11 ####
m11fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
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
                        fold_ids = clust,
                        k_folds = length(unique(clust))) 


# m11afall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
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
#                     fold_ids = clust,
#                     k_folds = length(unique(clust)),
#                     reml = TRUE) 



### save the data
saveRDS(m11fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m11-fall-cv.rds"))
#saveRDS(m11afall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m11-fall-cv2.rds"))


#### M12 ####
m12fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
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
                        fold_ids = clust,
                        k_folds = length(unique(clust))) 



### save the data
saveRDS(m12fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m12-fall-cv.rds"))

#### M13 ####
#  Newton failed to find minimum; NA/NaN gradient evaluation
# m13fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) +EST_YEAR,#-1,
#                   data = sf_fall,
#                   mesh = fall_mesh,
#                   family = tweedie(link = "log"),
#                   spatial = "on",
#                   time = "EST_YEAR",
#                   spatiotemporal = "RW",
#                   extra_time = 2020L,
#                   control = sdmTMBcontrol(nlminb_loops = 2),
#                   fold_ids = clust,
#                   k_folds = length(unique(clust)),
#                   reml = TRUE)
# # 
# sanity(m13_fall)

# ### save the data
# saveRDS(m13_fall, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m13_fall.rds"))

#### M14 ####
# 
# m14fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
#                      #as.factor(EST_YEAR) + 
#                      AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW", 
#                    extra_time = 2020L, 
#                    spatial_varying = ~0 + AREA, 
#                    reml = TRUE, 
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    fold_ids = clust,
#                    k_folds = length(unique(clust))) 
# 
# #sanity(m14_fall)
# 
# m14afall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
#                       #as.factor(EST_YEAR) + 
#                       AREA,#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"), 
#                     spatial = "on", 
#                     time = "EST_YEAR",
#                     spatiotemporal = "RW", 
#                     extra_time = 2020L,
#                     #spatial_varying = ~0 + AREA, 
#                     control = sdmTMBcontrol(newton_loops = 1), 
#                     fold_ids = clust,
#                     k_folds = length(unique(clust)),
#                     reml = TRUE)
# 
# #sanity(m14a_fall)
# 
# # save the data 
# saveRDS(m14fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m14-fall-cv.rds"))
# saveRDS(m14afall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m14-fall-cv2.rds"))
# 
# #### M15 ####
# # m15fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
# #                     EST_YEAR +
# #                      AREA,#-1,
# #                    data = sf_fall,
# #                    mesh = fall_mesh,
# #                    family = tweedie(link = "log"),
# #                    spatial = "on",
# #                    time = "EST_YEAR",
# #                    spatiotemporal = "IID",
# #                    extra_time = 2020L,
# #                    #spatial_varying = ~0 + AREA,
# #                    control = sdmTMBcontrol(newton_loops = 1),
# #                    fold_ids = clust,
# #                    k_folds = length(unique(clust)),
# #                    reml = TRUE)
# #  Newton failed to find minimum.  NA/NaN gradient evaluation
# 
# m15a_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                       #as.factor(EST_YEAR) +
#                       AREA,#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"),
#                     spatial = "on",
#                     time = "EST_YEAR",
#                     spatiotemporal = "IID",
#                     extra_time = 2020L,
#                     spatial_varying = ~0 + AREA,
#                     control = sdmTMBcontrol(newton_loops = 1),
#                     fold_ids = clust,
#                     k_folds = length(unique(clust)),
#                     reml = TRUE)
# #Warning messages:
# # 1: In sqrt(diag(cov)) : NaNs produced
# # 2: The model may not have converged: non-positive-definite Hessian matrix. 
# #sanity(m15a_fall)
# 
# ### save the data
# saveRDS(m15a_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m15-fall-cv.rds"))
# 
# 
# m15b_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                       #as.factor(EST_YEAR) +
#                       AREA,#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"),
#                     spatial = "on",
#                     time = "EST_YEAR",
#                     spatiotemporal = "IID",
#                     extra_time = 2020L,
#                     #spatial_varying = ~0 + AREA,
#                     control = sdmTMBcontrol(newton_loops = 1),
#                     fold_ids = clust,
#                     k_folds = length(unique(clust)),
#                     reml = TRUE)
# #sanity(m15b_fall)
# 
# ### save the data
# saveRDS(m15b_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m15-fall-cv2.rds"))
# 
# #### M16 ####
# 
# # m16fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
# #                     EST_YEAR +
# #                      AREA,#-1,
# #                    data = sf_fall,
# #                    mesh = fall_mesh,
# #                    family = tweedie(link = "log"),
# #                    spatial = "on",
# #                    time = "EST_YEAR",
# #                    spatiotemporal = "AR1",
# #                    extra_time = 2020L,
# #                    #spatial_varying = ~0 + AREA,
# #                    control = sdmTMBcontrol(newton_loops = 1),
# #                    fold_ids = clust,
# #                    k_folds = length(unique(clust)),
# #                    reml = TRUE)
# # Newton failed to find minimum. NA/NaN gradient evaluation
# 
# m16a_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
#                       #as.factor(EST_YEAR) +
#                       AREA,#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"),
#                     spatial = "on",
#                     time = "EST_YEAR",
#                     spatiotemporal = "AR1",
#                     extra_time = 2020L,
#                     spatial_varying = ~0 + AREA,
#                     control = sdmTMBcontrol(newton_loops = 1),
#                     fold_ids = clust,
#                     k_folds = length(unique(clust)),
#                     reml = TRUE)
# #sanity(m16a_fall)
# 
# saveRDS(m16a_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m16-fall-cv.rds"))
# 
# m16b_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
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
#                     fold_ids = clust,
#                     k_folds = length(unique(clust)),
#                     reml = TRUE)
# # sanity(m16b_fall)
# # 
# saveRDS(m16b_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m16-fall-cv2.rds"))
# 
# #### M17 ####
# 
# # m17_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
# #                     EST_YEAR +
# #                      AREA,#-1,
# #                    data = sf_fall,
# #                    mesh = fall_mesh,
# #                    family = tweedie(link = "log"),
# #                    spatial = "on",
# #                    time = "EST_YEAR",
# #                    spatiotemporal = "RW",
# #                    extra_time = 2020L,
# #                    #spatial_varying = ~0 + AREA,
# #                    control = sdmTMBcontrol(newton_loops = 1),
# #                    fold_ids = clust,
# #                    k_folds = length(unique(clust)),
# #                    reml = TRUE)
# # Newton failed to find minimum. NA/NaN gradient evaluation
# 
# # m17a_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
# #                       #as.factor(EST_YEAR) +
# #                       AREA,#-1,
# #                     data = sf_fall,
# #                     mesh = fall_mesh,
# #                     family = tweedie(link = "log"),
# #                     spatial = "on",
# #                     time = "EST_YEAR",
# #                     spatiotemporal = "RW",
# #                     extra_time = 2020L,
# #                     spatial_varying = ~0 + AREA,
# #                     control = sdmTMBcontrol(newton_loops = 1),
# #                     fold_ids = clust,
# #                     k_folds = length(unique(clust)),
# #                     reml = TRUE)
# # Newton failed to find minimum. NA/NaN gradient evaluation
# 
# m17b_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                       #as.factor(EST_YEAR) +
#                       AREA,#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"),
#                     spatial = "on",
#                     time = "EST_YEAR",
#                     spatiotemporal = "RW",
#                     extra_time = 2020L,
#                     #spatial_varying = ~0 + AREA,
#                     control = sdmTMBcontrol(newton_loops = 1),
#                     fold_ids = clust,
#                     k_folds = length(unique(clust)),
#                     reml = TRUE)
# 
# #sanity(m17b_fall)
# 
# saveRDS(m17b_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m17-fall-cv.rds"))
# 
# 
