### created: 12/10/2022
### last updated: 08/14/2023

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
# models 
# m1_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m1_fall.rds"))
# m2_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m2_fall.rds"))
# m3_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m3_fall.rds"))
# m4_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m4_fall.rds"))
# m5_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m5_fall.rds"))
# m6_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m6_fall.rds"))
# m7_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m7_fall.rds"))

sf_fall <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds"))
fall_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mesh.rds"))
clust <- sample(1:2, size = nrow(sf_fall), replace = T, prob = c(0.1, 0.9))


## MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)

### ####
# M1 - logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), 
             fold_ids = clust,
             k_folds = length(unique(clust)),
             reml = TRUE)

# save the data
saveRDS(m1fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m1-fall-cv.rds"))


# M2 - no spatial random effects, but 
m2fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), 
             fold_ids = clust,
             k_folds = length(unique(clust)),
             reml = TRUE) 

# save the data
saveRDS(m2fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m2-fall-cv.rds"))



# M3 logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects
m3fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1),   #extra optimization to help with convergence 
             fold_ids = clust,
             k_folds = length(unique(clust)),
             reml = TRUE)

# save the data
saveRDS(m3fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m3-fall-cv.rds"))


# M4 
m4fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1), 
             fold_ids = clust,
             k_folds = length(unique(clust)),
             reml = TRUE) 

# save the data
saveRDS(m4fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m4-fall-cv.rds"))


# m5
m5fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with AVGDEPTH
             time = "EST_YEAR",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)? 
             control = sdmTMBcontrol(newton_loops = 1), 
             fold_ids = clust,
             k_folds = length(unique(clust)),
             reml = TRUE) 

# save the data
saveRDS(m5fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m5-fall-cv.rds"))



# M6 
m6fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1, 
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "EST_YEAR",
             spatiotemporal = "IID",  #possbly AR1 (adapt slowly over time)?
             fold_ids = clust,
             k_folds = length(unique(clust)),
             reml = TRUE)

# save the data
saveRDS(m6fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m6-fall-cv.rds"))


# m7 
m7fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             time = "EST_YEAR",
             spatiotemporal = "IID", 
             fold_ids = clust,
             k_folds = length(unique(clust)),
             reml = TRUE) 

# save the data
saveRDS(m7fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m7-fall-cv.rds"))


#### M8 ####
#  logistic regression of summer flounder biomass catch rate as a function of depth and inside or outside wind area. 
m8fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                    #as.factor(EST_YEAR) + 
                    as.factor(AREA),#-1,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID",
                  extra_time = 2020L, # add in missing year of survey
                  spatial_varying = ~0 + as.factor(AREA), # a random field for spatially varying slopes that represent temporal trends that vary spatially
                  control = sdmTMBcontrol(newton_loops = 1), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE) 



m8afall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                     #as.factor(EST_YEAR) + 
                     as.factor(AREA),#-1,
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID", 
                   extra_time = 2020L,
                   #spatial_varying = ~0 + as.factor(AREA), 
                   control = sdmTMBcontrol(newton_loops = 1), 
                   fold_ids = clust,
                   k_folds = length(unique(clust)),
                   reml = TRUE) 

#sanity(m8a_fall)
#`sigma_Z` is smaller than 0.01
#Consider omitting this part of the model

### save the data
saveRDS(m8fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m8-fall-cv.rds"))
saveRDS(m8afall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m8-fall-cv2.rds"))

### AR1 Models ####
#### M9 ####
#  
m9fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "AR1",
                  extra_time = 2020L, 
                  #control = sdmTMBcontrol(nlminb_loops = 2), 
                  fold_ids = clust,
                  k_folds = length(unique(clust)),
                  reml = TRUE) 
#Error in match.arg(method) : 'arg' must be of length 1

### save the data
saveRDS(m9fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m9-fall-cv.rds"))

#### M10 ####
# 
m10fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH), # + as.factor(EST_YEAR)-1,
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "AR1", 
                   extra_time = 2020L, 
                   control = sdmTMBcontrol(newton_loops = 1), 
                   fold_ids = clust,
                   k_folds = length(unique(clust)),
                   reml = TRUE) 
#sanity(m10_fall)

### save the data
saveRDS(m10fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m10-fall-cv.rds"))

#### M11 ####
#  revisit no convergence
m11fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                     #as.factor(EST_YEAR) + 
                     as.factor(AREA),#-1,
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "AR1", 
                   extra_time = 2020L, 
                   spatial_varying = ~0 + as.factor(AREA), 
                   control = sdmTMBcontrol(newton_loops = 1), 
                   fold_ids = clust,
                   k_folds = length(unique(clust)),
                   reml = TRUE) 
#In sqrt(diag(cov)) : NaNs produced
# The model may not have converged: non-positive-definite Hessian matrix.
#sanity(m11_fall)

m11afall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                      #as.factor(EST_YEAR) + 
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "AR1", 
                    extra_time = 2020L,
                    #spatial_varying = ~0 + as.factor(AREA), 
                    control = sdmTMBcontrol(newton_loops = 1), 
                    fold_ids = clust,
                    k_folds = length(unique(clust)),
                    reml = TRUE) 

#(m11a_fall)

### save the data
saveRDS(m11fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m11-fall-cv.rds"))
saveRDS(m11afall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m11-fall-cv2.rds"))

### RW Models ####
#### M12 ####
# 
m12fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH,
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "RW", 
                   extra_time = 2020L, 
                   control = sdmTMBcontrol(nlminb_loops = 1), 
                   fold_ids = clust,
                   k_folds = length(unique(clust)),
                   reml = TRUE) 

#sanity(m12_fall)

### save the data
saveRDS(m12fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m12-fall-cv.rds"))

#### M13 ####
#  Newton failed to find minimum; NA/NaN gradient evaluation
# m13fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR),#-1,
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
m14fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                     #as.factor(EST_YEAR) + 
                     as.factor(AREA),#-1,
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "RW", 
                   extra_time = 2020L, 
                   spatial_varying = ~0 + as.factor(AREA), 
                   reml = TRUE, 
                   control = sdmTMBcontrol(newton_loops = 1),
                   fold_ids = clust,
                   k_folds = length(unique(clust))) 

#sanity(m14_fall)

m14afall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                      #as.factor(EST_YEAR) + 
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "RW", 
                    extra_time = 2020L,
                    #spatial_varying = ~0 + as.factor(AREA), 
                    control = sdmTMBcontrol(newton_loops = 1), 
                    fold_ids = clust,
                    k_folds = length(unique(clust)),
                    reml = TRUE)

#sanity(m14a_fall)

# save the data 
saveRDS(m14fall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m14-fall-cv.rds"))
saveRDS(m14afall.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m14-fall-cv2.rds"))

#### M15 ####
# m15fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                      as.factor(EST_YEAR) +
#                      as.factor(AREA),#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "IID",
#                    extra_time = 2020L,
#                    #spatial_varying = ~0 + as.factor(AREA),
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    fold_ids = clust,
#                    k_folds = length(unique(clust)),
#                    reml = TRUE)
#  Newton failed to find minimum.  NA/NaN gradient evaluation

m15a_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    extra_time = 2020L,
                    spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    fold_ids = clust,
                    k_folds = length(unique(clust)),
                    reml = TRUE)
#Warning messages:
# 1: In sqrt(diag(cov)) : NaNs produced
# 2: The model may not have converged: non-positive-definite Hessian matrix. 
#sanity(m15a_fall)

### save the data
saveRDS(m15a_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m15-fall-cv.rds"))


m15b_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    extra_time = 2020L,
                    #spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    fold_ids = clust,
                    k_folds = length(unique(clust)),
                    reml = TRUE)
#sanity(m15b_fall)

### save the data
saveRDS(m15b_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m15-fall-cv2.rds"))

#### M16 ####

# m16fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
#                      as.factor(EST_YEAR) +
#                      as.factor(AREA),#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1",
#                    extra_time = 2020L,
#                    #spatial_varying = ~0 + as.factor(AREA),
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    fold_ids = clust,
#                    k_folds = length(unique(clust)),
#                    reml = TRUE)
# Newton failed to find minimum. NA/NaN gradient evaluation

m16a_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "AR1",
                    extra_time = 2020L,
                    spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    fold_ids = clust,
                    k_folds = length(unique(clust)),
                    reml = TRUE)
#sanity(m16a_fall)

saveRDS(m16a_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m16-fall-cv.rds"))

m16b_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "AR1",
                    extra_time = 2020L,
                    #spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    fold_ids = clust,
                    k_folds = length(unique(clust)),
                    reml = TRUE)
# sanity(m16b_fall)
# 
saveRDS(m16b_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m16-fall-cv2.rds"))

#### M17 ####

# m17_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                      as.factor(EST_YEAR) +
#                      as.factor(AREA),#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW",
#                    extra_time = 2020L,
#                    #spatial_varying = ~0 + as.factor(AREA),
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    fold_ids = clust,
#                    k_folds = length(unique(clust)),
#                    reml = TRUE)
# Newton failed to find minimum. NA/NaN gradient evaluation

# m17a_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                       #as.factor(EST_YEAR) +
#                       as.factor(AREA),#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"),
#                     spatial = "on",
#                     time = "EST_YEAR",
#                     spatiotemporal = "RW",
#                     extra_time = 2020L,
#                     spatial_varying = ~0 + as.factor(AREA),
#                     control = sdmTMBcontrol(newton_loops = 1),
#                     fold_ids = clust,
#                     k_folds = length(unique(clust)),
#                     reml = TRUE)
# Newton failed to find minimum. NA/NaN gradient evaluation

m17b_fall.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "RW",
                    extra_time = 2020L,
                    #spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    fold_ids = clust,
                    k_folds = length(unique(clust)),
                    reml = TRUE)

#sanity(m17b_fall)

saveRDS(m17b_fall.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m17-fall-cv.rds"))


