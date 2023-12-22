### created: 12/10/2022
### last updated: 12/13/2023

# 03b - CROSS VALIDATE SPRING MODELS ####

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
# spring summer flounder data created in `01-prepare-data.R`
sf_spring <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_spring.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# spring mesh created in `01-prepare-data.R`  
spring_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mesh.rds"))


clust <- sample(1:2, size = nrow(sf_spring), replace = T, prob = c(0.1, 0.9))
saveRDS(clust, here("sdmtmb", "sumflounder", "data", "cross-valid", "sumflounder-spr-clust.rds"))


## MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)

### NO RANDOM EFFECTS ####
#### M1 #### 
m1spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust)))

### save the data
saveRDS(m1spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m1-spring-cv.rds"))


#### M2 ####
m2spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

### save the data
saveRDS(m2spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m2-spring-cv.rds"))



#### M3 #### 
m3spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1),
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust)))

### save the data
saveRDS(m3spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m3-spring-cv.rds"))


#### M4 ####
m4spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

### save the data
saveRDS(m4spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m4-spring-cv.rds"))

### SPATIAL RANDOM EFFECTS ####
#### M5 ####
m5spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", # spatial covariance with depth
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 


### save the data
saveRDS(m5spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m5-spring-cv.rds"))



#### M6 ####
m6spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

### save the data
saveRDS(m6spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m6-spring-cv.rds"))


#### M7 #### 
m7spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

### save the data
saveRDS(m7spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m7-spring-cv.rds"))

#### M8 ####
m8spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 


# m8spr.cv2 <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) + as.factor(AREA),
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"), 
#                      spatial = "on", 
#                      time = "EST_YEAR",
#                      spatiotemporal = "IID", 
#                      #spatial_varying = ~0 + as.factor(scaled_area), 
#                      control = sdmTMBcontrol(newton_loops = 1), 
#                      fold_ids = clust,
#                      k_folds = length(unique(clust)),
#                      reml = TRUE) 

# model sanity check
#sanity(m8_spring)
# no convergence, tau_z and sigma_z e

# extract the intercepts and coefficients
# tidy(m8_spring, conf.int = TRUE) 
# tidy(m8_spring, "ran_pars", conf.int = TRUE)

#sanity(m8_spring2)

### save the data 
saveRDS(m8spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m8-spring-cv.rds"))
#saveRDS(m8spr.cv2, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m8-spring-cv2.rds"))

### SPATIOTEMPORAL MODELS: IID STRUCTURE ####
#### M9 ####
m9spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                      data = sf_spring,
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID", 
                      control = sdmTMBcontrol(newton_loops = 1),
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 


### save the data 
saveRDS(m9spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m9-spring-cv.rds"))

#### M10 ####
m10spr.cv<- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                      data = sf_spring,
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID", 
                      control = sdmTMBcontrol(newton_loops = 1),
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 


### save the data 
saveRDS(m10spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m10-spring-cv.rds"))

#### M11 ####
m11spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                         EST_YEAR + 
                         AREA-1,
                       data = sf_spring,
                       mesh = spring_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 

# m11spr.cv2 <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)+ as.factor(AREA),
#                        data = sf_spring,
#                        mesh = spring_mesh,
#                        family = tweedie(link = "log"), 
#                        spatial = "on", 
#                        time = "EST_YEAR",
#                        spatiotemporal = "AR1", 
#                        #spatial_varying = ~0 + as.factor(scaled_area), 
#                        control = sdmTMBcontrol(newton_loops = 1), 
#                        fold_ids = clust,
#                        k_folds = length(unique(clust)),
#                        reml = TRUE) 

# model sanity check
# sanity(m11_spring)
# #sanity(m11_spring2)
# 
# # extract the intercepts and coefficients
# tidy(m11_spring, conf.int = TRUE) 
# tidy(m11_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m11spr.cv, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m11-spring-cv.rds"))
#saveRDS(m11spr.cv2, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m11spr.cv2.rds"))

#### M12 ####
m12spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                         EST_YEAR + 
                         AREA-1,
                       data = sf_spring,
                       mesh = spring_mesh,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE,
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 


### save the data 
saveRDS(m12spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m12-spring-cv.rds"))

# #### M13 ###
# # 
# m13spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"), 
#                      spatial = "on", 
#                      time = "EST_YEAR",
#                      spatiotemporal = "RW", 
#                      control = sdmTMBcontrol(newton_loops = 1), 
#                      fold_ids = clust,
#                      k_folds = length(unique(clust)),
#                      reml = TRUE) 
# 
# # model sanity check 
# # sanity(m13_spring)
# # 
# # # extract the intercepts and coefficients
# # tidy(m13_spring, conf.int = TRUE) 
# # tidy(m13_spring, "ran_pars", conf.int = TRUE)
# 
# ### save the data 
# saveRDS(m13spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m13-spring-cv.rds"))
# 
# 
# #### M14 ###
# # 
# m14spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) + as.factor(AREA),#-1,
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"), 
#                      spatial = "on", 
#                      time = "EST_YEAR",
#                      spatiotemporal = "RW", 
#                      spatial_varying = ~0 + as.factor(AREA), 
#                      control = sdmTMBcontrol(newton_loops = 1), 
#                      fold_ids = clust,
#                      k_folds = length(unique(clust)),
#                      reml = TRUE) 
# # consider omitting spatially varying part of model
# 
# m14spr.cv2 <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)+ as.factor(AREA),
#                       data = sf_spring,
#                       mesh = spring_mesh,
#                       family = tweedie(link = "log"), 
#                       spatial = "on", 
#                       time = "EST_YEAR",
#                       spatiotemporal = "RW", 
#                       #spatial_varying = ~0 + as.factor(scaled_area), 
#                       control = sdmTMBcontrol(newton_loops = 1), 
#                       fold_ids = clust,
#                       k_folds = length(unique(clust)),
#                       reml = TRUE)
# # model sanity check 
# # sanity(m14_spring)
# # sanity(m14_spring2)
# # 
# # # extract the intercepts and coefficients
# # tidy(m14_spring2, conf.int = TRUE) 
# # tidy(m14_spring2, "ran_pars", conf.int = TRUE)
# 
# ### save the data 
# saveRDS(m14spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m14-spring-cv.rds"))
# saveRDS(m14spr.cv2, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m14-spring-cv2.rds"))
# 
# #### M15 ###
# m15spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                        as.factor(EST_YEAR) +
#                        as.factor(AREA),#-1,
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"),
#                      spatial = "on",
#                      time = "EST_YEAR",
#                      spatiotemporal = "IID",
#                      #extra_time = 2020L,
#                      #spatial_varying = ~0 + as.factor(AREA),
#                      control = sdmTMBcontrol(newton_loops = 1),
#                      fold_ids = clust,
#                      k_folds = length(unique(clust)),
#                      reml = TRUE)
# 
# #sanity(m15_spring)
# 
# ### save the data
# saveRDS(m15spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m15-spring-cv.rds"))
# 
# m15a_spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                         #as.factor(EST_YEAR) +
#                         as.factor(AREA),#-1,
#                       data = sf_spring,
#                       mesh = spring_mesh,
#                       family = tweedie(link = "log"),
#                       spatial = "on",
#                       time = "EST_YEAR",
#                       spatiotemporal = "IID",
#                       #extra_time = 2020L,
#                       spatial_varying = ~0 + as.factor(AREA),
#                       control = sdmTMBcontrol(newton_loops = 1),
#                       fold_ids = clust,
#                       k_folds = length(unique(clust)),
#                       reml = TRUE)
# 
# #sanity(m15a_spring)
# 
# ### save the data
# saveRDS(m15a_spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m15-spring-cv2.rds"))
# 
# 
# m15b_spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                         #as.factor(EST_YEAR) +
#                         as.factor(AREA),#-1,
#                       data = sf_spring,
#                       mesh = spring_mesh,
#                       family = tweedie(link = "log"),
#                       spatial = "on",
#                       time = "EST_YEAR",
#                       spatiotemporal = "IID",
#                       #extra_time = 2020L,
#                       #spatial_varying = ~0 + as.factor(AREA),
#                       control = sdmTMBcontrol(newton_loops = 1),
#                       fold_ids = clust,
#                       k_folds = length(unique(clust)),
#                       reml = TRUE)
# #sanity(m15b_spring)
# 
# ### save the data
# saveRDS(m15b_spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m15-spring-cv3.rds"))
# 
# #### M16 ###
# 
# m16spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
#                        as.factor(EST_YEAR) +
#                        as.factor(AREA),#-1,
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"),
#                      spatial = "on",
#                      time = "EST_YEAR",
#                      spatiotemporal = "AR1",
#                      #extra_time = 2020L,
#                      #spatial_varying = ~0 + as.factor(AREA),
#                      control = sdmTMBcontrol(newton_loops = 1),
#                      fold_ids = clust,
#                      k_folds = length(unique(clust)),
#                      reml = TRUE)
# #sanity(m16_spring)
# #saveRDS(m16spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m16-spring-cv.rds"))
# 
# m16a_spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
#                         #as.factor(EST_YEAR) +
#                         as.factor(AREA),#-1,
#                       data = sf_spring,
#                       mesh = spring_mesh,
#                       family = tweedie(link = "log"),
#                       spatial = "on",
#                       time = "EST_YEAR",
#                       spatiotemporal = "AR1",
#                       #extra_time = 2020L,
#                       spatial_varying = ~0 + as.factor(AREA),
#                       control = sdmTMBcontrol(newton_loops = 1),
#                       fold_ids = clust,
#                       k_folds = length(unique(clust)),
#                       reml = TRUE)
# #sanity(m16a_spring)
# 
# # saveRDS(m16a_spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m16-spring-cv2.rds"))
# 
# m16b_spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                         #as.factor(EST_YEAR) +
#                         as.factor(AREA),#-1,
#                       data = sf_spring,
#                       mesh = spring_mesh,
#                       family = tweedie(link = "log"),
#                       spatial = "on",
#                       time = "EST_YEAR",
#                       spatiotemporal = "AR1",
#                       #extra_time = 2020L,
#                       #spatial_varying = ~0 + as.factor(AREA),
#                       control = sdmTMBcontrol(newton_loops = 1),
#                       fold_ids = clust,
#                       k_folds = length(unique(clust)),
#                       reml = TRUE)
# #sanity(m16b_spring)
# 
# #saveRDS(m16b_spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m16-spring-cvg3.rds"))
# 
# #### M17 ###
# 
# m17spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                        as.factor(EST_YEAR) +
#                        as.factor(AREA),#-1,
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"),
#                      spatial = "on",
#                      time = "EST_YEAR",
#                      spatiotemporal = "RW",
#                      #extra_time = 2020L,
#                      #spatial_varying = ~0 + as.factor(AREA),
#                      control = sdmTMBcontrol(newton_loops = 1),
#                      fold_ids = clust,
#                      k_folds = length(unique(clust)),
#                      reml = TRUE)
# # Newton failed to find minimum. NA/NaN gradient evaluation
# #sanity(m17_spring)
# 
# saveRDS(m17spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m17-spring-cv.rds"))
# 
# m17a_spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                         #as.factor(EST_YEAR) +
#                         as.factor(AREA),#-1,
#                       data = sf_spring,
#                       mesh = spring_mesh,
#                       family = tweedie(link = "log"),
#                       spatial = "on",
#                       time = "EST_YEAR",
#                       spatiotemporal = "RW",
#                       #extra_time = 2020L,
#                       spatial_varying = ~0 + as.factor(AREA),
#                       control = sdmTMBcontrol(newton_loops = 1),
#                       fold_ids = clust,
#                       k_folds = length(unique(clust)),
#                       reml = TRUE)
# # Warning messages:
# # 1: In sqrt(diag(cov)) : NaNs produced
# # 2: The model may not have converged: non-positive-definite Hessian matrix.
# 
# saveRDS(m17a_spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m17-spring-cv2.rds"))
# 
# m17b_spr.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                         #as.factor(EST_YEAR) +
#                         as.factor(AREA),#-1,
#                       data = sf_spring,
#                       mesh = spring_mesh,
#                       family = tweedie(link = "log"),
#                       spatial = "on",
#                       time = "EST_YEAR",
#                       spatiotemporal = "RW",
#                       #extra_time = 2020L,
#                       #spatial_varying = ~0 + as.factor(AREA),
#                       control = sdmTMBcontrol(newton_loops = 1),
#                       fold_ids = clust,
#                       k_folds = length(unique(clust)),
#                       reml = TRUE)
# # Warning messages:
# # 1: In sqrt(diag(cov)) : NaNs produced
# # 2: The model may not have converged: non-positive-definite Hessian matrix. 
# 
# #sanity(m17b_spring)
# 
# saveRDS(m17b_spr.cv, file = here("sdmtmb",  "sumflounder", "data", "cross-valid", "m17-spring-cv3.rds"))



