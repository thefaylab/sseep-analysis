### created: 12/10/2022
### last updated: 12/13/2023

# 03b - CROSS VALIDATE SPRING MODELS ####

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
# library(marmap)
# library(raster)
set.seed(120)

here()
### Environment set up ####
# folder location based on season: c("fall", "spring")
season <- "spring"

### File Locations ####
# species folder 
sumflounder.dat <- here("sdmtmb", "sumflounder", "data")

# model locations
cv.dat <- here("sdmtmb", "sumflounder", "data", "cross-valid")

### LOAD DATA ####
# spring summer flounder data created in `01-prepare-data.R`
sf_spring <- readRDS(here(sumflounder.dat, "sumflounder_spring.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# spring mesh created in `01-prepare-data.R`  
spring_mesh <- readRDS(here(sumflounder.dat, "spring_mesh.rds"))


# clust <- sample(1:2, size = nrow(sf_spring), replace = T, prob = c(0.1, 0.9))
# saveRDS(clust, here("sdmtmb", "sumflounder", "data", "cross-valid", "sumflounder-spr-clust.rds"))


## MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)
tic()
### NO RANDOM EFFECTS ####
#### M1 #### 
m1.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5)

### save the data
saveRDS(m1.cv, file = here(cv.dat, season, "m1-cv.rds"))


#### M2 ####
m2.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5) 

### save the data
saveRDS(m2.cv, file = here(cv.dat, season, "m2-cv.rds"))



#### M3 #### 
m3.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1),
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5)

### save the data
saveRDS(m3.cv, file = here(cv.dat, season, "m3-cv.rds"))


#### M4 ####
m4.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5) 

### save the data
saveRDS(m4.cv, file = here(cv.dat, season, "m4-cv.rds"))

### SPATIAL RANDOM EFFECTS ####
#### M5 ####
m5.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", # spatial covariance with depth
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5) 


### save the data
saveRDS(m5.cv, file = here(cv.dat, season, "m5-cv.rds"))



#### M6 ####
m6.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5) 

### save the data
saveRDS(m6.cv, file = here(cv.dat, season, "m6-cv.rds"))


#### M7 #### 
m7.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5) 

### save the data
saveRDS(m7.cv, file = here(cv.dat, season, "m7-cv.rds"))

#### M8 ####
m8.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                      data = sf_spring, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5) 


# m8.cv2 <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) + as.factor(AREA),
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"), 
#                      spatial = "on", 
#                      time = "EST_YEAR",
#                      spatiotemporal = "IID", 
#                      #spatial_varying = ~0 + as.factor(scaled_area), 
#                      control = sdmTMBcontrol(newton_loops = 1), 
#                      # fold_ids = clust,
#                      k_folds = 5,
#                      reml = TRUE) 

# model sanity check
#sanity(m8_spring)
# no convergence, tau_z and sigma_z e

# extract the intercepts and coefficients
# tidy(m8_spring, conf.int = TRUE) 
# tidy(m8_spring, "ran_pars", conf.int = TRUE)

#sanity(m8_spring2)

### save the data 
saveRDS(m8.cv, file = here(cv.dat, season, "m8-cv.rds"))
#saveRDS(m8.cv2, file = here("sdmtmb", "sumflounder", "data", "cross-valid", "m8-spring-cv2.rds"))

### SPATIOTEMPORAL MODELS: IID STRUCTURE ####
#### M9 ####
m9.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                      data = sf_spring,
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID", 
                      control = sdmTMBcontrol(newton_loops = 1),
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5) 


### save the data 
saveRDS(m9.cv, file = here(cv.dat, season, "m9-cv.rds"))

#### M10 ####
m10.cv<- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                      data = sf_spring,
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID", 
                      control = sdmTMBcontrol(newton_loops = 1),
                      silent = FALSE, 
                      # fold_ids = clust,
                      k_folds = 5) 


### save the data 
saveRDS(m10.cv, file = here(cv.dat, season, "m10-cv.rds"))

#### M11 ####
m11.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
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
                       # fold_ids = clust,
                       k_folds = 5) 

# m11.cv2 <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)+ as.factor(AREA),
#                        data = sf_spring,
#                        mesh = spring_mesh,
#                        family = tweedie(link = "log"), 
#                        spatial = "on", 
#                        time = "EST_YEAR",
#                        spatiotemporal = "AR1", 
#                        #spatial_varying = ~0 + as.factor(scaled_area), 
#                        control = sdmTMBcontrol(newton_loops = 1), 
#                        # fold_ids = clust,
#                        k_folds = 5,
#                        reml = TRUE) 

# model sanity check
# sanity(m11_spring)
# #sanity(m11_spring2)
# 
# # extract the intercepts and coefficients
# tidy(m11_spring, conf.int = TRUE) 
# tidy(m11_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m11.cv, file = here(cv.dat, season, "m11-cv.rds"))
#saveRDS(m11.cv2, file = here(cv.dat, season, "m11.cv2.rds"))

#### M12 ####
m12.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
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
                       # fold_ids = clust,
                       k_folds = 5) 


### save the data 
saveRDS(m12.cv, file = here(cv.dat, season, "m12-cv.rds"))

toc()