### created: 11/06/2023
### last updated: 05/28/2024

# 03a - CROSS VALIDATE SPRING MODELS ####

## OBJECTIVE ####
# cross validate models fit to historical Atlantic mackerel data  
# runs approx 30 hours 

### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(kableExtra)
library(tictoc)

set.seed(120)

here()

### Environment set up #### 
# folder location based on data set used: c("all-dat", "no-bio-out", "no-dep-out")
dat_set <- "all-dat"

# # data description: c("All observations", "Biomass outliers removed", "Depth outliers removed")
# data_desc <- "All observations"
# 
# # model family folder: c("tweedie", "dpg")
# fam_file <- "tweedie"
# 
# # model family description: c("Tweedie", "Delta Gamma")
# fam_desc <- "Tweedie"

### File Locations ####
atlmack.dat <- here("sdmtmb", "atlmackerel", "data")

tweedie.cvs <- here("sdmtmb", "atlmackerel", "data", "cross-valid", "tweedie", dat_set)

dpg.cvs <- here("sdmtmb", "atlmackerel", "data", "cross-valid", "dpg", dat_set)



### DATA ####
atlmack <- readRDS(here(atlmack.dat, "atlmackerel_spring.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

spring_mesh <- readRDS(here(atlmack.dat, "spring_mesh.rds"))


## TWEEDIE CROSS VALIDATIONS ####
# library(future)
# plan(multicore, workers = 4)

### No Random Effects Models ####
#### M1 ####
m1.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "off",
                   control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                   silent = FALSE,
                   k_folds = 10)

# save the data
saveRDS(m1.cv, file = here(tweedie.cvs, "m1-cv.rds"))


#### M2 ####
m2.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "off",
                   control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                   silent = FALSE,
                   k_folds = 10)

# save the data
saveRDS(m2.cv, file = here(tweedie.cvs, "m2-cv.rds"))



#### M3 ####
m3.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "off",
                   control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                   silent = FALSE,
                   k_folds = 10)


# save the data
saveRDS(m3.cv, file = here(tweedie.cvs, "m3-cv.rds"))


#### M4 ####
m4.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "off",
                   control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                   silent = FALSE,
                   k_folds = 10)

# save the data
saveRDS(m4.cv, file = here(tweedie.cvs, "m4-cv.rds"))


### Spatial Only Models ####
#### M5 ####
m5.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on", # spatial covariance with depth
                   control = sdmTMBcontrol(newton_loops = 1),
                   silent = FALSE,
                   k_folds = 10)

# save the data
saveRDS(m5.cv, file = here(tweedie.cvs, "m5-cv.rds"))

#### M6 ####
m6.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on",
                   control = sdmTMBcontrol(newton_loops = 1),
                   silent = FALSE,
                   k_folds = 10)

# save the data
saveRDS(m6.cv, file = here(tweedie.cvs, "m6-cv.rds"))

#### M7 ####
m7.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on",
                   control = sdmTMBcontrol(newton_loops = 1),
                   silent = FALSE,
                   k_folds = 10)

# save the data
saveRDS(m7.cv, file = here(tweedie.cvs, "m7-cv.rds"))

#### M8 ####
m8.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on",
                   control = sdmTMBcontrol(newton_loops = 1),
                   silent = FALSE,
                   k_folds = 10)

### save the data
saveRDS(m8.cv, file = here(tweedie.cvs, "m8-cv.rds"))


### Spatiotemporal Models - IID Structure ####
#### M9 ####
m9.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on",
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   silent = FALSE,
                   k_folds = 10)


### save the data
saveRDS(m9.cv, file = here(tweedie.cvs, "m9-cv.rds"))

#### M10 ####
m10.cv<- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on",
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   silent = FALSE,
                   k_folds = 10)


### save the data
saveRDS(m10.cv, file = here(tweedie.cvs, "m10-cv.rds"))

#### M11 ####
m11.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA-1,
                    data = atlmack,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE,
                    k_folds = 10) 


### save the data 
saveRDS(m11.cv, file = here(tweedie.cvs, "m11-cv.rds"))

#### M12 ####
m12.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA-1,
                    data = atlmack,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE,
                    k_folds = 10) 


### save the data 
saveRDS(m12.cv, file = here(tweedie.cvs, "m12-cv.rds"))

#### M13 ####
m13.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 3) + EST_YEAR - 1,
                    data = atlmack,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE,
                    k_folds = 10) 
# attempting to improve convergence with optimHessWarning messages:
# 1: In sqrt(diag(cov)) : NaNs produced
# 2: The model may not have converged: non-positive-definite Hessian matrix. 

### save the data
saveRDS(m13.cv, file = here(tweedie.cvs, "m13-cv.rds"))

#### M14 ####
m14.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                    data = atlmack,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE,
                    k_folds = 10) 


### save the data
saveRDS(m14.cv, file = here(tweedie.cvs, "m14-cv.rds"))


## DELTA/POISSON/GAMMA CROSS VALIDATIONS ####
### No random effect cross validations ####
#### M1 ####
m1.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "off",
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       silent = FALSE,
                       k_folds = 10)

# save the data
saveRDS(m1.cv.dpg, file = here(dpg.cvs, "m1-cv.rds"))

#### M2 ####
m2.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "off",
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       silent = FALSE,
                       k_folds = 10)

# save the data
saveRDS(m2.cv.dpg, file = here(dpg.cvs, "m2-cv.rds"))

#### M3 ####
m3.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "off",
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       silent = FALSE,
                       k_folds = 10)

# save the data
saveRDS(m3.cv.dpg, file = here(dpg.cvs, "m3-cv.rds"))

#### M4 ####
m4.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "off",
                       control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                       silent = FALSE,
                       k_folds = 10)

# save the data
saveRDS(m4.cv.dpg, file = here(dpg.cvs, "m4-cv.rds"))

### Spatial Only Cross Validations ####
#### M5 ####
# logistic regression of Atlantic mackerel biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m5.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "on", # spatial covariance with depth
                       control = sdmTMBcontrol(newton_loops = 1),
                       silent = FALSE,
                       k_folds = 10)

# save the data
saveRDS(m5.cv.dpg, file = here(dpg.cvs, "m5-cv.rds"))

#### M6 ####
m6.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "on",
                       control = sdmTMBcontrol(newton_loops = 1),
                       silent = FALSE,
                       k_folds = 10)


# save the data
saveRDS(m6.cv.dpg, file = here(dpg.cvs, "m6-cv.rds"))

#### M7 ####
m7.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "on",
                       control = sdmTMBcontrol(newton_loops = 1),
                       silent = FALSE,
                       k_folds = 10)


# save the data
saveRDS(m7.cv.dpg, file = here(dpg.cvs, "m7-cv.rds"))

#### M8 ####
m8.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "on",
                       control = sdmTMBcontrol(newton_loops = 1),
                       silent = FALSE,
                       k_folds = 10)


# save the data
saveRDS(m8.cv.dpg, file = here(dpg.cvs, "m8-cv.rds"))


### Spatiotemporal Cross Validations - IID Structure ####

#### M9 ####
#logistic regression of Atlantic mackerel biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each.
m9.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                       spatial = "on",
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       control = sdmTMBcontrol(newton_loops = 1),
                       silent = FALSE,
                       k_folds = 10)


### save the data
saveRDS(m9.cv.dpg, file = here(dpg.cvs, "m9-cv.rds"))

#### M10 ####
#logistic regression of Atlantic mackerel biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each.
m10.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                        spatial = "on",
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1),
                        silent = FALSE,
                        k_folds = 10)


### save the data
saveRDS(m10.cv.dpg, file = here(dpg.cvs, "m10-cv.rds"))


#### M11 ####
# logistic regression of Atlantic mackerel biomass catch rate as a function of depth, year, and inside or outside wind area.
m11.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA-1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                        spatial = "on",
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1),
                        silent = FALSE,
                        k_folds = 10)


### save the data
saveRDS(m11.cv.dpg, file = here(dpg.cvs, "m11-cv.rds"))

#### M12 ####
#  logistic regression of Atlantic mackerel biomass catch rate as a function of depth, year, and inside or outside wind area.
m12.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA-1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                        spatial = "on",
                        time = "EST_YEAR",
                        patiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1),
                        silent = FALSE,
                        k_folds = 10)




### save the data
saveRDS(m12.cv.dpg, file = here(dpg.cvs, "m12-cv.rds"))

#### M13 ####
m13.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 3) + EST_YEAR -1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                        spatial = "on",
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1),
                        silent = FALSE,
                        k_folds = 10)




### save the data
saveRDS(m13.cv.dpg, file = here(dpg.cvs, "m13-cv.rds"))

#### M14 ####
m14.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR -1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"),
                        spatial = "on",
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1),
                        silent = FALSE,
                        k_folds = 10)

### save the data
saveRDS(m14.cv.dpg, file = here(dpg.cvs, "m14-cv.rds"))
