### created: 11/06/2023
### last updated: 

# 03 - CROSS VALIDATE SPRING MODELS ####

## OBJECTIVE ####
# cross validate models fit to historical Atlantic mackerel data  

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

### DATA ####
atlmack <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))
spring_mesh <- readRDS(here("sdmtmb", "atlmackerel", "data", "spring_mesh.rds"))
clust <- sample(1:2, size = nrow(atlmack), replace = T, prob = c(0.1, 0.9))


## MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)

### No Random Effects Models ####
#### M1 ####
#
m1.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                   data = atlmack, 
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "off", 
                   control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                   silent = FALSE, 
                   fold_ids = clust,
                   k_folds = length(unique(clust)))

# save the data
saveRDS(m1.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m1-cv.rds"))


#### M2 ####
#
m2.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                   data = atlmack, 
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "off", 
                   control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                   silent = FALSE, 
                   fold_ids = clust,
                   k_folds = length(unique(clust))) 

# save the data
saveRDS(m2.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m2-cv.rds"))



#### M3 ####
#
m3.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                      data = atlmack, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust)))


# save the data
saveRDS(m3.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m3-cv.rds"))


#### M4 ####
#
m4.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                      data = atlmack, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

# save the data
saveRDS(m4.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m4-cv.rds"))


### Spatial Only Models ####
#### M5 ####
#
m5.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                      data = atlmack, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", # spatial covariance with depth
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

# save the data
saveRDS(m5.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m5-cv.rds"))



#### M6 ####
#
m6.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                      data = atlmack, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

# save the data
saveRDS(m6.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m6-cv.rds"))


#### M7 ####
#
m7.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                      data = atlmack, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE,
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

# save the data
saveRDS(m7.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m7-cv.rds"))

#### M8 ####
# 
m8.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                      data = atlmack, 
                      mesh = spring_mesh,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

### save the data 
saveRDS(m8.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m8-cv.rds"))


### Spatiotemporal Models - IID Structure ####
#### M9 ####
# 
m9.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                      data = atlmack,
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID", 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 


### save the data 
saveRDS(m9.cv, file = here("sdmtmb",  "atlmackerel", "data", "cross-valid", "m9-cv.rds"))

#### M10 ####
# 
m10.cv<- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                      data = atlmack,
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID", 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust)))


### save the data 
saveRDS(m10.cv, file = here("sdmtmb",  "atlmackerel", "data", "cross-valid", "m10-cv.rds"))

#### M11 ####
# 
m11.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA-1,
                       data = atlmack,
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
saveRDS(m11.cv, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m11-cv.rds"))

#### M12 ####
# 
m12.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                         EST_YEAR + 
                         AREA-1,
                       data = atlmack,
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
saveRDS(m12.cv, file = here("sdmtmb",  "atlmackerel", "data", "cross-valid", "m12-cv.rds"))

