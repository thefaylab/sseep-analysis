### created: 11/06/2023
### last updated: 12/13/2023

# 03b - CROSS VALIDATE SPRING MODELS: NO BIOMASS OUTLIERS ####

## OBJECTIVE ####
# cross validate models fit to historical Atlantic mackerel data  without outliers

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
atlmack_no.out <-  readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring_no-outliers.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# mesh 
spring_mesh_no.out <- readRDS(here("sdmtmb", "atlmackerel", "data", "spring_mesh_no-outliers.rds"))
clust <- sample(1:2, size = nrow(atlmack_no.out), replace = T, prob = c(0.1, 0.9))
saveRDS(clust, here("sdmtmb", "atlmackerel", "data", "cross-valid", "atlmack-no.out-clust.rds"))


## TWEEDIE MODELS ####
# library(future)
# plan(multisession)

### No Random Effects Models ####
#### M1 ####
#
m1.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                   data = atlmack_no.out, 
                   mesh = spring_mesh_no.out,
                   family = tweedie(link = "log"), 
                   spatial = "off", 
                   control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                   silent = FALSE, 
                   fold_ids = clust,
                   k_folds = length(unique(clust)))

# save the data
saveRDS(m1.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m1-cv_no.out.rds"))


#### M2 ####
#
m2.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                   data = atlmack_no.out, 
                   mesh = spring_mesh_no.out,
                   family = tweedie(link = "log"), 
                   spatial = "off", 
                   control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                   silent = FALSE, 
                   fold_ids = clust,
                   k_folds = length(unique(clust))) 

# save the data
saveRDS(m2.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m2-cv_no.out.rds"))



#### M3 ####
#
m3.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                      data = atlmack_no.out, 
                      mesh = spring_mesh_no.out,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust)))


# save the data
saveRDS(m3.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m3-cv_no.out.rds"))


#### M4 ####
#
m4.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                      data = atlmack_no.out, 
                      mesh = spring_mesh_no.out,
                      family = tweedie(link = "log"), 
                      spatial = "off", 
                      control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

# save the data
saveRDS(m4.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m4-cv_no.out.rds"))


### Spatial Only Models ####
#### M5 ####
#
m5.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                      data = atlmack_no.out, 
                      mesh = spring_mesh_no.out,
                      family = tweedie(link = "log"),  
                      spatial = "on", # spatial covariance with depth
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

# save the data
saveRDS(m5.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m5-cv_no.out.rds"))



#### M6 ####
#
m6.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                      data = atlmack_no.out, 
                      mesh = spring_mesh_no.out,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

# save the data
saveRDS(m6.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m6-cv_no.out.rds"))


#### M7 ####
#
m7.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                      data = atlmack_no.out, 
                      mesh = spring_mesh_no.out,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE,
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

# save the data
saveRDS(m7.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m7-cv_no.out.rds"))

#### M8 ####
# 
m8.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                      data = atlmack_no.out, 
                      mesh = spring_mesh_no.out,
                      family = tweedie(link = "log"),  
                      spatial = "on", 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 

### save the data 
saveRDS(m8.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m8-cv_no.out.rds"))


### Spatiotemporal Models - IID Structure ####
#### M9 ####
# 
m9.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                      data = atlmack_no.out,
                      mesh = spring_mesh_no.out,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID", 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust))) 


### save the data 
saveRDS(m9.cv_no.out, file = here("sdmtmb",  "atlmackerel", "data", "cross-valid", "m9-cv_no.out.rds"))

#### M10 ####
# 
m10.cv_no.out<- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                      data = atlmack_no.out,
                      mesh = spring_mesh_no.out,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "IID", 
                      silent = FALSE, 
                      fold_ids = clust,
                      k_folds = length(unique(clust)))


### save the data 
saveRDS(m10.cv_no.out, file = here("sdmtmb",  "atlmackerel", "data", "cross-valid", "m10-cv_no.out.rds"))

#### M11 ####
# 
m11.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA-1,
                       data = atlmack_no.out,
                       mesh = spring_mesh_no.out,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 


### save the data 
saveRDS(m11.cv_no.out, file = here("sdmtmb", "atlmackerel", "data", "cross-valid", "m11-cv_no.out.rds"))

#### M12 ####
# 
m12.cv_no.out <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                         EST_YEAR + 
                         AREA-1,
                       data = atlmack_no.out,
                       mesh = spring_mesh_no.out,
                       family = tweedie(link = "log"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE,
                       fold_ids = clust,
                       k_folds = length(unique(clust))) 


### save the data 
saveRDS(m12.cv_no.out, file = here("sdmtmb",  "atlmackerel", "data", "cross-valid", "m12-cv_no.out.rds"))



## DELTA/POISSON/GAMMA MODEL FITS ####

### No random effect models ####
#### M1 ####
m1.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                               data = atlmack_no.out, 
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(), 
                               spatial = "off", 
                               control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                               silent = FALSE)


# save the data
saveRDS(m1.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m1-cv-dpg_no.out.rds"))

#### M2 ####
m2.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                               data = atlmack_no.out, 
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(), 
                               spatial = "off", 
                               control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                               silent = FALSE)


# save the data
saveRDS(m2.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m2-cv-dpg_no.out.rds"))

#### M3 ####
m3.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                               data = atlmack_no.out, 
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(), 
                               spatial = "off", 
                               control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                               silent = FALSE)


# save the data
saveRDS(m3.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m3-cv-dpg_no.out.rds"))

#### M4 ####
m4.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                               data = atlmack_no.out, 
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(), 
                               spatial = "off", 
                               control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                               silent = FALSE)


# save the data
saveRDS(m4.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m4-cv-dpg_no.out.rds"))

### Spatial Only Models ####
#### M5 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m5.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                               data = atlmack_no.out, 
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(),  
                               spatial = "on", # spatial covariance with depth
                               control = sdmTMBcontrol(newton_loops = 1), 
                               silent = FALSE)



# save the data
saveRDS(m5.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m5-cv-dpg_no.out.rds"))

#### M6 ####
m6.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                               data = atlmack_no.out, 
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(),  
                               spatial = "on", 
                               control = sdmTMBcontrol(newton_loops = 1), 
                               silent = FALSE)



# save the data
saveRDS(m6.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m6-cv-dpg_no.out.rds"))

#### M7 ####
m7.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                               data = atlmack_no.out, 
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(),  
                               spatial = "on", 
                               control = sdmTMBcontrol(newton_loops = 1), 
                               silent = FALSE)



# save the data
saveRDS(m7.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m7-cv-dpg_no.out.rds"))

#### M8 ####
m8.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                               data = atlmack_no.out, 
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(),  
                               spatial = "on", 
                               control = sdmTMBcontrol(newton_loops = 1), 
                               silent = FALSE)


# save the data
saveRDS(m8.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m8-cv-dpg_no.out.rds"))


### Spatiotemporal Models - IID Structure ####

#### M9 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m9.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                               data = atlmack_no.out,
                               mesh = spring_mesh_no.out,
                               family = delta_poisson_link_gamma(), 
                               spatial = "on", 
                               time = "EST_YEAR",
                               spatiotemporal = "IID", 
                               control = sdmTMBcontrol(newton_loops = 1),
                               silent = FALSE)




### save the data
saveRDS(m9.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m9-cv-dpg_no.out.rds"))

#### M10 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m10.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                                data = atlmack_no.out,
                                mesh = spring_mesh_no.out,
                                family = delta_poisson_link_gamma(), 
                                spatial = "on", 
                                time = "EST_YEAR",
                                spatiotemporal = "IID", 
                                control = sdmTMBcontrol(newton_loops = 1),
                                silent = FALSE)



### save the data
saveRDS(m10.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m10-cv-dpg_no.out.rds"))


#### M11 ####
# logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m11.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + 
                                  EST_YEAR + 
                                  AREA-1,
                                data = atlmack_no.out,
                                mesh = spring_mesh_no.out,
                                family = delta_poisson_link_gamma(), 
                                spatial = "on", 
                                time = "EST_YEAR",
                                spatiotemporal = "IID",
                                control = sdmTMBcontrol(newton_loops = 1), 
                                silent = FALSE) 



### save the data
saveRDS(m11.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m11-cv-dpg_no.out.rds"))

#### M12 ####
#  logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m12.cv_no.out.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                                  EST_YEAR + 
                                  AREA-1,
                                data = atlmack_no.out,
                                mesh = spring_mesh_no.out,
                                family = delta_poisson_link_gamma(), 
                                spatial = "on", 
                                time = "EST_YEAR",
                                spatiotemporal = "IID",
                                control = sdmTMBcontrol(newton_loops = 1), 
                                silent = FALSE) 


### save the data
saveRDS(m12.cv_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m12-cv-dpg_no.out.rds"))
