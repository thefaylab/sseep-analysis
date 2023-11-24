### created: 11/22/2023
### last updated: 11/24/2023

# 02a - FIT FALL MODELS ####

## OBJECTIVE ####
# fit various forms of models using prepared scup data for the fall season
# include predictor for index standardization 
# estimate a coefficient for each year 
# include inside/outside wind area designation as part of the predictor

## LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)

here()


## LOAD DATA ####
# scup data 
sf_fall <- readRDS(here("sdmtmb", "scup", "data", "scup_fall.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# mesh 
fall_mesh <- readRDS(here("sdmtmb", "scup", "data", "fall_mesh.rds"))

## MODEL FITS ####

### No random effect models ####
#### M1 ####
m1_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
             silent = FALSE)

sanity(m1_fall)

# save the data
saveRDS(m1_fall, file = here("sdmtmb", "scup", "data", "mods", "m1_fall.rds"))

#### M2 ####
m2_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m2_fall)

# save the data
saveRDS(m2_fall, file = here("sdmtmb", "scup", "data", "mods", "m2_fall.rds"))

#### M3 ####
m3_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m3_fall)

# save the data
saveRDS(m3_fall, file = here("sdmtmb", "scup", "data", "mods", "m3_fall.rds"))

#### M4 ####
m4_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m4_fall)

# save the data
saveRDS(m4_fall, file = here("sdmtmb", "scup", "data", "mods", "m4_fall.rds"))

### Spatial Only Models ####
#### M5 ####
# logistic regression of scup biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m5_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", # spatial covariance with depth
             control = sdmTMBcontrol(newton_loops = 1), 
             silent = FALSE)

sanity(m5_fall) 

# save the data
saveRDS(m5_fall, file = here("sdmtmb", "scup", "data", "mods", "m5_fall.rds"))

#### M6 ####
m6_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m6_fall) 

# save the data
saveRDS(m6_fall, file = here("sdmtmb", "scup", "data", "mods", "m6_fall.rds"))

#### M7 ####
m7_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m7_fall) 

# save the data
saveRDS(m7_fall, file = here("sdmtmb", "scup", "data", "mods", "m7_fall.rds"))

#### M8 ####
m8_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                  data = sf_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m8_fall) 

# save the data
saveRDS(m8_fall, file = here("sdmtmb", "scup", "data", "mods", "m8_fall.rds"))


### Spatiotemporal Models - IID Structure ####

#### M9 ####
#logistic regression of scup biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m9_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             time = "EST_YEAR",
             spatiotemporal = "IID", 
             silent = FALSE)

sanity(m9_fall)
sdmTMB::tidy(m9_fall)

### save the data
saveRDS(m9_fall, file = here("sdmtmb", "scup", "data", "mods", "m9_fall.rds"))

#### M10 ####
#logistic regression of scup biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m10_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID", 
                  silent = FALSE)

sanity(m10_fall)

### save the data
saveRDS(m10_fall, file = here("sdmtmb", "scup", "data", "mods", "m10_fall.rds"))


#### M11 ####
# logistic regression of scup biomass catch rate as a function of depth, year, and inside or outside wind area. 
m11_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) +  EST_YEAR +  AREA-1,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID",
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE) 

sanity(m11_fall)

### save the data
saveRDS(m11_fall, file = here("sdmtmb", "scup", "data", "mods", "m11_fall.rds"))

#### M12 ####
#  logistic regression of scup biomass catch rate as a function of depth, year, and inside or outside wind area. 
m12_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA-1,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID",
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE) 

sanity(m12_fall)


### save the data
saveRDS(m12_fall, file = here("sdmtmb", "scup", "data", "mods", "m12_fall.rds"))

### AR1 Models ###

#### M13 ###
# m13_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR-1,
#                   data = sf_fall,
#                   mesh = fall_mesh,
#                   family = tweedie(link = "log"), 
#                   spatial = "on", 
#                   time = "EST_YEAR",
#                   spatiotemporal = "AR1", 
#                   extra_time = extra_time, 
#                   control = sdmTMBcontrol(newton_loops = 1), 
#                   #reml = TRUE, 
#                   silent = FALSE) 
# sanity(m13_fall)
# Error in solve.default(h, g) : 
#   Lapack routine dgesv: system is exactly singular: U[12,12] = 0

### save the data
# saveRDS(m13_fall, file = here("sdmtmb", "scup", "data", "mods", "m13_fall.rds"))

#### M14 ###
# m14_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1", 
#                    extra_time = extra_time, 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    reml = TRUE, 
#                    silent = FALSE) 
# sanity(m14_fall)
# # NaN/NaN evaluation; Newton failed to find minimum. 
# 
# ### save the data
# saveRDS(m14_fall, file = here("sdmtmb", "scup", "data", "mods", "m14_fall.rds"))

#### M15 ###
# m15_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
#                      EST_YEAR + 
#                      AREA-1,
#                   data = sf_fall,
#                   mesh = fall_mesh,
#                   family = tweedie(link = "log"), 
#                   spatial = "on", 
#                   time = "EST_YEAR",
#                   spatiotemporal = "AR1", 
#                   extra_time = extra_time,  
#                   control = sdmTMBcontrol(newton_loops = 1), 
#                   reml = TRUE, 
#                   silent = FALSE) 
# 
# sanity(m15_fall)
# NaN/NaN evaluation; Newton failed to find minimum. 

#### M13 ###
# m13_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
#                      s(EST_YEAR) + 
#                      AREA-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1", 
#                    extra_time = extra_time,  
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    #reml = TRUE, 
#                    silent = FALSE) 
# 
# sanity(m13_fall)
# tidy(m13_fall)
# 
# 
# ### save the data
# saveRDS(m13_fall, file = here("sdmtmb", "scup", "data", "mods", "m13_fall.rds"))

#### M14 ###
# m14_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
#                      EST_YEAR + 
#                      AREA-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1", 
#                    extra_time = extra_time,  
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    #reml = TRUE, 
#                    silent = FALSE) 
# 
# sanity(m14_fall)
# 
# 
# ### save the data
# saveRDS(m14_fall, file = here("sdmtmb", "scup", "data", "mods", "m14_fall.rds"))

#### M14 ###
# m14_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
#                      s(EST_YEAR) + 
#                      AREA-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1", 
#                    extra_time = extra_time,  
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    #reml = TRUE, 
#                    silent = FALSE) 
# 
# sanity(m14_fall)
# tidy(m14_fall)
# 
# 
# ### save the data
# saveRDS(m14_fall, file = here("sdmtmb", "scup", "data", "mods", "m14_fall.rds"))
# 
# #### M15 ###
# m15_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + AREA,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    time_varying = ~1,
#                    spatiotemporal = "AR1", 
#                    extra_time = extra_time,  
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    #reml = TRUE, 
#                    silent = FALSE) 
# 
# sanity(m15_fall)
# tidy(m15_fall)
# 
# ### save the data
# saveRDS(m15_fall, file = here("sdmtmb", "scup", "data", "mods", "m15_fall.rds"))

#### M16 ###
# m16_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + AREA,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    time_varying = ~1,
#                    spatiotemporal = "AR1", 
#                    extra_time = extra_time,  
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    #reml = TRUE, 
#                    silent = FALSE) 
# 
# sanity(m16_fall)
# 
# 
# ### save the data
# saveRDS(m16_fall, file = here("sdmtmb", "scup", "data", "mods", "m16_fall.rds"))

### RW Models ###
#### M17 ###
# m17_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
#                   data = sf_fall,
#                   mesh = fall_mesh,
#                   family = tweedie(link = "log"), 
#                   spatial = "on", 
#                   time = "EST_YEAR",
#                   spatiotemporal = "RW", 
#                   extra_time = extra_time, 
#                   #reml = TRUE, 
#                   control = sdmTMBcontrol(newton_loops = 1), 
#                   silent = FALSE) 
# 
# sanity(m17_fall)
# 
# # save the data 
# saveRDS(m17_fall, file = here("sdmtmb", "scup", "data", "mods", "m17_fall.rds"))

#### M17 ###
# m17_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
#                      EST_YEAR + 
#                      AREA-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW", 
#                    extra_time = extra_time, 
#                    #reml = TRUE, 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    silent = FALSE) 
# 
# sanity(m17_fall)

#### M17 ###
# m17_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
#                      s(EST_YEAR) + 
#                      AREA-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW", 
#                    extra_time = extra_time,  
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    #reml = TRUE, 
#                    silent = FALSE) 
# 
# sanity(m17_fall)
# tidy(m17_fall)
# 
# 
# ### save the data
# saveRDS(m17_fall, file = here("sdmtmb", "scup", "data", "mods", "m17_fall.rds"))


#### M18 ###
# m18_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR - 1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW", 
#                    extra_time = extra_time, 
#                    #reml = TRUE, 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    silent = FALSE) 
# 
# sanity(m18_fall)
# 
# # save the data 
# saveRDS(m18_fall, file = here("sdmtmb", "scup", "data", "mods", "m18_fall.rds"))

# #### M19 ###
# m19_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
#                      EST_YEAR + 
#                      AREA-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW", 
#                    extra_time = extra_time, 
#                    #reml = TRUE, 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    silent = FALSE) 
# 
# sanity(m19_fall)
# 
# # save the data 
# saveRDS(m19_fall, file = here("sdmtmb", "scup", "data", "mods", "m19_fall.rds"))

#### M18 ###
# m18_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
#                      s(EST_YEAR) + 
#                      AREA-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW", 
#                    extra_time = extra_time,  
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    #reml = TRUE, 
#                    silent = FALSE) 
# 
# sanity(m18_fall)
# tidy(m18_fall)
# 
# 
# ### save the data
# saveRDS(m18_fall, file = here("sdmtmb", "scup", "data", "mods", "m18_fall.rds"))

#### M19 ###
# m19_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + AREA,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW", 
#                    extra_time = extra_time, 
#                    #reml = TRUE, 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    silent = FALSE) 
# 
# sanity(m19_fall)
# tidy(m19_fall)
# 
# # save the data 
# saveRDS(m19_fall, file = here("sdmtmb", "scup", "data", "mods", "m19_fall.rds"))

#### M20 ###
# m20_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + AREA,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW", 
#                    extra_time = extra_time, 
#                    #reml = TRUE, 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    silent = FALSE) 
# 
# sanity(m20_fall)
# tidy(m20_fall)
# 
# # save the data 
# saveRDS(m20_fall, file = here("sdmtmb", "scup", "data", "mods", "m20_fall.rds"))





### POLY(DEPTH) Relationship ###
#### M15 - IID ###
# m15_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                    EST_YEAR +
#                    AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "IID",
#                    extra_time = extra_time,
#                    #spatial_varying = ~0 + AREA,
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    reml = TRUE, 
#                    silent = FALSE)
# #  Newton failed to find minimum.  NA/NaN gradient evaluation
# 
# m15a_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                      #EST_YEAR +
#                      AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "IID",
#                    extra_time = extra_time,
#                    spatial_varying = ~0 + AREA,
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    reml = TRUE, 
#                    silent = FALSE)
# #Warning messages:
# # 1: In sqrt(diag(cov)) : NaNs produced
# # 2: The model may not have converged: non-positive-definite Hessian matrix. 
# sanity(m15a_fall)
# 
# ### save the data
# saveRDS(m15a_fall, file = here("sdmtmb",  "scup", "data", "mods", "m15_fall.rds"))
# 
# 
# m15b_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                       #EST_YEAR +
#                       AREA,#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"),
#                     spatial = "on",
#                     time = "EST_YEAR",
#                     spatiotemporal = "IID",
#                     extra_time = extra_time,
#                     #spatial_varying = ~0 + AREA,
#                     control = sdmTMBcontrol(newton_loops = 1),
#                     reml = TRUE, 
#                     silent = FALSE)
# sanity(m15b_fall)
# 
# ### save the data
# saveRDS(m15b_fall, file = here("sdmtmb",  "scup", "data", "mods", "m15_fall2.rds"))
# 
# #### M16 - AR1 ###
# m16_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
#                    EST_YEAR +
#                    AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1",
#                    extra_time = extra_time,
#                    #spatial_varying = ~0 + AREA,
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    reml = TRUE, 
#                    silent = FALSE)
# # Newton failed to find minimum. NA/NaN gradient evaluation
# 
# m16a_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
#                      #EST_YEAR +
#                      AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1",
#                    extra_time = extra_time,
#                    spatial_varying = ~0 + AREA,
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    reml = TRUE, 
#                    silent = FALSE)
# sanity(m16a_fall)
# 
# saveRDS(m16a_fall, file = here("sdmtmb",  "scup", "data", "mods", "m16_fall.rds"))
# 
# m16b_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                       #EST_YEAR +
#                       AREA,#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"),
#                     spatial = "on",
#                     time = "EST_YEAR",
#                     spatiotemporal = "AR1",
#                     extra_time = extra_time,
#                     #spatial_varying = ~0 + AREA,
#                     control = sdmTMBcontrol(newton_loops = 1),
#                     reml = TRUE, 
#                     silent = FALSE)
# sanity(m16b_fall)
# 
# saveRDS(m16b_fall, file = here("sdmtmb",  "scup", "data", "mods", "m16_fall2.rds"))
# 
# #### M17 - RW ###
# m17_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                    EST_YEAR +
#                    AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW",
#                    extra_time = extra_time,
#                    #spatial_varying = ~0 + AREA,
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    reml = TRUE, 
#                    silent = FALSE)
# # Newton failed to find minimum. NA/NaN gradient evaluation
# 
# m17a_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                      #EST_YEAR +
#                      AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW",
#                    extra_time = extra_time,
#                    spatial_varying = ~0 + AREA,
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    reml = TRUE, 
#                    silent = FALSE)
# # Newton failed to find minimum. NA/NaN gradient evaluation
# 
# m17b_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
#                      #EST_YEAR +
#                      AREA,#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"),
#                    spatial = "on",
#                    time = "EST_YEAR",
#                    spatiotemporal = "RW",
#                    extra_time = extra_time,
#                    #spatial_varying = ~0 + AREA,
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    reml = TRUE, 
#                    silent = FALSE)
# 
# sanity(m17b_fall)
# 
# saveRDS(m17b_fall, file = here("sdmtmb",  "scup", "data", "mods", "m17_fall.rds"))





