### created: 12/10/2022
### last updated: 08/02/2023

# 02a - FIT FALL MODELS ####

## OBJECTIVE ####
# fit various forms of models using prepared summer flounder data for the fall season  

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
# summer flounder data 
sf_fall <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds"))

# mesh 
fall_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mesh.rds"))

## MODEL FITS ####

### No random effect models ####
#### M1 ####
# logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1_fall <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             extra_time = 2020L, # add in missing year of survey
             reml = TRUE, 
             silent = FALSE)

# look at intercepts and coefficients 
tidy(m1_fall, conf.int = TRUE) 
tidy(m1_fall, "ran_pars", conf.int = TRUE)
sanity(m1_fall) 

### save the data
saveRDS(m1_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m1_fall.rds"))



#### M2 ####
#no spatial random effects, but 
m2_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1,
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             extra_time = 2020L, # add in missing year of survey
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
             reml = TRUE, 
             silent = FALSE)

# look at intercepts and coefficients 
tidy(m2_fall, conf.int = TRUE) 
tidy(m2_fall, "ran_pars", conf.int = TRUE)
sanity(m2_fall)

# save the data
saveRDS(m2_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m2_fall.rds"))


### Spatial Only Models ####

#### M3 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects
m3_fall <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             extra_time = 2020L, # add in missing year of survey
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with 
             reml = TRUE, 
             silent = FALSE
)
             
# look at intercepts and coefficients 
tidy(m3_fall, conf.int = TRUE) 
tidy(m3_fall, "ran_pars", conf.int = TRUE)
sanity(m3_fall) 

### save the data
saveRDS(m3_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m3_fall.rds"))

#### M4 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m4_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1,
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1), 
             extra_time = 2020L, # add in missing year of survey
             reml = TRUE, 
             silent = FALSE)

# look at intercepts and coefficients 
tidy(m4_fall, conf.int = TRUE) 
tidy(m4_fall, "ran_pars", conf.int = TRUE)
sanity(m4_fall) 

# save the data
saveRDS(m4_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m4_fall.rds"))

### Spatiotemporal Only Model ####
#### M5 ####
# tested a model fit with only spatiotemporal random fields to test dynamics of summer flounder. Coupled with similar spatial Std Devs; confirms that both spatial and spatiotemporal random fields are needed and summer flounder does not change significantly from year to year.

#if spatiotemporal Std Dev is 10x spatial Std Dev then indicates species is more dynamic (and shifting around a lot in space) and may not need spatial random fields
m5_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH),  #+ as.factor(EST_YEAR)-1,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID", 
                  extra_time = 2020L, # add in missing year of survey
                  reml = TRUE, 
                  silent = FALSE) 

sanity(m5_fall)

### save the data
saveRDS(m5_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m5_fall.rds"))

### IID Models ####
#### M6 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated according to EST_YEAR. Independent correlation, so each EST_YEAR is independent of each other 
m6_fall <- sdmTMB(EXPCATCHWT ~ AVGDEPTH,  
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with AVGDEPTH
             time = "EST_YEAR",
             spatiotemporal = "IID", 
             extra_time = 2020L, # add in missing year of survey
             control = sdmTMBcontrol(nlminb_loops = 2), # did not converge with one loop
             reml = TRUE, 
             silent = FALSE)

sanity(m6_fall) 
tidy(m6_fall, conf.int = TRUE) 
tidy(m6_fall, "ran_pars", conf.int = TRUE)

### save the data
saveRDS(m6_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m6_fall.rds"))

#### M7 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each.  
m7_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH), #+ as.factor(EST_YEAR)-1, 
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with AVGDEPTH
             time = "EST_YEAR",
             spatiotemporal = "IID", 
             extra_time = 2020L, # add in missing year of survey
             reml = TRUE, 
             silent = FALSE)

sanity(m7_fall)

### save the data
saveRDS(m7_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m7_fall.rds"))

# # pull estimates and confidence intervals etc? 
# est <- tidy(m7_fall, conf.int = TRUE) 
# pars <- tidy(m7_fall, "ran_pars", conf.int = TRUE, exponentiate = TRUE)
# 
# m7_fall_est <- bind_rows(est, pars)
# 
# kable(m7_fall_est, align = "lcccc", caption = "M7 fall model estimates", format.args = list(big.mark = ","), booktabs = TRUE) %>%
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14)


#### M8 ####
#  logistic regression of summer flounder biomass catch rate as a function of depth and inside or outside wind area. 
m8_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
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
                  reml = TRUE, 
                  silent = FALSE) 

sanity(m8_fall)

m8a_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
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
                  reml = TRUE, 
                  silent = FALSE) 

sanity(m8a_fall)
#`sigma_Z` is smaller than 0.01
#Consider omitting this part of the model

### save the data
saveRDS(m8_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m8_fall.rds"))
saveRDS(m8a_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m8_fall2.rds"))

### AR1 Models ####
#### M9 ####
#  
m9_fall <- sdmTMB(EXPCATCHWT ~ AVGDEPTH,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "AR1",
                  extra_time = 2020L, 
                  control = sdmTMBcontrol(nlminb_loops = 2), 
                  reml = TRUE, 
                  silent = FALSE) 
sanity(m9_fall)

#m9_fall_ex <- run_extra_optimization(m9_fall, nlminb = 1)

#sanity(m9_fall_ex)

### save the data
saveRDS(m9_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m9_fall.rds"))

#### M10 ####
# 
m10_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH), # + as.factor(EST_YEAR)-1,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "AR1", 
                  extra_time = 2020L, 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  reml = TRUE, 
                  silent = FALSE) 
sanity(m10_fall)

### save the data
saveRDS(m10_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m10_fall.rds"))

#### M11 ####
#  revisit no convergence
m11_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
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
                  reml = TRUE, 
                  silent = FALSE) 

sanity(m11_fall)

m11a_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
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
                   reml = TRUE, 
                   silent = FALSE) 

sanity(m11a_fall)

### save the data
saveRDS(m11_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m11_fall.rds"))
saveRDS(m11a_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m11_fall2.rds"))

### RW Models ####
#### M12 ####
# 
m12_fall <- sdmTMB(EXPCATCHWT ~ AVGDEPTH,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "RW", 
                  extra_time = 2020L, 
                  control = sdmTMBcontrol(nlminb_loops = 1), 
                  reml = TRUE, 
                  silent = FALSE) 

sanity(m12_fall)

### save the data
saveRDS(m12_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m12_fall.rds"))

#### M13 ####
#  Newton failed to find minimum; NA/NaN gradient evaluation
# m13_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR),#-1,
#                   data = sf_fall,
#                   mesh = fall_mesh,
#                   family = tweedie(link = "log"), 
#                   spatial = "on", 
#                   time = "EST_YEAR",
#                   spatiotemporal = "RW", 
#                   extra_time = 2020L, 
#                   control = sdmTMBcontrol(nlminb_loops = 2), 
#                   reml = TRUE, 
#                   silent = FALSE) 
# 
# sanity(m13_fall)

# ### save the data
# saveRDS(m13_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m13_fall.rds"))

#### M14 ####
# 
m14_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
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
                  silent = FALSE) 

sanity(m14_fall)

m14a_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
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
                    reml = TRUE, 
                    silent = FALSE)

sanity(m14a_fall)

# save the data 
saveRDS(m14_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m14_fall.rds"))
saveRDS(m14a_fall, file = here("sdmtmb", "sumflounder", "data", "mods", "m14_fall2.rds"))

#### M15 ####
m15_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                   as.factor(EST_YEAR) +
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
                   reml = TRUE, 
                   silent = FALSE)
#  Newton failed to find minimum.  NA/NaN gradient evaluation

m15a_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
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
                   reml = TRUE, 
                   silent = FALSE)
#Warning messages:
# 1: In sqrt(diag(cov)) : NaNs produced
# 2: The model may not have converged: non-positive-definite Hessian matrix. 
sanity(m15a_fall)

### save the data
saveRDS(m15a_fall, file = here("sdmtmb",  "sumflounder", "data", "mods", "m15_fall.rds"))


m15b_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
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
                    reml = TRUE, 
                    silent = FALSE)
sanity(m15b_fall)

### save the data
saveRDS(m15b_fall, file = here("sdmtmb",  "sumflounder", "data", "mods", "m15_fall2.rds"))

m16_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
                   as.factor(EST_YEAR) +
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
                   reml = TRUE, 
                   silent = FALSE)
# Newton failed to find minimum. NA/NaN gradient evaluation

m16a_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
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
                   reml = TRUE, 
                   silent = FALSE)
sanity(m16a_fall)

saveRDS(m16a_fall, file = here("sdmtmb",  "sumflounder", "data", "mods", "m16_fall.rds"))

m16b_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
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
                    reml = TRUE, 
                    silent = FALSE)
sanity(m16b_fall)

saveRDS(m16b_fall, file = here("sdmtmb",  "sumflounder", "data", "mods", "m16_fall2.rds"))

m17_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                   as.factor(EST_YEAR) +
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
                   reml = TRUE, 
                   silent = FALSE)
# Newton failed to find minimum. NA/NaN gradient evaluation

m17a_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
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
                   control = sdmTMBcontrol(newton_loops = 1),
                   reml = TRUE, 
                   silent = FALSE)
# Newton failed to find minimum. NA/NaN gradient evaluation

m17b_fall <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
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
                   reml = TRUE, 
                   silent = FALSE)

sanity(m17b_fall)

saveRDS(m17b_fall, file = here("sdmtmb",  "sumflounder", "data", "mods", "m17_fall.rds"))

# ggplot(sf_fall, aes(EST_YEAR, EXPCATCHWT, colour = AREA_CODE)) +
#   geom_jitter() +
#   scale_colour_viridis_c() +
#   theme_light()



