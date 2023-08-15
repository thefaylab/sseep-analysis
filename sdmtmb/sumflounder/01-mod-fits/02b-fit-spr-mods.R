### created: 12/10/2022
### last updated: 03/03/2023

# 02b - FIT SPRING MODELS ####

## OBJECTIVE ####
# fit various forms of models using prepared summer flounder data for the spring season

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
sf_spring <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_spring.rds"))

# mesh 
spring_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mesh.rds"))

## MODEL FITS ####

### No random effect models ####
#### M1 ####
#logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1_spring <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off",
             control = sdmTMBcontrol(newton_loops = 1),
             reml = TRUE, 
             silent = FALSE)

# model sanity check - did it converge
sanity(m1_spring) 

# extract intercepts and coefficients
tidy(m1_spring, conf.int = TRUE) 
tidy(m1_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m1_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m1_spring.rds"))


#### M2 ####
# 
m2_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), 
             reml = TRUE, 
             silent = FALSE)

# model sanity check 
sanity(m2_spring) 

# extract intercepts and coefficients
tidy(m2_spring, conf.int = TRUE) 
tidy(m2_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m2_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m2_spring.rds"))

### Spatial Only Models ####

#### M3 ####
# logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects
m3_spring <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, #revisit =  NA/NaN function evaluation, but converges
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
             reml = TRUE, 
             silent = FALSE)

# model sanity check
sanity(m3_spring)

# extract intercepts and coefficients
tidy(m3_spring, conf.int = TRUE) 
tidy(m3_spring, "ran_pars", conf.int = TRUE)

### save the data
saveRDS(m3_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m3_spring.rds"))

#### M4 ####
# logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and index standardization to estimate a separate intercept for each year
m4_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1), 
             reml = TRUE, 
             silent = FALSE)

# model sanity check 
sanity(m4_spring)

# extract intercepts and coefficients
tidy(m4_spring, conf.int = TRUE) 
tidy(m4_spring, "ran_pars", conf.int = TRUE)

### save the data
saveRDS(m4_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m4_spring.rds"))

### Spatiotemporal Only Model ####
#### M5 ####
# tested a model fit with only spatiotemporal random fields to test dynamics of summer flounder. Coupled with similar spatial SDs; confirms that both spatial and spatiotemporal random fields are needed and summer flounder does not change significantly from year to year. 
m5_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
                    data = sf_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "off", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID", 
                    reml = TRUE, 
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE)

# model sanity check 
sanity(m5_spring)

# extract the intercepts and coefficients
tidy(m5_spring, conf.int = TRUE) 
tidy(m5_spring, "ran_pars", conf.int = TRUE)

### save the data
saveRDS(m5_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m5_spring.rds"))

### IID Models ####
#### M6 ####
# logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and spatiotemporal random fields estimated according to year. Independent correlation, so each year is independent of each other 
m6_spring <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, # revisit = NA/NaN function, but converges
             data = sf_spring,
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "EST_YEAR",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)? 
             control = sdmTMBcontrol(newton_loops = 1), 
             reml = TRUE, 
             silent = FALSE) 

# model sanity check 
sanity(m6_spring)

# extract the intercepts and coefficients
tidy(m6_spring, conf.int = TRUE) 
tidy(m6_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m6_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m6_spring.rds"))

#### M7 ####
#logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and spatiotemporal random fields estimated by year and with a separate intercept for each.  
m7_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1, #estimate separate intercepts by year; point est and uncertainty fed into SAs
             data = sf_spring,
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "EST_YEAR",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)?
             control = sdmTMBcontrol(newton_loops = 1),
             reml = TRUE, 
             silent = FALSE)

#model sanity check 
sanity(m7_spring)

### save the data 
saveRDS(m7_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m7_spring.rds"))

# pull estimates and confidence intervals etc? 
# spring_est <- tidy(m7_spring, conf.int = TRUE) 
# spring_pars <- tidy(m7_spring, "ran_pars", conf.int = TRUE, exponentiate = TRUE)
# 
# m7_spr_est <- bind_rows(spring_est, spring_pars)
# 
# kable(m7_spr_est, align = "lcccc", caption = "M7 spring model estimates", format.args = list(big.mark = ","), booktabs = TRUE) %>%
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14)


#if spatiotemporal Sd is 10x spatial SD then indicates species is more dynamic (and shifting around a lot in space) and may not need spatial random fields

#### M8 ####
# 
m8_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)+ as.factor(AREA),#-1,
                  data = sf_spring,
                  mesh = spring_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID", 
                  spatial_varying = ~0 + as.factor(AREA), 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  reml = TRUE, 
                  silent = FALSE) 

m8_spring2 <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) + as.factor(AREA),
                    data = sf_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID", 
                    #spatial_varying = ~0 + as.factor(scaled_area), 
                    control = sdmTMBcontrol(newton_loops = 1), 
                    reml = TRUE, 
                    silent = FALSE) 

# model sanity check
sanity(m8_spring)
# no convergence, tau_z and sigma_z e

# extract the intercepts and coefficients
tidy(m8_spring, conf.int = TRUE) 
tidy(m8_spring, "ran_pars", conf.int = TRUE)

sanity(m8_spring2)

### save the data 
saveRDS(m8_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m8_spring.rds"))
saveRDS(m8_spring2, file = here("sdmtmb",  "sumflounder", "data", "mods", "m8_spring2.rds"))

### AR1 Models ####
#### M9 ####
# 
m9_spring <- sdmTMB(EXPCATCHWT ~ AVGDEPTH,
                  data = sf_spring,
                  mesh = spring_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "AR1",
                  control = sdmTMBcontrol(nlminb_loops = 1), 
                  reml = TRUE, 
                  silent = FALSE) 

# model sanity check 
sanity(m9_spring)

# extract the intercepts and coefficients
tidy(m9_spring, conf.int = TRUE) 
tidy(m9_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m9_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m9_spring.rds"))

#### M10 ####
# 
m10_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "AR1", 
                   control = sdmTMBcontrol(newton_loops = 1), 
                   reml = TRUE, 
                   silent = FALSE) 

# model sanity check 
sanity(m10_spring)

# extract the intercepts and coefficients
tidy(m10_spring, conf.int = TRUE) 
tidy(m10_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m10_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m10_spring.rds"))

#### M11 ####
# 
# m11_spring <- sdmTMB(EXPCATCHWT ~ s(scaled_depth) + as.factor(scaled_year) + as.factor(scaled_area), #-1,
#                    data = sf_spring,
#                    mesh = spring_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1", 
#                    spatial_varying = ~0 + as.factor(scaled_area), 
#                    control = sdmTMBcontrol(newton_loops = 1),
#                    reml= TRUE, 
#                    silent = FALSE) 
m11_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)+ as.factor(AREA),
                     data = sf_spring,
                     mesh = spring_mesh,
                     family = tweedie(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "AR1", 
                     #spatial_varying = ~0 + as.factor(scaled_area), 
                     control = sdmTMBcontrol(newton_loops = 1), 
                     reml = TRUE, 
                     silent = FALSE) 



# m11a_spring <- sdmTMB(EXPCATCHWT ~ s(scaled_depth) + as.factor(scaled_year) + as.factor(scaled_area)-1,
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"),
#                      spatial = "on",
#                      time = "EST_YEAR",
#                      spatiotemporal = "AR1",
#                      spatial_varying = ~0 + as.factor(scaled_area),
#                      silent = FALSE)

# model sanity check
sanity(m11_spring)
#sanity(m11_spring2)

# extract the intercepts and coefficients
tidy(m11_spring, conf.int = TRUE) 
tidy(m11_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m11_spring, file = here("sdmtmb", "sumflounder", "data", "mods", "m11_spring.rds"))
#saveRDS(m11_spring2, file = here("sdmtmb", "data", "mods", "m11_spring2.rds"))

### RW Models ####
#### M12 ####
# 
m12_spring <- sdmTMB(EXPCATCHWT ~ AVGDEPTH,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "RW", 
                   control = sdmTMBcontrol(nlminb_loops = 1),
                   silent = FALSE, 
                   reml = TRUE) 

# model sanity check 
sanity(m12_spring)

# m12_spring_ex <- run_extra_optimization(m12_spring, nlminb = 1)
# sanity(m12_spring_ex)

# extract the intercepts and coefficients
tidy(m12_spring, conf.int = TRUE) 
tidy(m12_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m12_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m12_spring.rds"))

#### M13 ####
# 
m13_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "RW", 
                   silent = FALSE, 
                   reml = TRUE, 
                   control = sdmTMBcontrol(newton_loops = 1)) 

# model sanity check 
sanity(m13_spring)

# extract the intercepts and coefficients
tidy(m13_spring, conf.int = TRUE) 
tidy(m13_spring, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m13_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m13_spring.rds"))


#### M14 ####
# 
m14_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) + as.factor(AREA),#-1,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "RW", 
                   spatial_varying = ~0 + as.factor(AREA), 
                   silent = FALSE, 
                   reml = TRUE, 
                   control = sdmTMBcontrol(newton_loops = 1)) 
# consider omitting spatially varying part of model

m14_spring2 <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)+ as.factor(AREA),
                      data = sf_spring,
                      mesh = spring_mesh,
                      family = tweedie(link = "log"), 
                      spatial = "on", 
                      time = "EST_YEAR",
                      spatiotemporal = "RW", 
                      #spatial_varying = ~0 + as.factor(scaled_area), 
                      control = sdmTMBcontrol(newton_loops = 1), 
                      reml = TRUE, 
                      silent = FALSE)
# model sanity check 
sanity(m14_spring)
sanity(m14_spring2)

# extract the intercepts and coefficients
tidy(m14_spring2, conf.int = TRUE) 
tidy(m14_spring2, "ran_pars", conf.int = TRUE)

### save the data 
saveRDS(m14_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m14_spring.rds"))
saveRDS(m14_spring2, file = here("sdmtmb",  "sumflounder", "data", "mods", "m14_spring2.rds"))


#### M15 ####
m15_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                     as.factor(EST_YEAR) +
                     as.factor(AREA),#-1,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on",
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   #extra_time = 2020L,
                   #spatial_varying = ~0 + as.factor(AREA),
                   control = sdmTMBcontrol(newton_loops = 1),
                   reml = TRUE, 
                   silent = FALSE)

sanity(m15_spring)

### save the data
saveRDS(m15_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m15_spring.rds"))

m15a_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    #extra_time = 2020L,
                    spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    reml = TRUE, 
                    silent = FALSE)

sanity(m15a_spring)

### save the data
saveRDS(m15a_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m15_spring2.rds"))


m15b_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    #extra_time = 2020L,
                    #spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    reml = TRUE, 
                    silent = FALSE)
sanity(m15b_spring)

### save the data
saveRDS(m15b_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m15_spring3.rds"))

m16_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
                     as.factor(EST_YEAR) +
                     as.factor(AREA),#-1,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on",
                   time = "EST_YEAR",
                   spatiotemporal = "AR1",
                   #extra_time = 2020L,
                   #spatial_varying = ~0 + as.factor(AREA),
                   control = sdmTMBcontrol(newton_loops = 1),
                   reml = TRUE, 
                   silent = FALSE)
sanity(m16_spring)
saveRDS(m16_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m16_spring.rds"))

m16a_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + 
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "AR1",
                    #extra_time = 2020L,
                    spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    reml = TRUE, 
                    silent = FALSE)
sanity(m16a_spring)

saveRDS(m16a_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m16_spring2.rds"))

m16b_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "AR1",
                    #extra_time = 2020L,
                    #spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    reml = TRUE, 
                    silent = FALSE)
sanity(m16b_spring)

saveRDS(m16b_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m16_spring3.rds"))

m17_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                     as.factor(EST_YEAR) +
                     as.factor(AREA),#-1,
                   data = sf_spring,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"),
                   spatial = "on",
                   time = "EST_YEAR",
                   spatiotemporal = "RW",
                   #extra_time = 2020L,
                   #spatial_varying = ~0 + as.factor(AREA),
                   control = sdmTMBcontrol(newton_loops = 1),
                   reml = TRUE, 
                   silent = FALSE)
# Newton failed to find minimum. NA/NaN gradient evaluation
sanity(m17_spring)

saveRDS(m17_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m17_spring.rds"))

m17a_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "RW",
                    #extra_time = 2020L,
                    spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    reml = TRUE, 
                    silent = FALSE)
# Newton failed to find minimum. NA/NaN gradient evaluation

saveRDS(m17a_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m17_spring2.rds"))

m17b_spring <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) +
                      #as.factor(EST_YEAR) +
                      as.factor(AREA),#-1,
                    data = sf_spring,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"),
                    spatial = "on",
                    time = "EST_YEAR",
                    spatiotemporal = "RW",
                    #extra_time = 2020L,
                    #spatial_varying = ~0 + as.factor(AREA),
                    control = sdmTMBcontrol(newton_loops = 1),
                    reml = TRUE, 
                    silent = FALSE)

sanity(m17b_spring)

saveRDS(m17b_spring, file = here("sdmtmb",  "sumflounder", "data", "mods", "m17_spring3.rds"))
