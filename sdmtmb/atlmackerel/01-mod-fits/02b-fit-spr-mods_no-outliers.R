### created: 11/02/2023
### last updated: 12/14/2023

# 02b - FIT SPRING MODELS WITHOUT BIOMASS OUTLIERS ####

## OBJECTIVE ####
# fit various forms of models using prepared atlantic mackerel data for the spring season
# include predictor for index standardization 
# estimate a coefficient for each year 
# include inside/outside wind area designation as part of the predictor
# stock assessment only uses the spring survey 
# removes observations if greater than quantile(EXPCATCHWT, 0.99)

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
# Atlantic mackerel data created in `01-prepare-data.R` 
am_spring_no.out <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring_no-outliers.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# spring mesh created in `01-prepare-data.R` 
spring_mesh_no.out <- readRDS(here("sdmtmb", "atlmackerel", "data", "spring_mesh_no-outliers.rds"))

## TWEEDIE MODEL FITS ####

### No random effect models ####
#### M1 ####
m1_spring_no.out <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                  data = am_spring_no.out, 
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m1_spring_no.out)

# save the data
saveRDS(m1_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m1_spring_no.out.rds"))

#### M2 ####
m2_spring_no.out <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = am_spring_no.out, 
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m2_spring_no.out)

# save the data
saveRDS(m2_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m2_spring_no.out.rds"))

#### M3 ####
m3_spring_no.out <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = am_spring_no.out, 
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m3_spring_no.out)

# save the data
saveRDS(m3_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m3_spring_no.out.rds"))

#### M4 ####
m4_spring_no.out <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                  data = am_spring_no.out, 
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                  silent = FALSE)

sanity(m4_spring_no.out)

# save the data
saveRDS(m4_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m4_spring_no.out.rds"))

### Spatial Only Models ####
#### M5 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m5_spring_no.out <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                  data = am_spring_no.out, 
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"),  
                  spatial = "on", # spatial covariance with depth
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m5_spring_no.out) 

# save the data
saveRDS(m5_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m5_spring_no.out.rds"))

#### M6 ####
m6_spring_no.out <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                  data = am_spring_no.out, 
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m6_spring_no.out) 

# save the data
saveRDS(m6_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m6_spring_no.out.rds"))

#### M7 ####
m7_spring_no.out <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = am_spring_no.out, 
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m7_spring_no.out) 

# save the data
saveRDS(m7_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m7_spring_no.out.rds"))

#### M8 ####
m8_spring_no.out <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                  data = am_spring_no.out, 
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m8_spring_no.out) 

# save the data
saveRDS(m8_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m8_spring_no.out.rds"))


### Spatiotemporal Models - IID Structure ####

#### M9 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m9_spring_no.out <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                  data = am_spring_no.out,
                  mesh = spring_mesh_no.out,
                  family = tweedie(link = "log"), 
                  spatial = "on", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID", 
                  control = sdmTMBcontrol(newton_loops = 1),
                  silent = FALSE)

sanity(m9_spring_no.out)


### save the data
saveRDS(m9_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m9_spring_no.out.rds"))

#### M10 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m10_spring_no.out <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                   data = am_spring_no.out,
                   mesh = spring_mesh_no.out,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID", 
                   control = sdmTMBcontrol(newton_loops = 1),
                   silent = FALSE)

sanity(m10_spring_no.out)

### save the data
saveRDS(m10_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m10_spring_no.out.rds"))


#### M11 ####
# logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m11_spring_no.out <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
                     EST_YEAR + 
                     AREA-1,
                   data = am_spring_no.out,
                   mesh = spring_mesh_no.out,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   control = sdmTMBcontrol(newton_loops = 1), 
                   silent = FALSE) 

sanity(m11_spring_no.out)

### save the data
saveRDS(m11_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m11_spring_no.out.rds"))

#### M12 ####
#  logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m12_spring_no.out <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                     EST_YEAR + 
                     AREA-1,
                   data = am_spring_no.out,
                   mesh = spring_mesh_no.out,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   control = sdmTMBcontrol(newton_loops = 1), 
                   silent = FALSE) 

sanity(m12_spring_no.out)


### save the data
saveRDS(m12_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m12_spring_no.out.rds"))


### Extending Polynomial Relationship ####
# building upon m10 to compare third and forth order polynomial relationships. 
# AIC for m10 shows minimally worse fit than m9 or m11, and slightly better fit than m12.
# third and forth order polynomial might result in better fit than smoother 

#### M13 ####
m13_spring_no.out <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 3) + EST_YEAR - 1,
                     data = am_spring,
                     mesh = spring_mesh,
                     family = tweedie(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID",
                     control = sdmTMBcontrol(newton_loops = 1), 
                     silent = FALSE) 

sanity(m13_spring_no.out)


### save the data
saveRDS(m13_spring_no.out, file = here("sdmtmb", "atlmackerel", "data", "mods", "m13_spring_no.out.rds"))


#### M14 ####
m14_spring_no.out <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                     data = am_spring,
                     mesh = spring_mesh,
                     family = tweedie(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID",
                     control = sdmTMBcontrol(newton_loops = 1), 
                     silent = FALSE) 

sanity(m14_spring_no.out)


### save the data
saveRDS(m14_spring, file = here("sdmtmb", "atlmackerel", "data", "mods", "m14_spring_no.out.rds"))


## DELTA/POISSON/GAMMA MODEL FITS ####

### No random effect models ####
#### M1 ####
m1_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                           data = am_spring_no.out, 
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                           spatial = "off", 
                           control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                           silent = FALSE)

sanity(m1_spring_no.out.dpg)

# save the data
saveRDS(m1_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m1_spring_no.out.dpg.rds"))

#### M2 ####
m2_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                           data = am_spring_no.out, 
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                           spatial = "off", 
                           control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                           silent = FALSE)

sanity(m2_spring_no.out.dpg)

# save the data
saveRDS(m2_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m2_spring_no.out.dpg.rds"))

#### M3 ####
m3_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                           data = am_spring_no.out, 
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                           spatial = "off", 
                           control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                           silent = FALSE)

sanity(m3_spring_no.out.dpg)

# save the data
saveRDS(m3_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m3_spring_no.out.dpg.rds"))

#### M4 ####
m4_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1,
                           data = am_spring_no.out, 
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                           spatial = "off", 
                           control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                           silent = FALSE)

sanity(m4_spring_no.out.dpg)

# save the data
saveRDS(m4_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m4_spring_no.out.dpg.rds"))

### Spatial Only Models ####
#### M5 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m5_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                           data = am_spring_no.out, 
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"),  
                           spatial = "on", # spatial covariance with depth
                           control = sdmTMBcontrol(newton_loops = 1), 
                           silent = FALSE)

sanity(m5_spring_no.out.dpg) 

# save the data
saveRDS(m5_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m5_spring_no.out.dpg.rds"))

#### M6 ####
m6_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                           data = am_spring_no.out, 
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"),  
                           spatial = "on", 
                           control = sdmTMBcontrol(newton_loops = 1), 
                           silent = FALSE)

sanity(m6_spring_no.out.dpg) 

# save the data
saveRDS(m6_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m6_spring_no.out.dpg.rds"))

#### M7 ####
m7_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                           data = am_spring_no.out, 
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"),  
                           spatial = "on", 
                           control = sdmTMBcontrol(newton_loops = 1), 
                           silent = FALSE)

sanity(m7_spring_no.out.dpg) 

# save the data
saveRDS(m7_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m7_spring_no.out.dpg.rds"))

#### M8 ####
m8_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR + AREA - 1,
                           data = am_spring_no.out, 
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"),  
                           spatial = "on", 
                           control = sdmTMBcontrol(newton_loops = 1), 
                           silent = FALSE)

sanity(m8_spring_no.out.dpg) 

# save the data
saveRDS(m8_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m8_spring_no.out.dpg.rds"))


### Spatiotemporal Models - IID Structure ####

#### M9 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m9_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                           data = am_spring_no.out,
                           mesh = spring_mesh_no.out,
                           family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                           spatial = "on", 
                           time = "EST_YEAR",
                           spatiotemporal = "IID", 
                           control = sdmTMBcontrol(newton_loops = 1),
                           silent = FALSE)

sanity(m9_spring_no.out.dpg)


### save the data
saveRDS(m9_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m9_spring_no.out.dpg.rds"))

#### M10 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m10_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR-1, 
                            data = am_spring_no.out,
                            mesh = spring_mesh_no.out,
                            family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                            spatial = "on", 
                            time = "EST_YEAR",
                            spatiotemporal = "IID", 
                            control = sdmTMBcontrol(newton_loops = 1),
                            silent = FALSE)

sanity(m10_spring_no.out.dpg)

### save the data
saveRDS(m10_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m10_spring_no.out.dpg.rds"))


#### M11 ####
# logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m11_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
                              EST_YEAR + 
                              AREA-1,
                            data = am_spring_no.out,
                            mesh = spring_mesh_no.out,
                            family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                            spatial = "on", 
                            time = "EST_YEAR",
                            spatiotemporal = "IID",
                            control = sdmTMBcontrol(newton_loops = 1), 
                            silent = FALSE) 

sanity(m11_spring_no.out.dpg)

### save the data
saveRDS(m11_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m11_spring_no.out.dpg.rds"))

#### M12 ####
#  logistic regression of summer flounder biomass catch rate as a function of depth, year, and inside or outside wind area. 
m12_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                              EST_YEAR + 
                              AREA-1,
                            data = am_spring_no.out,
                            mesh = spring_mesh_no.out,
                            family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                            spatial = "on", 
                            time = "EST_YEAR",
                            spatiotemporal = "IID",
                            control = sdmTMBcontrol(newton_loops = 1), 
                            silent = FALSE) 

sanity(m12_spring_no.out.dpg)


### save the data
saveRDS(m12_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m12_spring_no.out.dpg.rds"))


### Extending Polynomial Relationship ####
# building upon m10 to compare third and forth order polynomial relationships. 
# AIC for m10 shows minimally worse fit than m9 or m11, and slightly better fit than m12.
# third and forth order polynomial might result in better fit than smoother 

#### M13 ####
m13_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 3) + EST_YEAR - 1,
                            data = am_spring,
                            mesh = spring_mesh,
                            family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                            spatial = "on", 
                            time = "EST_YEAR",
                            spatiotemporal = "IID",
                            control = sdmTMBcontrol(newton_loops = 1), 
                            silent = FALSE) 

sanity(m13_spring_no.out.dpg)


### save the data
saveRDS(m13_spring_no.out.dpg, file = here("sdmtmb", "atlmackerel", "data", "mods", "m13_spring_no.out.dpg.rds"))


#### M14 ####
m14_spring_no.out.dpg <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                            data = am_spring,
                            mesh = spring_mesh,
                            family = delta_poisson_link_gamma(link1 = "log", link2 = "log"), 
                            spatial = "on", 
                            time = "EST_YEAR",
                            spatiotemporal = "IID",
                            control = sdmTMBcontrol(newton_loops = 1), 
                            silent = FALSE) 

sanity(m14_spring_no.out)


### save the data
saveRDS(m14_spring, file = here("sdmtmb", "atlmackerel", "data", "mods", "m14_spring_no.out.dpg.rds"))
