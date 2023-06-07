### created: 12/10/2022
### last updated: 04/23/2023

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
sf_fall <- readRDS(here("sdmtmb", "data", "sumflounder_fall.rds"))

# mesh 
fall_mesh <- readRDS(here("sdmtmb", "data", "fall_mesh.rds"))

## MODEL FITS ####

### No random effect models ####
#### M1 ####
# logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1_fall <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             reml = TRUE, 
             silent = FALSE)

# look at intercepts and coefficients 
tidy(m1_fall, conf.int = TRUE) 
tidy(m1_fall, "ran_pars", conf.int = TRUE)
sanity(m1_fall) 

### save the data
saveRDS(m1_fall, file = here("sdmtmb", "model-outputs", "m1_fall.rds"))



#### M2 ####
#no spatial random effects, but 
m2_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), 
             reml = TRUE, 
             silent = FALSE)

saveRDS(m2_fall, file = here("sdmtmb", "model-outputs", "m2_fall.rds"))

tidy(m2_fall, conf.int = TRUE) 
tidy(m2_fall, "ran_pars", conf.int = TRUE)
sanity(m2_fall) 


### Spatial Only Models ####

#### M3 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects
m3_fall <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
             reml = TRUE, 
             silent = FALSE
)
             
saveRDS(m3_fall, file = here("sdmtmb", "model-outputs", "m3_fall.rds"))


tidy(m3_fall, conf.int = TRUE) 
tidy(m3_fall, "ran_pars", conf.int = TRUE)
sanity(m3_fall) 

#### M4 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
m4_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1), 
             reml = TRUE, 
             silent = FALSE)

saveRDS(m4_fall, file = here("sdmtmb", "model-outputs", "m4_fall.rds"))

tidy(m4_fall, conf.int = TRUE) 
tidy(m4_fall, "ran_pars", conf.int = TRUE)
sanity(m4_fall) 

### Spatiotemporal Only Model ####
#### M5 ####
# tested a model fit with only spatiotemporal random fields to test dynamics of summer flounder. Coupled with similar spatial SDs; confirms that both spatial and spatiotemporal random fields are needed and summer flounder does not change significantly from year to year.

#if spatiotemporal Sd is 10x spatial SD then indicates species is more dynamic (and shifting around a lot in space) and may not need spatial random fields
m5_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
                  data = sf_fall,
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  time = "EST_YEAR",
                  spatiotemporal = "IID", 
                  reml = TRUE, 
                  silent = FALSE) 
sanity(m5_fall)

### save the data
saveRDS(m5_fall, file = here("sdmtmb", "model-outputs", "m5_fall.rds"))

### IID Models ####
#### M6 ####
# logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated according to EST_YEAR. Independent correlation, so each EST_YEAR is independent of each other 
m6_fall <- sdmTMB(EXPCATCHWT ~ AVGDEPTH,  
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with AVGDEPTH
             time = "EST_YEAR",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)? 
             control = sdmTMBcontrol(nlminb_loops = 2), # did not converge with one loop
             reml = TRUE, 
             silent = FALSE)

sanity(m6_fall) 
tidy(m6_fall, conf.int = TRUE) 
tidy(m6_fall, "ran_pars", conf.int = TRUE)

### save the data
saveRDS(m6_fall, file = here("sdmtmb", "model-outputs", "m6_fall.rds"))

#### M7 ####
#logistic regression of summer flounder biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each.  
m7_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1, #estimate separate intercepts by EST_YEAR; point est and uncertainty fed into SAs
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with AVGDEPTH
             time = "EST_YEAR",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)?
             reml = TRUE, 
             silent = FALSE)

sanity(m7_fall)

### save the data
saveRDS(m7_fall, file = here("sdmtmb", "model-outputs", "m7_fall.rds"))

# pull estimates and confidence intervals etc? 
est <- tidy(m7_fall, conf.int = TRUE) 
pars <- tidy(m7_fall, "ran_pars", conf.int = TRUE, exponentiate = TRUE)

m7_fall_est <- bind_rows(est, pars)

kable(m7_fall_est, align = "lcccc", caption = "M7 fall model estimates", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)


#### M8 ####
#  
m8_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
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
saveRDS(m8_fall, file = here("sdmtmb", "model-outputs", "m8_fall.rds"))
saveRDS(m8a_fall, file = here("sdmtmb", "model-outputs", "m8_fall2.rds"))

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
saveRDS(m9_fall, file = here("sdmtmb", "model-outputs", "m9_fall.rds"))

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
saveRDS(m10_fall, file = here("sdmtmb", "model-outputs", "m10_fall.rds"))

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
saveRDS(m11_fall, file = here("sdmtmb", "model-outputs", "m11_fall.rds"))
saveRDS(m11a_fall, file = here("sdmtmb", "model-outputs", "m11_fall2.rds"))

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
saveRDS(m12_fall, file = here("sdmtmb", "model-outputs", "m12_fall.rds"))

#### M13 ####
#  
m13_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR),#-1,
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
sanity(m13_fall)

### save the data
saveRDS(m13_fall, file = here("sdmtmb", "model-outputs", "m13_fall.rds"))

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
sanity(m14a_fall)

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
saveRDS(m14a_fall, file = here("sdmtmb", "model-outputs", "m14_fall2.rds"))

#### M15 ####
# m15_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH), #+ 
#                    #as.factor(EST_YEAR) + 
#                    #as.factor(AREA),#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1", 
#                    extra_time = 2020L, 
#                    spatial_varying = ~0 + as.factor(AREA), 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    reml = TRUE)
# m16_fall <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH, by = EST_YEAR) + 
#                    #as.factor(EST_YEAR) + 
#                    as.factor(AREA),#-1,
#                    data = sf_fall,
#                    mesh = fall_mesh,
#                    family = tweedie(link = "log"), 
#                    spatial = "on", 
#                    time = "EST_YEAR",
#                    spatiotemporal = "AR1", 
#                    extra_time = 2020L, 
#                    spatial_varying = ~0 + as.factor(AREA), 
#                    control = sdmTMBcontrol(newton_loops = 1), 
#                    reml = TRUE)

ggplot(sf_fall, aes(EST_YEAR, EXPCATCHWT, colour = AREA_CODE)) +
  geom_jitter() +
  scale_colour_viridis_c() +
  theme_light()


### save the data
saveRDS(m14_fall, file = here("sdmtmb", "model-outputs", "m14_fall.rds"))




### AIC ####
fall.mods <- str_c("m",seq(1:14)) |>
  map(~list(.)) |>
  map(~readRDS(here(sdmtmb.dir, "model-outputs", str_c(., "_fall.rds"))))
fall.aic <- map(fall.mods, ~AIC(.))|> 
  as.data.frame() |> t()
mod.names <- str_c("m",seq(1:14))
 
rownames(fall.aic) <- seq(1:14)
fall.aic <- fall.aic |> 
  bind_cols(mod.names) |>
  rename(AIC = ...1, 
         models = ...2) |> 
  relocate(models, AIC)
m8a_fall <- readRDS(here(sdmtmb.dir, "model-outputs", "m8_fall2.rds"))
m11a_fall <- readRDS(here(sdmtmb.dir, "model-outputs", "m11_fall2.rds"))
m14a_fall <- readRDS(here(sdmtmb.dir, "model-outputs", "m14_fall2.rds"))
m8a <- data.frame(models = "m8b", AIC = AIC(m8a_fall))
m11a <- data.frame(models = "m11b", AIC = AIC(m11a_fall))
m14a<- data.frame(models = "m14b", AIC = AIC(m14a_fall))
fall.aic <- bind_rows(fall.aic, m8a, m11a, m14a) |> mutate(AIC = round(AIC, 2)) |> arrange(desc(AIC))

kable(fall.aic, align = "lcccc", caption = "Model AIC Values", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%
  #row_spec(7, color = "red") 


### CONFIGURATIONS TABLE #####
fall.mods.config <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                          "Formula" = c("EXPCATCHWT ~ AVGDEPTH", "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ AVGDEPTH",
                                        "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ AVGDEPTH", 
                                        "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1"), 
                          "Family" = rep(paste("Tweedie(link = log)"), 7), 
                          "Spatial Fields" = c(rep(paste("off"), 2), rep(paste("on"), 4), "off"), 
                          "Time" = c(rep(paste("-"), 4), rep(paste("EST_YEAR"), 3)), 
                          "Spatiotemporal Fields" =  c(rep(paste("-"), 4), rep(paste("IID"), 3)))

# save the data
saveRDS(fall.mods.config, file = here("sdmtmb", "data", "fall-mod-configs.rds"))


kable(fall.mods.config, align = "lcccc", caption = "Fall Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)# %>%
  #row_spec(7, color = "red") 

#### DEPTH RELATIONSHIP TABLE #####
fall.dep.rel <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                      "depth_rel" = c(m1_fall$model$par[2], m2_fall$model$par[14], m3_fall$model$par[2], m4_fall$model$par[14],
                                      m5_fall$model$par[2], m6_fall$model$par[14], m7_fall$model$par[14])) 

# save the data
saveRDS(fall.dep.rel, file = here("sdmtmb", "data", "fall-depth-rel.rds"))


fall.dep.rel <- fall.dep.rel %>% rename("Depth Estimate" = depth_rel)

kable(fall.dep.rel, align = "lcccc", caption = "Fall Depth Relationships", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 
