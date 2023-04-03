### created: 12/10/2022
### last updated: 03/03/2023

#### 02b - FIT SPRING MODELS ####

###################
#### OBJECTIVE ####
###################
# fit various forms of models using prepared summer flounder data  

####################


#### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)

here()


#### LOAD DATA ####
# summer flounder data 
#sumflounder <- readRDS(here("sdmtmb", "data", "sumflounder.rds"))
# sf_fall <- readRDS(here("sdmtmb", "data", "sumflounder_fall.rds"))
sf_spring <- readRDS(here("sdmtmb", "data", "sumflounder_spring.rds"))


# mesh 
spring_mesh <- readRDS(here("sdmtmb", "data", "spring_mesh.rds"))



#### MODEL FITS ####
# M1 - logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1_spring <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off")

sanity(m1_spring) 
tidy(m1_spring, conf.int = TRUE) 
tidy(m1_spring, "ran_pars", conf.int = TRUE)

saveRDS(m1_spring, file = here("sdmtmb", "model-outputs", "m1_spring.rds"))

# M2 - no spatial random effects, but 
m2_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1))

sanity(m2_spring) 
tidy(m2_spring, conf.int = TRUE) 
tidy(m2_spring, "ran_pars", conf.int = TRUE)

saveRDS(m2_spring, file = here("sdmtmb", "model-outputs", "m2_spring.rds"))

# M3 - logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects
m3_spring <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, #revisit =  NA/NaN function evaluation
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1)) #extra optimization to help with convergence

sanity(m3_spring) 
tidy(m3_spring, conf.int = TRUE) 
tidy(m3_spring, "ran_pars", conf.int = TRUE)

saveRDS(m3_spring, file = here("sdmtmb", "model-outputs", "m3_spring.rds"))

# M4 - logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and index standardization to estimate a separate intercept for each year
m4_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1))

sanity(m4_spring)
tidy(m4_spring, conf.int = TRUE) 
tidy(m4_spring, "ran_pars", conf.int = TRUE)

saveRDS(m4_spring, file = here("sdmtmb", "model-outputs", "m4_spring.rds"))

# M5 - logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and spatiotemporal random fields estimated according to year. Independent correlation, so each year is independent of each other 
m5_spring <- sdmTMB(EXPCATCHWT ~ AVGDEPTH, # revisit = NA/NaN function
             data = sf_spring,
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "EST_YEAR",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)? 
             control = sdmTMBcontrol(newton_loops = 1)) 

sanity(m5_spring)
tidy(m5_spring, conf.int = TRUE) 
tidy(m5_spring, "ran_pars", conf.int = TRUE)

saveRDS(m5_spring, file = here("sdmtmb", "model-outputs", "m5_spring.rds"))

# m6 logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and spatiotemporal random fields estimated by year and with a separate intercept for each.  
m6_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1, #estimate separate intercepts by year; point est and uncertainty fed into SAs
             data = sf_spring,
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "EST_YEAR",
             spatiotemporal = "IID") #possbly AR1 (adapt slowly over time)?

sanity(m6_spring)

saveRDS(m6_spring, file = here("sdmtmb", "model-outputs", "m6_spring.rds"))

# pull estimates and confidence intervals etc? 
spring_est <- tidy(m6_spring, conf.int = TRUE) 
spring_pars <- tidy(m6_spring, "ran_pars", conf.int = TRUE, exponentiate = TRUE)

m6_spr_est <- bind_rows(spring_est, spring_pars)

kable(m6_spr_est, align = "lcccc", caption = "M6 spring model estimates", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)


#if spatiotemporal Sd is 10x spatial SD then indicates species is more dynamic (and shifting around a lot in space) and may not need spatial random fields


# m7 
# tested a model fit with only spatiotemporal random fields to test dynamics of summer flounder. Coupled with similar spatial SDs; confirms that both spatial and spatiotemporal random fields are needed and summer flounder does not change significantly from year to year. 
m7_spring <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
             data = sf_spring,
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             time = "EST_YEAR",
             spatiotemporal = "IID") 


#### CONFIGURATIONS TABLE #####
spr.mods.config <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                          "Formula" = c("EXPCATCHWT ~ AVGDEPTH", "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ AVGDEPTH",
                                        "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ AVGDEPTH", 
                                        "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1"), 
                          "Family" = rep(paste("Tweedie(link = log)"), 7), 
                          "Spatial Fields" = c(rep(paste("off"), 2), rep(paste("on"), 4), "off"), 
                          "Time" = c(rep(paste("-"), 4), rep(paste("EST_YEAR"), 3)), 
                          "Spatiotemporal Fields" =  c(rep(paste("-"), 4), rep(paste("IID"), 3)))

# save the data
saveRDS(spr.mods.config, file = here("sdmtmb", "data", "spr-mod-configs.rds"))


kable(spr.mods.config, align = "lcccc", caption = "Spring Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%
  #row_spec(7, color = "red") 

#### DEPTH RELATIONSHIP TABLE #####
spr.dep.rel <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                      "depth_rel" = c(m1_spring$model$par[2], m2_spring$model$par[14], m3_spring$model$par[2], m4_spring$model$par[14],
                                      m5_spring$model$par[2], m6_spring$model$par[14], m7_spring$model$par[14])) 

# save the data
saveRDS(spr.dep.rel, file = here("sdmtmb", "data", "spr-depth-rel.rds"))


spr.dep.rel <- spr.dep.rel %>% rename("Spring Depth Estimates" = depth_rel)

kable(spr.dep.rel, align = "lcccc", caption = "Spring Depth Relationships", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 
