### created: 12/10/2022
### last updated: 03/03/2023

#### 03a - COMPARE FALL MODELS ####

###################
#### OBJECTIVE ####
###################
# compare and cross validate models fit to historical summer flounder data  

####################

#### LOAD PACKAGES ####
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

here()

#### LOAD DATA ####
# models 
m1_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m1_fall.rds"))
m2_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m2_fall.rds"))
m3_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m3_fall.rds"))
m4_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m4_fall.rds"))
m5_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m5_fall.rds"))
m6_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m6_fall.rds"))
m7_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m7_fall.rds"))

sf_fall <- readRDS(here("sdmtmb", "data", "sumflounder_fall.rds"))
fall_mesh <- readRDS(here("sdmtmb", "data", "fall_mesh.rds"))


#### MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)


# M1 - logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1))

# save the data
saveRDS(m1fall.cv, file = here("sdmtmb", "model-outputs", "m1-fall-cv.rds"))


# M2 - no spatial random effects, but 
m2fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1)) 

# save the data
saveRDS(m2fall.cv, file = here("sdmtmb", "model-outputs", "m2-fall-cv.rds"))



# M3 logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects
m3fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1))  #extra optimization to help with convergence 

# save the data
saveRDS(m3fall.cv, file = here("sdmtmb", "model-outputs", "m3-fall-cv.rds"))


# M4 
m4fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1)) 

# save the data
saveRDS(m4fall.cv, file = here("sdmtmb", "model-outputs", "m4-fall-cv.rds"))


# m5
m5fall.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with AVGDEPTH
             time = "EST_YEAR",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)? 
             control = sdmTMBcontrol(newton_loops = 1)) 

# save the data
saveRDS(m5fall.cv, file = here("sdmtmb", "model-outputs", "m5-fall-cv.rds"))



# M6 
m6fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1, 
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "EST_YEAR",
             spatiotemporal = "IID") #possbly AR1 (adapt slowly over time)?

# save the data
saveRDS(m6fall.cv, file = here("sdmtmb", "model-outputs", "m6-fall-cv.rds"))


# m7 
m7fall.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
             data = sf_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             time = "EST_YEAR",
             spatiotemporal = "IID") 

# save the data
saveRDS(m7fall.cv, file = here("sdmtmb", "model-outputs", "m7-fall-cv.rds"))


#### COMPARISON TABLE ####

fall.mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                   "AIC" = c(AIC(m1_fall), AIC(m2_fall), AIC(m3_fall), AIC(m4_fall), AIC(m5_fall), AIC(m6_fall), AIC(m7_fall)),#)


#fall.mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                   "TOTAL_ELPD" = c(m1fall.cv$elpd, m2fall.cv$elpd, m3fall.cv$elpd, m4fall.cv$elpd, m5fall.cv$elpd, m6fall.cv$elpd, m7fall.cv$elpd), 
                   "TOTAL_LOGLIK" = c(m1fall.cv$sum_loglik, m2fall.cv$sum_loglik, m3fall.cv$sum_loglik, m4fall.cv$sum_loglik, m5fall.cv$sum_loglik, m6fall.cv$sum_loglik, m7fall.cv$sum_loglik)) #%>% 
  #right_join(lamods, by = "models")

# save the data
saveRDS(fall.mods, file = here("sdmtmb", "model-outputs", "fall-mod-diagnostics.rds"))


# format the table for presentation
fall_mods_tbl <- fall.mods %>% 
  mutate(TOTAL_ELPD = round(TOTAL_ELPD, 2),
         TOTAL_LOGLIK = round(TOTAL_LOGLIK, 2)) %>% 
  rename(" " = models, 
         "Total Expected Log Predictive Density" = TOTAL_ELPD,
         "Total Log Likelihood" = TOTAL_LOGLIK)%>% 
  arrange(desc(AIC))

kable(fall_mods_tbl[,c(1, 4, 2:3)], align = "lcccc", caption = "Fall Model Diagnostic Values", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 
  
# how to save the table 





