### created: 12/10/2022
### last updated: 03/03/2023

#### 03b - COMPARE SPRING MODELS ####

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
m1_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m1_spring.rds"))
m2_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m2_spring.rds"))
m3_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m3_spring.rds"))
m4_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m4_spring.rds"))
m5_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m5_spring.rds"))
m6_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m6_spring.rds"))
m7_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m7_spring.rds"))

sf_spring <- readRDS(here("sdmtmb", "data", "sumflounder_spring.rds"))
spring_mesh <- readRDS(here("sdmtmb", "data", "spring_mesh.rds"))


#### MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)


# M1 - logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1spr.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1))

# save the data
saveRDS(m1spr.cv, file = here("sdmtmb", "model-outputs", "m1spr-cv.rds"))


# M2 - no spatial random effects, but 
m2spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1)) 

# save the data
saveRDS(m2spr.cv, file = here("sdmtmb", "model-outputs", "m2spr-cv.rds"))



# M3 logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects
m3spr.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, # did not converge
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             control = sdmTMBcontrol(nlminb_loops = 2))  #extra optimization to help with convergence 
# Non-linear minimizer did not converge: do not trust this model!
# m3spr.cv$converged 
# [1] TRUE

# save the data
saveRDS(m3spr.cv, file = here("sdmtmb", "model-outputs", "m3spr-cv.rds"))


# M4 
m4spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1, 
             data = sf_spring, 
             mesh = spring_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1)) 

# save the data
saveRDS(m4spr.cv, file = here("sdmtmb", "model-outputs", "m4spr-cv.rds"))


# m5
m5spr.cv <- sdmTMB_cv(EXPCATCHWT ~ AVGDEPTH, # did not converge
             data = sf_spring,
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "EST_YEAR",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)? 
             control = sdmTMBcontrol(newton_loops = 1)) 
# Non-linear minimizer did not converge: do not trust this model!
# m5spr.cv$converged 
# [1] TRUE

# save the data
saveRDS(m5spr.cv, file = here("sdmtmb", "model-outputs", "m5spr-cv.rds"))



# M6 
m6spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1, 
             data = sf_spring,
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "EST_YEAR",
             spatiotemporal = "IID") #possbly AR1 (adapt slowly over time)?

# save the data
saveRDS(m6spr.cv, file = here("sdmtmb", "model-outputs", "m6spr-cv.rds"))


# m7 
m7spr.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR)-1,
             data = sf_spring,
             mesh = spring_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             time = "EST_YEAR",
             spatiotemporal = "IID") 

# save the data
saveRDS(m7spr.cv, file = here("sdmtmb", "model-outputs", "m7spr-cv.rds"))


#### COMPARISON TABLE ####

spr.mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                   "AIC" = c(AIC(m1_spring), AIC(m2_spring), AIC(m3_spring), AIC(m4_spring), AIC(m5_spring), AIC(m6_spring), AIC(m7_spring)),# )


#mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                   "TOTAL_ELPD" = c(m1spr.cv$elpd, m2spr.cv$elpd, m3spr.cv$elpd, m4spr.cv$elpd, m5spr.cv$elpd, m6spr.cv$elpd, m7spr.cv$elpd), 
                   "TOTAL_LOGLIK" = c(m1spr.cv$sum_loglik, m2spr.cv$sum_loglik, m3spr.cv$sum_loglik, m4spr.cv$sum_loglik, m5spr.cv$sum_loglik, m6spr.cv$sum_loglik, m7spr.cv$sum_loglik)) #%>% 
  #right_join(mods, by = "models")

# save the data
saveRDS(spr.mods, file = here("sdmtmb", "model-outputs", "spr-mod-diagnostics.rds"))


# format the table for presentation
spr_mods_tbl <- spr.mods %>% 
  mutate(TOTAL_ELPD = round(TOTAL_ELPD, 2),
         TOTAL_LOGLIK = round(TOTAL_LOGLIK, 2)) %>% 
  rename(" " = models, 
         "Total Expected Log Predictive Density" = TOTAL_ELPD,
         "Total Log Likelihood" = TOTAL_LOGLIK)%>% 
  arrange(desc(AIC))

kable(spr_mods_tbl[,c(1, 4, 2:3)], align = "lcccc", caption = "Spring Model Diagnostic Values", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 
  
# how to save the table 





