### created: 12/10/2022
### last updated: 

#### 02b - COMPARE MODELS ####

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
m1 <- readRDS(file = here("sdmtmb", "model-outputs", "m1.rds"))
m2 <- readRDS(file = here("sdmtmb", "model-outputs", "m2.rds"))
m3 <- readRDS(file = here("sdmtmb", "model-outputs", "m3.rds"))
m4 <- readRDS(file = here("sdmtmb", "model-outputs", "m4.rds"))
m5 <- readRDS(file = here("sdmtmb", "model-outputs", "m5.rds"))
m6 <- readRDS(file = here("sdmtmb", "model-outputs", "m6.rds"))

sumflounder <- readRDS(here("sdmtmb", "data", "sumflounder.rds"))
mesh <- readRDS(here("sdmtmb", "data", "mesh"))


#### MODEL CROSS VALIDATIONS ####
# library(future)
# plan(multisession)


# M1 - logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1.cv <- sdmTMB_cv(EXPCATCHWT ~ depth, 
             data = sumflounder, 
             mesh = mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1))

# save the data
saveRDS(m1.cv, file = here("sdmtmb", "model-outputs", "m1-cv.rds"))


# M2 - no spatial random effects, but 
m2.cv <- sdmTMB_cv(EXPCATCHWT ~ s(depth) + as.factor(year) - 1, 
             data = sumflounder, 
             mesh = mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1)) 

# save the data
saveRDS(m2.cv, file = here("sdmtmb", "model-outputs", "m2-cv.rds"))



# M3 logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects
m3.cv <- sdmTMB_cv(EXPCATCHWT ~ depth, 
             data = sumflounder, 
             mesh = mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1))  #extra optimization to help with convergence 

# save the data
saveRDS(m3.cv, file = here("sdmtmb", "model-outputs", "m3-cv.rds"))


# M4 
m4.cv <- sdmTMB_cv(EXPCATCHWT ~ s(depth) + as.factor(year) - 1, 
             data = sumflounder, 
             mesh = mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1)) 

# save the data
saveRDS(m4, file = here("sdmtmb", "model-outputs", "m4.rds"))


# m5
m5.cv <- sdmTMB_cv(EXPCATCHWT ~ depth, 
             data = sumflounder,
             mesh = mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "year",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)? 
             control = sdmTMBcontrol(newton_loops = 1)) 

# save the data
saveRDS(m5.cv, file = here("sdmtmb", "model-outputs", "m5-cv.rds"))



# M6 
m6.cv <- sdmTMB_cv(EXPCATCHWT ~ s(depth) + as.factor(year)-1, 
             data = sumflounder,
             mesh = mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "year",
             spatiotemporal = "IID") #possbly AR1 (adapt slowly over time)?

# save the data
saveRDS(m6.cv, file = here("sdmtmb", "model-outputs", "m6-cv.rds"))


# m7 
m7.cv <- sdmTMB_cv(EXPCATCHWT ~ s(depth) + as.factor(year)-1,
             data = sumflounder,
             mesh = mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             time = "year",
             spatiotemporal = "IID") 

# save the data
saveRDS(m7.cv, file = here("sdmtmb", "model-outputs", "m7-cv.rds"))


#### COMPARISON TABLE ####

mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                   "AIC" = c(AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5), AIC(m6), AIC(m7)))


mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                   "TOTAL_ELPD" = c(m1.cv$elpd, m2.cv$elpd, m3.cv$elpd, m4.cv$elpd, m5.cv$elpd, m6.cv$elpd, m7.cv$elpd), 
                   "TOTAL_LOGLIK" = c(m1.cv$sum_loglik, m2.cv$sum_loglik, m3.cv$sum_loglik, m4.cv$sum_loglik, m5.cv$sum_loglik, m6.cv$sum_loglik, m7.cv$sum_loglik)) %>% 
  right_join(mods, by = "models")

# save the data
saveRDS(mods, file = here("sdmtmb", "model-outputs", "mod-diagnostics.rds"))


# format the table for presentation
mods_tbl <- mods %>% 
  mutate(TOTAL_ELPD = round(TOTAL_ELPD, 2),
         TOTAL_LOGLIK = round(TOTAL_LOGLIK, 2)) %>% 
  rename(" " = models, 
         "Total Expected Log Predictive Density" = TOTAL_ELPD,
         "Total Log Likelihood" = TOTAL_LOGLIK)%>% 
  arrange(desc(AIC))

kable(mods_tbl[,c(1, 4, 2:3)], align = "lcccc", caption = "Model Diagnostic Values", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 
  
# how to save the table 





