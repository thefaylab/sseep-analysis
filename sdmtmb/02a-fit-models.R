### created: 12/10/2022
### last updated: 

#### 02a - FIT MODELS ####

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

here()


#### LOAD DATA ####
# summer flounder data 
sumflounder <- readRDS(here("sdmtmb", "data", "sumflounder.rds"))
# sf_fall <- readRDS(here("sdmtmb", "data", "sumflounder_fall.rds"))
# sf_spring <- readRDS(here("sdmtmb", "data", "sumflounder_spring.rds"))


# mesh 
mesh <- readRDS(here("sdmtmb", "data", "mesh"))



#### MODEL FITS ####
# M1 - logistic regression of summer flounder biomass in tows as a function of depth without spatial random effects
m1 <- sdmTMB(EXPCATCHWT ~ depth, 
             data = sumflounder, 
             mesh = mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off")

saveRDS(m1, file = here("sdmtmb", "model-outputs", "m1.rds"))

tidy(m1, conf.int = TRUE) 
tidy(m1, "ran_pars", conf.int = TRUE)
sanity(m1) 

# M2 - no spatial random effects, but 
m2 <- sdmTMB(EXPCATCHWT ~ s(depth) + as.factor(year) - 1, 
             data = sumflounder, 
             mesh = mesh,
             family = tweedie(link = "log"), # useful for positive continuous data, biomass 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1))

saveRDS(m2, file = here("sdmtmb", "model-outputs", "m2.rds"))

tidy(m2, conf.int = TRUE) 
tidy(m2, "ran_pars", conf.int = TRUE)
sanity(m2) 




# M3 - logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects
m3 <- sdmTMB(EXPCATCHWT ~ depth, 
             data = sumflounder, 
             mesh = mesh,
             family = tweedie(link = "log"), 
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1)) #extra optimization to help with convergence

saveRDS(m3, file = here("sdmtmb", "model-outputs", "m3.rds"))


tidy(m3, conf.int = TRUE) 
tidy(m3, "ran_pars", conf.int = TRUE)
sanity(m3) 

# M4 - logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and index standardization to estimate a separate intercept for each year
m4 <- sdmTMB(EXPCATCHWT ~ s(depth) + as.factor(year) - 1, 
             data = sumflounder, 
             mesh = mesh,
             family = tweedie(link = "log"),  
             spatial = "on", 
             control = sdmTMBcontrol(newton_loops = 1))

saveRDS(m4, file = here("sdmtmb", "model-outputs", "m4.rds"))

tidy(m4, conf.int = TRUE) 
tidy(m4, "ran_pars", conf.int = TRUE)
sanity(m4) 



# M5 - logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and spatiotemporal random fields estimated according to year. Independent correlation, so each year is independent of each other 
m5 <- sdmTMB(EXPCATCHWT ~ depth, 
             data = sumflounder,
             mesh = mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "year",
             spatiotemporal = "IID", #possbly AR1 (adapt slowly over time)? 
             control = sdmTMBcontrol(newton_loops = 1)) 

saveRDS(m5, file = here("sdmtmb", "model-outputs", "m5.rds"))

tidy(m5, conf.int = TRUE) 
tidy(m5, "ran_pars", conf.int = TRUE)
sanity(m5)


# m6 logistic regression of summer flounder biomass in tows as a function of depth with spatial random effects and spatiotemporal random fields estimated by year and with a separate intercept for each.  
m6 <- sdmTMB(EXPCATCHWT ~ s(depth) + as.factor(year)-1, #estimate separate intercepts by year; point est and uncertainty fed into SAs
             data = sumflounder,
             mesh = mesh,
             family = tweedie(link = "log"), 
             spatial = "on", #spatial covariance with depth
             time = "year",
             spatiotemporal = "IID") #possbly AR1 (adapt slowly over time)?

sanity(m6)

saveRDS(m6, file = here("sdmtmb", "model-outputs", "m6.rds"))

# pull estimates and confidence intervals etc? 
est <- tidy(m6, conf.int = TRUE) 
pars <- tidy(m6, "ran_pars", conf.int = TRUE, exponentiate = TRUE)

m6_est <- bind_rows(est, pars)

kable(m6_est, align = "lcccc", caption = "M6 model estimates", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)


#if spatiotemporal Sd is 10x spatial SD then indicates species is more dynamic (and shifting around a lot in space) and may not need spatial random fields


# m7 
# tested a model fit with only spatiotemporal random fields to test dynamics of summer flounder. Coupled with similar spatial SDs; confirms that both spatial and spatiotemporal random fields are needed and summer flounder does not change significantly from year to year. 
m7 <- sdmTMB(EXPCATCHWT ~ s(depth) + as.factor(year)-1,
             data = sumflounder,
             mesh = mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             time = "year",
             spatiotemporal = "IID") 


#### CONFIGURATIONS TABLE #####
mods.config <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                          "Formula" = c("EXPCATCHWT ~ depth", "EXPCATCHWT ~ s(depth) + as.factor(year) - 1", "EXPCATCHWT ~ depth",
                                        "EXPCATCHWT ~ s(depth) + as.factor(year) - 1", "EXPCATCHWT ~ depth", 
                                        "EXPCATCHWT ~ s(depth) + as.factor(year) - 1", "EXPCATCHWT ~ s(depth) + as.factor(year) - 1"), 
                          "Family" = rep(paste("Tweedie(link = log)"), 7), 
                          "Spatial Fields" = c(rep(paste("off"), 2), rep(paste("on"), 4), "off"), 
                          "Time" = c(rep(paste("-"), 4), rep(paste("year"), 3)), 
                          "Spatiotemporal Fields" =  c(rep(paste("-"), 4), rep(paste("IID"), 3)))

# save the data
saveRDS(mods.config, file = here("sdmtmb", "data", "mod-configs.rds"))


kable(mods.config, align = "lcccc", caption = "Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 

#### DEPTH RELATIONSHIP TABLE #####
dep.rel <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                      "depth_rel" = c(m1$model$par[2], m2$model$par[14], m3$model$par[2], m4$model$par[14],
                                      m5$model$par[2], m6$model$par[14], m7$model$par[14])) 

# save the data
saveRDS(dep.rel, file = here("sdmtmb", "data", "depth-rel.rds"))


dep.rel <- dep.rel %>% rename("Depth Estimate" = depth_rel)

kable(dep.rel, align = "lcccc", caption = "Depth Relationships", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 
