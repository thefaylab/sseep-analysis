### created: 04/24/2023
### last updated: 

# Residuals ####

## OBJECTIVE ####
# 


## LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
library(raster)
library(sdmTMB)
library(marmap)
library(oce)
library(sdmTMBextra)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

## LOAD DATA ####
strata <- sf::st_read(dsn = here("gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) %>% 
  rename(STRATUM = "Strata_Num")

### FALL DATA ####
# read in fall data for model fitting
#sf_fall <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds"))

# # read in the fall mesh for model fitting 
# fall_mesh <- readRDS(here(sdmtmb.dir, "data", "fall_mesh.rds"))
# 
# ### SPRING DATA ####
# read in spring data for model fitting
#sf_spring <- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds"))

# read in the spring mesh for model fitting  
#spring_mesh <- readRDS(here(sdmtmb.dir, "data", "spring_mesh.rds"))

# best fit model created in `02a-fit-models.R` here("sdmtmb")
fall_mod <- readRDS(here(sdmtmb.dir, "mar-536-project", "data", "fall_mod.rds"))

spring_mod <- readRDS(here("sdmtmb", "mar-536-project", "data", "spring_mod.rds"))

# define extra years to forecast 
# fall_extra_years <- c(2020, 2022:2026)
# spring_extra_years <- c(2022:2026)

## DATA WRANGLE ####
fall_moddat <- fall_mod$data

spr_moddat <- spring_mod$data




### FALL MODEL 
##### Best Fit Model ####
# pull residuals from best fit model
fall_moddat$resids <- residuals(fall_mod) 

# plot frequency of residuals to find distribution
hist(fall_moddat$resids)

# qplot 
qqnorm(fall_moddat$resids)
qqline(fall_moddat$resids) # add trend line


#fit10_ml <- update(m10_fall, reml = FALSE) #refit your model with `reml = FALSE` to use MCMC-MLE residuals.
#saveRDS(fit10_ml, here("sdmtmb", "data", "m10fall_reml-off.rds"))

fall_samps <- sdmTMBextra::predict_mle_mcmc(fall_mod, mcmc_iter = 201, mcmc_warmup = 200)

fall_mod_mcres <- residuals(fall_mod, type = "mle-mcmc", mcmc_samples = fall_samps)

# qqplot
qqnorm(fall_mod_mcres)
qqline(fall_mod_mcres) # add trendline

saveRDS(fall_mod_mcres, here("sdmtmb", "data", "fall_mod_mcres.rds"))

fall_moddat$mc_resids <-fall_mod_mcres

# save the data 
saveRDS(fall_moddat, file = here("sdmtmb", "data", "fall_moddat.rds"))


### SPRING MODEL ###
##### Best Fit Model ####
# pull residuals from best fit model
spr_moddat$resids <- residuals(spring_mod) 

# plot frequency of residuals to find distribution 
hist(spr_moddat$resids) 

# qqplot 
qqnorm(spr_moddat$resids) 
qqline(spr_moddat$resids) # add trend line


#fit10_ml <- update(m10_fall, reml = FALSE) #refit your model with `reml = FALSE` to use MCMC-MLE residuals.
#saveRDS(fit10_ml, here("sdmtmb", "data", "m10fall_reml-off.rds"))

spr_samps <- sdmTMBextra::predict_mle_mcmc(spring_mod, mcmc_iter = 201, mcmc_warmup = 200)

spr_mod_mcres <- residuals(spring_mod, type = "mle-mcmc", mcmc_samples = spr_samps)

# qqplot
qqnorm(spr_mod_mcres) 
qqline(spr_mod_mcres) # add trendline

saveRDS(spr_mod_mcres, here("sdmtmb", "data", "spr_mod_mcres.rds"))

spr_moddat$mc_resids <- spr_mod_mcres

# save the data 
saveRDS(spr_moddat, file = here("sdmtmb", "data", "spr_moddat.rds"))


### FITTED VS RESIDUALS 
fall <- ggplot(fall_moddat) +
  geom_point(aes(x = EXPCATCHWT, y = mc_resids)) + 
  facet_wrap(~SEASON)

spring <- ggplot(spr_moddat) +
  geom_point(aes(x = EXPCATCHWT, y = mc_resids)) + 
  facet_wrap(~SEASON)

fall + spring


### RESIDUAL PLOTTED BY SPACE 
ggplot(fall_moddat, aes(DECDEG_BEGLON, DECDEG_BEGLAT, col = mc_resids)) + scale_colour_gradient2()+#low ="#5dc5e9", high = "#0a4c8a") +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Residuals") +
  #theme_bw() +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        #panel.border = element_rect(fill = NA, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

ggplot(spr_moddat, aes(DECDEG_BEGLON, DECDEG_BEGLAT, col = resids)) + scale_colour_gradient2()+#low ="#5dc5e9", high = "#0a4c8a") +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Residuals") +
  #theme_bw() +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        #panel.border = element_rect(fill = NA, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))
