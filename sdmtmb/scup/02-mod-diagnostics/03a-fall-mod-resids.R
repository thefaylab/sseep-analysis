### created: 11/24/2023
### last updated: 04/30/2024

# 03a - FALL MODEL RESIDUALS ####

## OBJECTIVE ####
# calculate randomized quantile and mcmc-based residuals for the best fitting fall model 
# plot the mcmc-based residuals against the fitted data and spatially to note any potential spatial autocorrelation

## LOAD PACKAGES ####
library(patchwork)
library(here)
library(sdmTMB)
library(sdmTMBextra)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

## LOAD DATA ####
strata <- sf::st_read(dsn = here("gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) %>% 
   rename(STRATUM = "Strata_Num")

# best fit model created here("sdmtmb", "scup", "01-mod-fits", "01a-fit-fall-mods.R")
fall_mod <- readRDS(here("sdmtmb", "scup", "data", "mods", "tweedie","m7_fall_tw.rds"))

### Model Predictions ####
fall_mod_preds <- predict(fall_mod)

### save the data 
saveRDS(fall_mod_preds, file = here("sdmtmb", "scup", "data", "fall_mod_preds.rds"))
saveRDS(fall_mod, file = here("sdmtmb", "scup", "data", "fall_mod.rds"))

## DATA WRANGLE ####
# extract data used to fit the model 
fall_moddat <- fall_mod$data

## CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
# pull residuals from best fit model
fall_moddat$resids <- residuals(fall_mod, type = "mle-mvn") 

# plot frequency of residuals to find distribution
hist(fall_moddat$resids)

# qplot of residuals 
qqnorm(fall_moddat$resids)
qqline(fall_moddat$resids) # add trend line



### CALCULATE MCMC-BASED RESIDUALS ####
fall_samps <- sdmTMBextra::predict_mle_mcmc(fall_mod, mcmc_iter = 201, mcmc_warmup = 200)

fall_mod_mcres <- residuals(fall_mod, type = "mle-mcmc", mcmc_samples = fall_samps)

# qqplot
qqnorm(fall_mod_mcres)
qqline(fall_mod_mcres) # add trendline

### save the data 
saveRDS(fall_mod_mcres, here("sdmtmb", "scup", "data", "fall_mcres.rds"))
 
# add the MCMC residuals to the fall data 
fall_moddat$mc_resids <-fall_mod_mcres

### save the data 
saveRDS(fall_moddat, file = here("sdmtmb", "scup", "data", "fall_dat-resids.rds"))
fall_moddat<-readRDS(file = here("sdmtmb", "scup", "data", "fall_dat-resids.rds"))

## DIAGNOSTIC PLOTS ####
### FITTED VS RESIDUALS PLOT ####
ggplot(fall_moddat) +
  geom_point(aes(x = EXPCATCHWT, y = mc_resids)) + 
  facet_wrap(~YEAR) + 
  labs(x = "log(Biomass (kg))", y = "MCMC Residual", title = "MCMC residuals")

ggsave("fall_fit-v-resid.png" , plot = last_plot(), device = "png", here("sdmtmb", "scup", "plots"), width = 6, height = 4)

### RESIDUALS PLOTTED BY SPACE ####
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

ggsave("fall_resid_map.png" ,plot = last_plot(), device = "png", here("sdmtmb", "scup", "plots"), width = 8, height = 5)





