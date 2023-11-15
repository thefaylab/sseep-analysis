### created: 11/06/2023
### last updated: 

# 03 - SPRING MODEL RESIDUALS ####

## OBJECTIVE ####
# calculate randomized quantile and mcmc-based residuals for the best fitting spring model 
# plot the mcmc-based residuals against the fitted data and spatially to note any potential spatial autocorrelation


### LOAD PACKAGES ####
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

### LOAD DATA ####
strata <- sf::st_read(dsn = here("gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) %>% 
  rename(STRATUM = "Strata_Num")

# best fit model created in  here("sdmtmb", "sumflounder", "01-mod-fits", "01a-fit-spr-mods.R")
spring_mod <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m12_spring.rds"))


## DATA WRANGLE ####
# extract data used to fit the model 
spr_moddat <- spring_mod$data

## CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
# pull residuals from best fit model
spr_moddat$resids <- residuals(spring_mod) 

# plot frequency of residuals to find distribution 
hist(spr_moddat$resids) 

# qqplot of residuals
qqnorm(spr_moddat$resids) 
qqline(spr_moddat$resids) # add trend line


### CALCULATE MCMC-BASED RESIDUALS ####
spr_samps <- sdmTMBextra::predict_mle_mcmc(spring_mod, mcmc_iter = 201, mcmc_warmup = 200)

spr_mod_mcres <- residuals(spring_mod, type = "mle-mcmc", mcmc_samples = spr_samps)

# qqplot
qqnorm(spr_mod_mcres) 
qqline(spr_mod_mcres) # add trendline

### save the data
saveRDS(spr_mod_mcres, here("sdmtmb", "atlmackerel", "data", "spr_mcres.rds"))

# add the MCMC residuals to the spring data 
spr_moddat$mc_resids <- spr_mod_mcres

### save the data 
saveRDS(spr_moddat, file = here("sdmtmb", "atlmackerel", "data", "spr_dat-resids.rds"))


### FITTED VS RESIDUALS 
ggplot(spr_moddat) +
  geom_point(aes(x = EXPCATCHWT, y = mc_resids)) + 
  facet_wrap(~SEASON) + 
  labs(x = "Biomass (kg)", y = "MCMC Residual")

ggsave("spring_fit-v-resid.png", plot = last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 4)

### RESIDUAL PLOTTED BY SPACE 
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


ggsave("spring_resid_map.png", plot = last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 5)
