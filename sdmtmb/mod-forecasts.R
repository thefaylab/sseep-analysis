### created: 04/01/2023
### last updated: 04/17/2023

# PREDICT AND FORECAST ####

## OBJECTIVE ####
# run best fit models for fall and spring
# include extra years in the model fit for future distribution forecasting
# make predictions and forecasts over the impacted grid from model fits
# plot predictions over the impacted grid


## LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
library(raster)
library(sdmTMB)
library(marmap)
library(oce)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

## LOAD DATA ####
strata <- sf::st_read(dsn = here("gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) %>% 
  rename(STRATUM = "Strata_Num")

### FALL DATA ####
# read in fall data for model fitting
sf_fall <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds"))

# read in the fall mesh for model fitting 
fall_mesh <- readRDS(here(sdmtmb.dir, "data", "fall_mesh.rds"))

### SPRING DATA ####
# read in spring data for model fitting
sf_spring <- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds"))

# read in the spring mesh for model fitting  
spring_mesh <- readRDS(here(sdmtmb.dir, "data", "spring_mesh.rds"))

### THE GRID ####
# read in the grid with the area predictor created here(sdmtmb.dir, "grids")
grid <- readRDS(here(sdmtmb.dir, "data", "survey_grid.rds")) |> 
  mutate(X = X/1000, 
         Y = Y/1000)
         
# define extra years to forecast 
fall_extra_years <- c(2020, 2022:2026)
spring_extra_years <- c(2022:2026)

#### MODEL FITS ####
# re run model 11 from here(sdmtmb.dir, "02a-fit-fall-mods.R") with forecasted years, and REML off 
fall_mod <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                      #as.factor(EST_YEAR) + 
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "AR1", 
                    extra_time = fall_extra_years, 
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE) 
saveRDS(fall_mod, here(sdmtmb.dir, "mar-536-project", "data", "fall_mod.rds"))
#fall_mod <- readRDS(here(sdmtmb.dir, "mar-536-project", "data", "fall_mod.rds"))


# re run model 8 from here(sdmtmb.dir, "02b-fit-spr-mods.R") with forecasted years, and REML off
spring_mod <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + #as.factor(EST_YEAR) + 
                       as.factor(AREA),#-1,
                     data = sf_spring,
                     mesh = spring_mesh,
                     family = tweedie(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID",
                     extra_time = spring_extra_years,  
                     control = sdmTMBcontrol(newton_loops = 1), 
                     silent = FALSE) 
saveRDS(spring_mod, here("sdmtmb", "mar-536-project", "data", "spring_mod.rds"))
#spring_mod <- readRDS(here("sdmtmb", "mar-536-project", "data", "spring_mod.rds"))

#### MAKE PREDICTIONS ####
# replicate our grid across all necessary years
grid <- sdmTMB::replicate_df(grid, "EST_YEAR", c(2009:2026)) 

# predictions
fall_preds <- predict(fall_mod, newdata = grid)#, return_tmb_object = TRUE)# |> as.data.frame()
spring_preds <- predict(spring_mod, newdata = grid)#, return_tmb_object = TRUE) #|> as.data.frame()

### save the predictions
saveRDS(fall_preds, file = here(sdmtmb.dir, "data", "fall_projects.rds"))
saveRDS(spring_preds, file = here(sdmtmb.dir, "data", "spring_projects.rds"))

#### PLOT THE PREDICTIONS #### 
# call the plotting functions 
source(file = here("sdmtmb", "plot_fns.R"))

# biomass estimates (main + random effects)
plot_preds(fall_preds |> filter(EST_YEAR %in% c(2018:2026)), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

plot_preds(spring_preds |> filter(EST_YEAR %in% c(2018:2026)), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

# biomass estimates (main + random effects) in wind areas only
plot_preds(fall_preds |> filter(EST_YEAR %in% c(2018:2026), AREA == "WIND"), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

plot_preds(spring_preds |> filter(EST_YEAR %in% c(2018:2026), AREA == "WIND"), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

