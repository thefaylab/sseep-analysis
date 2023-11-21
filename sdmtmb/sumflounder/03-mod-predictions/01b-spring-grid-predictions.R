### created: 04/01/2023
### last updated: 11/21/2023

# PREDICT AND FORECAST ####

## OBJECTIVE ####
# run best fit models for spring
# include extra years in the model fit for future distribution forecasting
# make predictions and forecasts over the impacted grid from model fits


### LOAD PACKAGES ####
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

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"

### LOAD DATA ####
# load active strata shapefile 
strata <- readRDS(here("data", "rds", "active_strata.rds"))

# read in spring data for model fitting created here("sdmtmb", "sumflounder", "01-mod-fits", "01-prepare-data.R")
sf_spring <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_spring.rds"))

# read in the spring mesh for model fitting created here("sdmtmb", "sumflounder", "01-mod-fits", "01-prepare-data.R")
#spring_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mesh.rds"))

# read in the best fitting spring model created here("sdmtmb", "sumflounder", "01-mod-fits", "02b-fit-spr-mods.R")
spring_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_mod.rds"))

# define extra years to forecast 
#spring_extra_years <- c(2022:2026)

# read in the grid with the area predictor created here("sdmtmb", "R", "make-survey-grid.R")
grid <- readRDS(here("sdmtmb", "survey_grid_Jun2022.rds")) 


## SUBSET THE GRID ####
# convert the data frame to a simple feature to crop the grid to areas of historical summer flounder catch rates
grid_sf <- grid |> 
  sf::st_as_sf(coords = c("X", "Y")) # convert to sf using X and Y UTM coords

# establish coordinate system as UTM 18N zone
st_crs(grid_sf) <-32618  

# filter strata sf based on spring catch data 
sf_spr_strat <- right_join(strata, sf_spring, by = "STRATUM") 

# transform coordinate system to UTM 18N
spr_strat_utm <- st_transform(sf_spr_strat, crs = 32618) 

# merge strata to create one polygon
spr_strat_union <- st_union(spr_strat_utm) 

# identify cells that intersect with the fall strata polygon 
intersected <- sf::st_intersects(grid_sf, spr_strat_union) 

# filter the grid for only intersecting cells, where intersected has a value greater than 0. 
selected_grid <- grid_sf[lengths(intersected) > 0, ] 

# retrieve the coordinates from the selected grid matrix
coords <- selected_grid |>
  st_coordinates()

# bind the coordinates with the selected grid 
spr_grid <- bind_cols(coords, selected_grid) |> 
  select(-geometry) |>
  mutate(X = X/1000,  # convert coordinate to km to match coordinates in bts data
         Y = Y/1000, 
         SEASON = "SPRING")

saveRDS(spr_grid, here("sdmtmb", "sumflounder", "data", "sf_spring_grid_Jun2022.rds"))

ggplot(spr_grid, aes(X, Y, fill = AVGDEPTH)) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()


## FIT MODEL ###
# re run model 15b from here("sdmtmb", "sumflounder", "01-mod-fits", "02b-fit-spr-mods.R") with forecasted years, and REML off
# spring_mod <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + #as.factor(EST_YEAR) + 
#                        as.factor(AREA),#-1,
#                      data = sf_spring,
#                      mesh = spring_mesh,
#                      family = tweedie(link = "log"), 
#                      spatial = "on", 
#                      time = "EST_YEAR",
#                      spatiotemporal = "IID",
#                      extra_time = spring_extra_years,  
#                      control = sdmTMBcontrol(newton_loops = 1), 
#                      silent = FALSE) 
# saveRDS(spring_mod, here("sdmtmb", "sumflounder", "data", "spring_mod.rds"))
#spring_mod <- readRDS(here("sdmtmb", "mar-536-project", "data", "spring_mod.rds"))

## MAKE PREDICTIONS ####
### Grid predictions ####
# replicate the grid across all necessary years
spr_grid <- sdmTMB::replicate_df(spr_grid, "EST_YEAR", c(2009:2021)) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# predictions
spring_preds <- predict(spring_mod, newdata = spr_grid)#, return_tmb_object = TRUE) #|> as.data.frame()

### save the predictions
saveRDS(spring_preds, file = here("sdmtmb", "sumflounder", "data", "spring_grid_preds.rds"))

#### #### 
#call the plotting functions
source(file = here("sdmtmb", "R", "plot_fns.R"))

# biomass estimates (main + random effects)
plot_preds(spring_preds |> filter(EST_YEAR %in% c(2018:2026)), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

# biomass estimates (main + random effects) in wind areas only
plot_preds(spring_preds |> filter(EST_YEAR %in% c(2018:2026), AREA == "WIND"), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

