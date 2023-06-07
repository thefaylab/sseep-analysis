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

# define extra years to forecast 
fall_extra_years <- c(2020, 2022:2026)
spring_extra_years <- c(2022:2026)

### THE GRID ####
# read in the grid with the area predictor created here(sdmtmb.dir, "grids")
grid <- readRDS(here(sdmtmb.dir, "data", "survey_grid.rds")) 

## DATA WRANGLE ####
grid_sf <- grid |> 
  st_as_sf(coords = c("X", "Y"))
st_crs(grid_sf) <-32618

### FALL GRID ####
sf_fall_strat <- right_join(strata, sf_fall, by = "STRATUM")
fall_strat_utm <- st_transform(sf_fall_strat, crs = 32618)
fall_strat_union <- st_union(fall_strat_utm)
intersected <- sf::st_intersects(grid_sf, fall_strat_union)
selected_grid <- grid_sf[lengths(intersected) > 0, ]  

coords <- selected_grid |>
  st_coordinates()
  
fall_grid <- bind_cols(coords, selected_grid) |> 
  select(-geometry) |>
  mutate(X = X/1000,
         Y = Y/1000, 
         SEASON = "FALL")

ggplot(fall_grid, aes(X, Y, fill = AVGDEPTH)) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()

### SPRING GRID ####
sf_spr_strat <- right_join(strata, sf_spring, by = "STRATUM")
spr_strat_utm <- st_transform(sf_spr_strat, crs = 32618)
spr_strat_union <- st_union(spr_strat_utm)
intersected <- sf::st_intersects(grid_sf, spr_strat_union)
selected_grid <- grid_sf[lengths(intersected) > 0, ]  

coords <- selected_grid |>
  st_coordinates()

spr_grid <- bind_cols(coords, selected_grid) |> 
  select(-geometry) |>
  mutate(X = X/1000,
         Y = Y/1000, 
         SEASON = "SPRING")

ggplot(spr_grid, aes(X, Y, fill = AVGDEPTH)) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()


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
fall_grid <- sdmTMB::replicate_df(fall_grid, "EST_YEAR", c(2009:2026)) 
spr_grid <- sdmTMB::replicate_df(spr_grid, "EST_YEAR", c(2009:2026)) 

# predictions
fall_preds <- predict(fall_mod, newdata = fall_grid)#, return_tmb_object = TRUE)# |> as.data.frame()
spring_preds <- predict(spring_mod, newdata = spr_grid)#, return_tmb_object = TRUE) #|> as.data.frame()

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

