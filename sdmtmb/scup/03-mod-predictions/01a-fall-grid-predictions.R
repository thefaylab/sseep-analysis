### created: 01/05/2023
### modified: 01/05

# 01a - GRID PREDICTIONS: FALL ####

## OBJECTIVE ####
# make predictions from the best fitting spatiotemporal model over the fall spatial footprint for scup


### LOAD PACKAGES ####
library(sf)
library(patchwork)
library(here)
library(sdmTMB)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"

### LOAD DATA ####
# load active strata shapefile 
act_strata <- readRDS(here("data", "rds", "active_strata.rds"))

#read in fall data for model fitting created here("sdmtmb", "scup", "01-mod-fits", "01-prepare-data.R")
scup_fall <- readRDS(here("sdmtmb", "scup", "data", "scup_fall.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))
# 

# read in the best fitting spring model created here("sdmtmb", "scup", "01-mod-fits", "02a-fit-fall-mods.R")

#fall_mod0 <- readRDS(here("sdmtmb", "scup", "data", "fall_mod.rds"))
fall_mod_tw <- readRDS(here("sdmtmb", "scup", "data","mods","comps", "m7d_fall_tw.rds"))


# read in the grid with the area predictor created here("sdmtmb", "R", "make-survey-grid.R")
#grid <- readRDS(here("sdmtmb", "survey_grid_Jun2022.rds")) 
grid <- readRDS(here("sdmtmb", "survey_grid_all_122024.rds")) |>
  mutate(X = X*1000,
         Y = Y*1000)

## SUBSET THE GRID ####
# convert the data frame to a simple feature to crop the grid to areas of historical summer flounder catch rates
grid_sf <- grid |> 
  sf::st_as_sf(coords = c("X", "Y")) # convert to sf using X and Y UTM coords

# establish coordinate system as UTM 18N zone
st_crs(grid_sf) <-32618  

# filter strata sf based on fall catch data 
scup_fall_strat <- right_join(act_strata, scup_fall, by = "STRATUM") 

# transform coordinate system to UTM 18N
fall_strat_utm <- st_transform(scup_fall_strat, crs = 32618) 

# merge strata to create one polygon
fall_strat_union <- st_union(fall_strat_utm)

# identify cells that intersect with the fall strata polygon 
intersected <- sf::st_intersects(grid_sf, fall_strat_union)


# filter the grid for only intersecting cells, where intersected has a value greater than 0. 
selected_grid <- grid_sf[lengths(intersected) > 0, ] 

# retrieve the coordinates from the selected grid matrix
coords <- selected_grid |>
  st_coordinates()

# bind the coordinates with the selected grid 
fall_grid <- bind_cols(coords, selected_grid) |> 
  dplyr::select(-geometry) |> # coerce to dataframe
  mutate(X = X/1000, # convert coordinate to km to match coordinates in bts data
         Y = Y/1000, 
         SEASON = "FALL")

# plot it 
ggplot(fall_grid, aes(X, Y, fill = median_1 )) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()

ggplot(fall_grid, aes(X, Y, fill = median_2 )) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()

ggplot(fall_grid, aes(X, Y, fill = mean_1 )) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()

ggplot(fall_grid, aes(X, Y, fill = mean_2 )) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()

saveRDS(fall_grid, here("sdmtmb", "scup", "data", "scup_fall_grid_122024.rds"))




## MAKE PREDICTIONS ####
### Grid Predictions ####
# replicate grid across all necessary years
fall_grid_1 <- sdmTMB::replicate_df(fall_grid, "EST_YEAR", c(2009:2016, 2018, 2019, 2021)) |> # fall survey does not have data for 2020 and an incomplete survey year in 2017 so there cannot be a grid for 2017 or 2020 to predict across
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA),
         AVGDEPTH = mean_1)


fall_grid_2 <- sdmTMB::replicate_df(fall_grid, "EST_YEAR", c(2009:2016, 2018, 2019, 2021)) |> # fall survey does not have data for 2020 and an incomplete survey year in 2017 so there cannot be a grid for 2017 or 2020 to predict across
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA),
         AVGDEPTH = mean_2)

fall_grid_m1 <- sdmTMB::replicate_df(fall_grid, "EST_YEAR", c(2009:2016, 2018, 2019, 2021)) |> # fall survey does not have data for 2020 and an incomplete survey year in 2017 so there cannot be a grid for 2017 or 2020 to predict across
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA),
         AVGDEPTH = median_1)

fall_grid_m2 <- sdmTMB::replicate_df(fall_grid, "EST_YEAR", c(2009:2016, 2018, 2019, 2021)) |> # fall survey does not have data for 2020 and an incomplete survey year in 2017 so there cannot be a grid for 2017 or 2020 to predict across
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA),
         AVGDEPTH = median_2)

# predictions for tweedie model
fall_preds_tw_1 <- predict(fall_mod_tw, newdata = fall_grid_1)#, fall preds based on mean
fall_preds_tw_2 <- predict(fall_mod_tw, newdata = fall_grid_2)#, fall preds based on filtered mean
fall_preds_tw_m1 <- predict(fall_mod_tw, newdata = fall_grid_m1)#, fall preds based on median
fall_preds_tw_m2 <- predict(fall_mod_tw, newdata = fall_grid_m2)#, fall preds based on median filtered

### save the predictions
saveRDS(fall_preds_tw_1, file = here("sdmtmb", "scup", "data", "fall_grid_preds_tw_1.rds"))
saveRDS(fall_preds_tw_2, file = here("sdmtmb", "scup", "data", "fall_grid_preds_tw_2.rds"))
saveRDS(fall_preds_tw_m1, file = here("sdmtmb", "scup", "data", "fall_grid_preds_tw_m1.rds"))
saveRDS(fall_preds_tw_m2, file = here("sdmtmb", "scup", "data", "fall_grid_preds_tw_m2.rds"))

# #### ###
# call the plotting functions
source(file = here("sdmtmb", "R", "plot_fns.R"))
# 
# # biomass estimates (main + random effects)
plot_preds(fall_preds_tw_1, exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "right")

plot_preds(fall_preds_tw_2, exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "right")


plot_preds(fall_preds_tw_m1, exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "right")

plot_preds(fall_preds_tw_m2, exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "right")
# 
# 
