### created: 04/01/2023
### last updated: 12/22/2023

# 01a - GRID PREDICTIONS: FALL ####

## OBJECTIVE ####
# make predictions from the best fitting spatiotemporal model over the fall spatial footprint for summer flounder


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
strata <- readRDS(here("data", "rds", "active_strata.rds"))

#read in fall data for model fitting created here("sdmtmb", "sumflounder", "01-mod-fits", "01-prepare-data.R")
sf_fall <- readRDS(here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds"))
# 
# # read in the fall mesh for model fitting created here("sdmtmb", "sumflounder", "01-mod-fits", "01-prepare-data.R")
# fall_mesh <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mesh.rds"))
# 
# # define extra years to forecast 
# fall_extra_years <- c(2020, 2022:2026)

# read in the best fitting spring model created here("sdmtmb", "sumflounder", "01-mod-fits", "02a-fit-fall-mods.R")
fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

# read in the grid with the area predictor created here("sdmtmb", "R", "make-survey-grid.R")
grid <- readRDS(here("sdmtmb", "survey_grid_Jun2022.rds")) 


## SUBSET THE GRID ####
# convert the data frame to a simple feature to crop the grid to areas of historical summer flounder catch rates
grid_sf <- grid |> 
  sf::st_as_sf(coords = c("X", "Y")) # convert to sf using X and Y UTM coords

# establish coordinate system as UTM 18N zone
st_crs(grid_sf) <-32618  

# filter strata sf based on fall catch data 
sf_fall_strat <- right_join(strata, sf_fall, by = "STRATUM") 

# transform coordinate system to UTM 18N
fall_strat_utm <- st_transform(sf_fall_strat, crs = 32618) 

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
  select(-geometry) |> # coerce to dataframe
  mutate(X = X/1000, # convert coordinate to km to match coordinates in bts data
         Y = Y/1000, 
         SEASON = "FALL")

# plot it 
ggplot(fall_grid, aes(X, Y, fill = AVGDEPTH)) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()

saveRDS(fall_grid, here("sdmtmb", "sumflounder", "data", "sf_fall_grid_Jun2022.rds"))


## FIT MODEL ####
# re-run model 15a from here("sdmtmb", "sumflounder", "01-mod-fits", "02a-fit-fall-mods.R") with forecasted years, and REML off 
# fall_mod <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
#                       #as.factor(EST_YEAR) + 
#                       as.factor(AREA),#-1,
#                     data = sf_fall,
#                     mesh = fall_mesh,
#                     family = tweedie(link = "log"), 
#                     spatial = "on", 
#                     time = "EST_YEAR",
#                     spatiotemporal = "IID", 
#                     extra_time = fall_extra_years, 
#                     control = sdmTMBcontrol(newton_loops = 1), 
#                     silent = FALSE) 
# saveRDS(fall_mod, here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))
#fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))


## MAKE PREDICTIONS ####
### Grid Predictions ####
# replicate grid across all necessary years
fall_grid <- sdmTMB::replicate_df(fall_grid, "EST_YEAR", c(2009:2016, 2018, 2019, 2021)) |> # fall survey does not have data for 2020 and an incomplete survey year in 2017 so there cannot be a grid for 2017 or 2020 to predict across
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# predictions
fall_preds <- predict(fall_mod, newdata = fall_grid)#, return_tmb_object = TRUE)# |> as.data.frame()

### save the predictions
saveRDS(fall_preds, file = here("sdmtmb", "sumflounder", "data", "fall_grid_preds.rds"))


# #### ###
# call the plotting functions
# source(file = here("sdmtmb", "R", "plot_fns.R"))
# 
# # biomass estimates (main + random effects)
plot_preds(fall_preds |> filter(EST_YEAR %in% c(2018:2021)), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")
# 
# 
# # biomass estimates (main + random effects) in wind areas only
# plot_preds(fall_preds |> filter(EST_YEAR %in% c(2018:2026), AREA == "WIND"), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
#   labs(fill = "Biomass estimates") +
#   theme(legend.position = "bottom")


