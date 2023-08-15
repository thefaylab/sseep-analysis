### created: 04/18/2023
### last updated: 08/14/2023

# 02 - MAKE PRETTY MAPS ####

## OBJECTIVE ####
# project predictions onto map with Eastern Coast for frame of reference


### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
library(raster)
library(sdmTMB)
library(marmap)
library(oce)
library(rnaturalearth)
library(rnaturalearthdata)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())


# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
#sseep.dir <- "../sseep-analysis"
source(here("sdmtmb", "R", "plot_fns.R"))

### LOAD DATA ####
# read in predictions created here("mod-forecasts.R")
fall_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "fall_predictions.rds"))

spring_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "spring_predictions.rds"))


## FALL MAPS ####
# estimates 
plot_map(coastline, dat = fall_preds |> filter(EST_YEAR %in% c(2018:2021)), column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") 

# main effects only
plot_map(coastline, dat = fall_preds |> filter(EST_YEAR %in% c(2018:2021)), column = exp(est_non_rf))

# spatial random effects
plot_map(coastline, dat = fall_preds |> filter(EST_YEAR %in% c(2018:2021)), column = omega_s) + scale_fill_gradient2()

# spatiotemporal random effects
plot_map(coastline, dat = fall_preds |> filter(EST_YEAR %in% c(2018:2021)), column = epsilon_st) + scale_fill_gradient2()


## SPRING MAPS ####
# estimates 
plot_map(coastline, dat = spring_preds |> filter(EST_YEAR %in% c(2018:2021)), column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") 

# main effects only
plot_map(coastline, dat = spring_preds |> filter(EST_YEAR %in% c(2018:2021)), column = exp(est_non_rf))

# spatial random effects
plot_map(coastline, dat = spring_preds |> filter(EST_YEAR %in% c(2018:2021)), column = omega_s) + scale_fill_gradient2()

# spatiotemporal random effects
plot_map(coastline, dat = spring_preds |> filter(EST_YEAR %in% c(2018:2021)), column = epsilon_st) + scale_fill_gradient2()


#### ####
# ggplot(ne_states_proj) + geom_sf() +
#   geom_tile(data = spring_preds, aes(x = X * 1000, y = Y * 1000, fill = exp(est)), width = 10000, height = 10000) +
#   #xlim(230957.7, 1157991 - 300000) +
#   #ylim(5366427, 6353456 - 200000) +
#   scale_fill_viridis_c() +
#   theme_bw() +
#   labs(fill = "Predicted\ndensity") +
#   labs(x = "Longitude", y = "Latitude")
# 
# ggplot() + #geom_sf() +
#   geom_tile(data = fall_preds, aes(x = X * 1000, y = Y * 1000, fill = exp(est)), width = 10000, height = 10000) +
#   #xlim(230957.7, 1157991 - 300000) +
#   #ylim(5366427, 6353456 - 200000) +
#   geom_sf(data = ne_states_proj) +
#   scale_fill_viridis_c(trans = "sqrt", option = "H") +
#   theme_bw() +
#   labs(fill = "Predicted\ndensity") +
#   labs(x = "Longitude", y = "Latitude")
# 
# ggplot(east_coast) + 
#   geom_sf() +
#   geom_raster(data = fall_preds, aes(x = X * 1000, y = Y * 1000, fill = exp(est))) +
#   #xlim(230957.7, 1157991 - 300000) +
#   #ylim(5366427, 6353456 - 200000) +
#   scale_fill_viridis_c() +
#   theme_light() +
#   labs(fill = "Predicted\ndensity") +
#   labs(x = "Longitude", y = "Latitude")

