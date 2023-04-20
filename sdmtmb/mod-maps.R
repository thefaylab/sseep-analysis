### created: 04/18/2023
### last updated: 

#### MAKE PRETTY MAPS ####

###################
#### OBJECTIVE ####
###################
# project predictions onto map with Eastern Coast for frame of reference

####################


#### LOAD PACKAGES ####
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
#source()

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

#### LOAD DATA ####
fall_preds <- readRDS(file = here(sdmtmb.dir, "data", "fall_projects.rds"))
spring_preds <- readRDS(file = here(sdmtmb.dir, "data", "spring_projects.rds"))
east_coast <- st_read(here(sseep.dir, "gis", "east_coast.shp"))


#### PLOTS ####
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


# main effects only
plot_map(sf = east_coast, dat = fall_preds |> filter(EST_YEAR %in% c(2018:2021)), column = exp(est_non_rf))

# spatial random effects
plot_map(sf = east_coast, dat = fall_preds |> filter(EST_YEAR %in% c(2018:2021)), column = omega_s) + scale_fill_gradient2()

# spatiotemporal random effects
plot_map(sf = east_coast, dat = fall_preds |> filter(EST_YEAR %in% c(2018:2021)), column = epsilon_st) + scale_fill_gradient2()

