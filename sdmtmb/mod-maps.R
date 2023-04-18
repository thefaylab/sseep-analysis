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
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
#source()

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"


state_data <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf") |>
  filter(region %in% c("Northeast", "South"))

ne_states <- suppressWarnings(suppressMessages(
  st_crop(state_data,
          c(xmin = -80, ymin = 32, xmax = -60, ymax = 46))))


utm_zone18 <- 32618
ne_states_proj <- sf::st_transform(ne_states, crs = utm_zone18)
sf::st_boundary(ne_coast_proj)

ggplot(ne_states_proj) + geom_sf() +
  geom_tile(data = spring_preds, aes(x = X * 1000, y = Y * 1000, fill = exp(est)), width = 10000, height = 10000) +
  #xlim(230957.7, 1157991 - 300000) +
  #ylim(5366427, 6353456 - 200000) +
  scale_fill_viridis_c() +
  theme_bw() +
  labs(fill = "Predicted\ndensity") +
  labs(x = "Longitude", y = "Latitude")

ggplot(ne_states_proj) + geom_sf() +
  geom_tile(data = fall_preds, aes(x = X * 1000, y = Y * 1000, fill = exp(est)), width = 10000, height = 10000) +
  #xlim(230957.7, 1157991 - 300000) +
  #ylim(5366427, 6353456 - 200000) +
  scale_fill_viridis_c() +
  theme_bw() +
  labs(fill = "Predicted\ndensity") +
  labs(x = "Longitude", y = "Latitude")


# main effects only
plot_map(spring_preds, exp(est_non_rf))

# spatial random effects
plot_map(fall_preds|> filter(EST_YEAR %in% c(2021)), omega_s) + scale_fill_gradient2()

# spatiotemporal random effects
plot_map(fall_preds |> filter(EST_YEAR %in% c(2018:2021)), epsilon_st) + scale_fill_gradient2()

