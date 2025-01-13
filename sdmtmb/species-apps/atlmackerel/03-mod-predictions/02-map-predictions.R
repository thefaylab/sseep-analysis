### created: 12/28/2023
### last updated: 

# 02 - MAKE PRETTY MAPS ####

## OBJECTIVE ####
# project predictions onto map with Eastern Coast for frame of reference


### LOAD PACKAGES ####
library(sf)
library(patchwork)
library(here)
library(sdmTMB)
library(rnaturalearth)
library(rnaturalearthdata)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

source(here("sdmtmb", "R", "plot_fns.R"))

### LOAD DATA ####
# read in model fitting data where observations at depths greater than 200m were removed; created here("sdmtmb", "atlmackerel", "01-mod-fits", "04-explore-polynomial-mods.Rmd")
am_spring <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring_rm200.rds"))

# read in predictions created here("sdmtmb", "atlmackerel", "03-mod-predictions", "01-spring-grid-predictions.R")
grid_preds <- readRDS(file = here("sdmtmb", "atlmackerel", "data", "atlmackerel_grid_preds.rds")) |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)))

## MAPS ####
# model fitting data 
ggplot() + 
  geom_sf(data = coastline, fill = "#efe5c7", color = "#816c62") +
  geom_sf(data = strata_utm, fill = NA) +
  geom_point(data = am_spring, aes(X*1000, Y*1000, size = EXPCATCHWT, color = EXPCATCHWT)) +
  scale_color_viridis_c(guide = "legend", trans = "sqrt") + 
  labs(x = "Longitude", y = "Latitude", color = "Biomass", size = "Biomass") + 
  theme(legend.position = "bottom")
# ggsave("atlmackerel-biomass-map.png", last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 8)

# estimates 
plot_map(coastline, strata_utm, dat = grid_preds, column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  labs(fill = "Biomass") + 
  facet_wrap(~EST_YEAR) +
  theme(legend.position = "bottom", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14), legend.title = element_text(size = 14),  legend.text = element_text(size = 12, angle = 45))

# ggsave("atlmackerel-preds-map.png", device = "png", path = here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 8)
# ggsave("atlmackerel-preds-map-all-years.png", device = "png", path = here("sdmtmb", "atlmackerel", "plots"), width = 8, height = 10)

# main effects only
plot_map(coastline, strata_utm, dat = grid_preds, column = exp(est_non_rf)) + labs(fill = "Main effect estimates") + theme(legend.position = "bottom")
ggsave("spring-ME_ests-map.png", device = "png", path = here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 8)

# spatial random effects
plot_map(coastline, strata_utm, dat = grid_preds, column = omega_s) + scale_fill_gradient2() + labs(fill = "Spatial RE\nestimates") + theme(legend.position = "bottom")
ggsave("spring-SRE_ests-map.png", device = "png", path = here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 8)

# spatiotemporal random effects
plot_map(coastline, strata_utm, dat = grid_preds |> filter(EST_YEAR %in% c(2017:2021)), column = epsilon_st) + scale_fill_gradient2() + labs(fill =  "Spatiotemporal RE\nestimates") + facet_wrap(vars(EST_YEAR)) + theme(legend.position = "bottom")
ggsave("spring-ST-RE_ests-map.png", device = "png", path = here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 8)

