### created: 04/18/2023
### last updated: 12/23/2023

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


# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
#sseep.dir <- "../sseep-analysis"
source(here("sdmtmb", "R", "plot_fns.R"))

### LOAD DATA ####
# read in predictions created here("mod-forecasts.R")
fall_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "fall_grid_preds.rds"))

spring_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "spring_grid_preds.rds"))


## FALL MAPS ####
# estimates 
fall_pred_map <- plot_map(coastline, strata_utm, dat = fall_preds |> filter(EST_YEAR %in% c(2018:2021)), column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  labs(fill = "Biomass") +
  facet_wrap(~str_to_title(SEASON)) + 
  theme(legend.position = "bottom", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14), legend.title = element_text(size = 14),  legend.text = element_text(size = 12))

ggsave("fall-preds-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# main effects only
plot_map(coastline, strata_utm, dat = fall_preds, column = exp(est_non_rf)) + labs(fill = "Main effect estimates") + facet_wrap(~SEASON) + theme(legend.position = "bottom")
ggsave("fall-ME_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# spatial random effects
plot_map(coastline, strata_utm, dat = fall_preds, column = omega_s) + scale_fill_gradient2() + labs(fill = "Spatial RE\nestimates") + facet_wrap(~SEASON) + theme(legend.position = "bottom")
ggsave("fall-SRE_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# spatiotemporal random effects
plot_map(coastline, strata_utm, dat = fall_preds |> filter(EST_YEAR %in% c(2016:2021)), column = epsilon_st) + scale_fill_gradient2() + labs(fill =  "Spatiotemporal RE\nestimates") + facet_wrap(vars(SEASON, EST_YEAR)) + theme(legend.position = "bottom")
ggsave("fall-ST-RE_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)


## SPRING MAPS ####
# estimates 
spring_pred_map <- plot_map(coastline, strata_utm, dat = spring_preds |> filter(EST_YEAR %in% c(2018:2021)), column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  labs(fill = "Biomass") + 
  facet_wrap(~str_to_title(SEASON)) + 
  theme(legend.position = "bottom", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14), legend.title = element_text(size = 14),  legend.text = element_text(size = 12))

ggsave("spring-preds-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# main effects only
plot_map(coastline, strata_utm, dat = spring_preds, column = exp(est_non_rf)) + labs(fill = "Main effect estimates") + facet_wrap(~SEASON) + theme(legend.position = "bottom")
ggsave("spring-ME_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# spatial random effects
plot_map(coastline, strata_utm, dat = spring_preds, column = omega_s) + scale_fill_gradient2() + labs(fill = "Spatial RE\nestimates") + facet_wrap(~SEASON) + theme(legend.position = "bottom")
ggsave("spring-SRE_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# spatiotemporal random effects
plot_map(coastline, strata_utm, dat = spring_preds |> filter(EST_YEAR %in% c(2017:2021)), column = epsilon_st) + scale_fill_gradient2() + labs(fill =  "Spatiotemporal RE\nestimates") + facet_wrap(vars(SEASON, EST_YEAR)) + theme(legend.position = "bottom")
ggsave("spring-ST-RE_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# patchwork
fall_pred_map + spring_pred_map & theme(legend.position = "bottom")
ggsave("seasonal-preds-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

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

