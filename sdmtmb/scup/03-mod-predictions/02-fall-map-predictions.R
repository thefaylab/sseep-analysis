### created: 04/18/2023
### last updated: 01/05/2025

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
fall_preds_tw_1 <- readRDS(file = here("sdmtmb", "scup", "data", "fall_grid_preds_tw_1.rds"))
fall_preds_tw_2 <- readRDS(file = here("sdmtmb", "scup", "data", "fall_grid_preds_tw_2.rds"))
fall_preds_tw_m1 <- readRDS(file = here("sdmtmb", "scup", "data", "fall_grid_preds_tw_m1.rds"))
fall_preds_tw_m2 <- readRDS(file = here("sdmtmb", "scup", "data", "fall_grid_preds_tw_m2.rds"))

## FALL MAPS tweedie####
# estimates 
fall_pred_tw_map_1 <- plot_map(coastline, strata_utm, dat = fall_preds_tw_1 |> 
                               filter(EST_YEAR %in% c(2009:2021)), column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  labs(fill = "Biomass") +
  facet_wrap(~str_to_title(EST_YEAR)) + 
  theme(legend.position = "right", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), 
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), 
        axis.text = element_text(size = 14), 
        strip.text = element_text(size = 14), 
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12))

fall_pred_tw_map_2 <- plot_map(coastline, strata_utm, dat = fall_preds_tw_2 |> 
                                 filter(EST_YEAR %in% c(2009:2021)), column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  labs(fill = "Biomass") +
  facet_wrap(~str_to_title(EST_YEAR)) + 
  theme(legend.position = "right", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), 
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), 
        axis.text = element_text(size = 14), 
        strip.text = element_text(size = 14), 
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12))

fall_pred_tw_map_m1 <- plot_map(coastline, strata_utm, dat = fall_preds_tw_m1 |> 
                                 filter(EST_YEAR %in% c(2009:2021)), column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  labs(fill = "Biomass") +
  facet_wrap(~str_to_title(EST_YEAR)) + 
  theme(legend.position = "right", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), 
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), 
        axis.text = element_text(size = 14), 
        strip.text = element_text(size = 14), 
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12))

fall_pred_tw_map_m2 <- plot_map(coastline, strata_utm, dat = fall_preds_tw_m2 |> 
                                 filter(EST_YEAR %in% c(2009:2021)), column = exp(est)) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  labs(fill = "Biomass") +
  facet_wrap(~str_to_title(EST_YEAR)) + 
  theme(legend.position = "right", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), 
        axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), 
        axis.text = element_text(size = 14), 
        strip.text = element_text(size = 14), 
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12))


ggsave("fall_pred_tw_map_1.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)
ggsave("fall_pred_tw_map_2.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)
ggsave("fall_pred_tw_map_m1.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)
ggsave("fall_pred_tw_map_m2.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

# main effects only

fall_main_1 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_1, column = exp(est_non_rf)) +
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_main_1.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_main_2 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_2, column = exp(est_non_rf)) +
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_main_2.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_main_m1 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_m1, column = exp(est_non_rf)) +
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_main_m1.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_main_m2 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_m2, column = exp(est_non_rf)) +
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_main_m2.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

# spatial random effects
fall_rree_1 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_1, column = omega_s, size=5) + scale_fill_gradient2() + 
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_rree_1.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_rree_2 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_2, column = omega_s, size=5) + scale_fill_gradient2() + 
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_rree_2.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_rree_m1 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_m1, column = omega_s, size=5) + scale_fill_gradient2() + 
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_rree_m1.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_rree_m2 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_m2, column = omega_s, size=5) + scale_fill_gradient2() + 
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_rree_m2.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)


# spatiotemporal random effects

fall_st_rree_1 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_1, column = epsilon_st, size=10) + 
  scale_fill_gradient2()  + 
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_st_rree_1.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_st_rree_2 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_2, column = epsilon_st, size=10) + 
  scale_fill_gradient2()  + 
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_st_rree_2.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_st_rree_m1 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_m1, column = epsilon_st, size=10) + 
  scale_fill_gradient2()  + 
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_st_rree_m1.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)

fall_st_rree_m2 <- plot_map(coastline,strata_utm, dat = fall_preds_tw_m2, column = epsilon_st, size=10) + 
  scale_fill_gradient2()  + 
  facet_wrap(~str_to_title(EST_YEAR)) 
ggsave("fall_st_rree_m2.png", device = "png", path = here("sdmtmb", "scup", "plots"), width = 8, height = 8)



