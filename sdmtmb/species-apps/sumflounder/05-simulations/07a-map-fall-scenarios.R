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
# library(marmap)
# library(oce)
# library(rnaturalearth)
# library(rnaturalearthdata)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())


# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
#sseep.dir <- "../sseep-analysis"
source(here("sdmtmb", "R", "plot_fns.R"))

### LOAD DATA ####
sim_base_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseGrid.rds"))
sim_increase_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseGrid.rds"))
sim_decrease_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseGrid.rds"))


# Maps 

dec_map <- plot_map(coastline, strata_utm, dat = sim_decrease_grid|>filter(rep ==1000, EST_YEAR == 2026), column = EXPCATCHWT) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Biomass", subtitle = "Reduced") +
  facet_wrap(~SEASON) +
  theme(legend.position = "bottom", plot.subtitle = element_text(hjust = 0.5), legend.text = element_text(angle = 45))

inc_map <- plot_map(coastline, strata_utm, dat = sim_increase_grid|>filter(rep ==1000, EST_YEAR == 2026), column = EXPCATCHWT) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Biomass", subtitle = "Enhanced") +
  facet_wrap(~SEASON) +
  theme(legend.position = "bottom", plot.subtitle = element_text(hjust = 0.5), legend.text = element_text(angle = 45))

base_map <- plot_map(coastline, strata_utm, dat = sim_base_grid|>filter(rep ==1000, EST_YEAR == 2026), column = EXPCATCHWT) +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(fill = "Biomass", subtitle = "Baseline") +
  facet_wrap(~SEASON) +
  theme(legend.position = "bottom", plot.subtitle = element_text(hjust = 0.5), legend.text = element_text(angle = 45))

(base_map + inc_map + dec_map) + theme(legend.position = "bottom")

ggsave("fall_sim_distributions.png", plot = last_plot(), path = here("sdmtmb", "sumflounder", "plots"), width = 10, height = 8)
