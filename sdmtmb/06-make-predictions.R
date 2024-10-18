### created: 06/06/2024
### last updated: 06/26/2024

# 06 - MAKE PREDICTIONS ####

## OBJECTIVE ####
# make predictions from the best fitting spatiotemporal model over the spatial footprint of a given species
# extracted fitted values from the model


### LOAD PACKAGES ####
library(sf)
library(patchwork)
library(here)
library(sdmTMB)
library(NatParksPalettes)
suppressPackageStartupMessages(library(tidyverse))
source(here("sdmtmb", "functions", "plot_fns.R"))

### Environment Set Up ####
# set plotting layout 
theme <- theme_bw(base_size = 14) + theme(legend.position = "bottom")
theme_set(theme)
pal <- natparks.pals("DeathValley", type = "continuous")

# season 
season <- "spring"

# species
species <- "atlmackerel"

species_name <- "atlantic mackerel"

### File locations ####
dat.files <- here("data", "rds", "sdmtmb", species) 
plot.files <- here("outputs", "sdmtmb",  species, "plots") 

### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, str_c(season, "mod.rds", sep = "_"))) ## FIXME as needed

# seasonal spatial footprint grid
grid <- readRDS(here(dat.files, str_c(season, "grid_Jun2022.rds", sep = "_"))) 
### check grid coordinates against model fitting coordinates; divide by 1000 if grid coordinates > model fitting coordinates

## MAKE PREDICTIONS ####
### Model Predictions ####
mod_preds <- predict(mod)

### Grid Predictions ####
# replicate grid across all necessary years
grid <- sdmTMB::replicate_df(grid, "EST_YEAR", unique(mod$data$EST_YEAR)) |> 
  mutate(AREA = as.factor(AREA))

# predictions
grid_preds <- predict(mod, newdata = grid)


## PLOT PREDICTIONS ####
# estimates 
pred_map <- #plot_map(coastline, strata_utm, dat = grid_preds, column = exp(est)) +
plot_map(coastline, strata_utm, dat = grid_preds, column = exp(est1+est2)) +
  scale_fill_gradient2(low = pal[4], mid = pal[3], high = pal[1], midpoint = 20, trans = "sqrt") +
  labs(fill = "Biomass", subtitle = str_c("Predictions of", season, species_name, "spatial distribution", sep = " ")) +
  facet_wrap(~EST_YEAR)


# main effects only
fixed.eff_map <- #plot_map(coastline, strata_utm, dat = grid_preds, column = exp(est_non_rf)
  plot_map(coastline, strata_utm, dat = grid_preds, column = exp(est_non_rf1+est_non_rf2)) + 
  scale_fill_gradient2(low = pal[4], mid = pal[5], high = pal[3], midpoint = 0.10, trans = "sqrt") + 
  labs(fill = "Main effect estimates", subtitle = str_c("Predictions of fixed effects on", season, species_name, "spatial distribution", sep = " ")) + 
  facet_wrap(~EST_YEAR) 

# spatial random effects
omega_map <- plot_map(coastline, strata_utm, dat = grid_preds, column = omega_s) +
  scale_fill_gradient2(low = pal[4], mid = "white", high = pal[2], midpoint = 0) + 
  labs(fill = "Spatial RE\nestimates", subtitle = str_c("Predictions of", season, species_name, "spatial random fields", sep = " ")) 

# spatiotemporal random effects
epsilon_map <- plot_map(coastline, strata_utm, dat = grid_preds, column = epsilon_st) +
  scale_fill_gradient2(low = pal[4], mid = "white", high = pal[2], midpoint = 0) +
  labs(fill =  "Spatiotemporal RE\nestimates", subtitle = str_c("Predictions of", season, species_name, "spatiotemporal random fields", sep = " ")) + 
  facet_wrap(~EST_YEAR) 


## SAVE THE DATA ####
data <- list("mod_preds.rds" = mod_preds, 
             "grid_preds.rds" = grid_preds)

pmap(list(data, names(data)), ~saveRDS(.x, here(dat.files, str_c(season, .y, sep = "_")))) # FIXME 

plots <- list("pred-dist_maps.png" = pred_map, 
              "fixed-eff_maps.png" = fixed.eff_map, 
              "omega-re_map.png" = omega_map,
              "epsilon-re_maps.png" = epsilon_map)

pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = str_c(season, .y, sep = "_"), device = "png", path = here(plot.files), width = 15, height = 15))

