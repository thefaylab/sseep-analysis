### created: 04/18/2023
### last updated: 04/05/2024

# 02 - MAKE PRETTY MAPS ####

## OBJECTIVE ####
# project predictions onto map with Eastern Coast for frame of reference


### LOAD PACKAGES ####
library(sf)
library(patchwork)
library(here)
library(sdmTMB)
library(NatParksPalettes)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
theme <- theme_bw(base_size = 14) + theme(legend.position = "bottom", legend.text = element_text(angle = 45))
theme_set(theme)
pal <- natparks.pals("DeathValley", type = "continuous")


# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
#sseep.dir <- "../sseep-analysis"
source(here("sdmtmb", "functions", "plot_fns.R"))
dat.files <- here("data", "rds", "sdmtmb", "sumflounder") 
plot.files <- here("outputs", "sdmtmb",  "sumflounder", "plots") 

### LOAD DATA ####
# read in predictions created here("mod-forecasts.R")
fall_preds <- readRDS(file = here(dat.files, "fall_grid_preds.rds"))

spring_preds <- readRDS(file = here(dat.files, "spring_grid_preds.rds"))


## FALL MAPS ####
# estimates 
fall_pred_map <- plot_map(coastline, strata_utm, dat = fall_preds |> filter(EST_YEAR %in% c(2016:2021)), column = exp(est)) + 
  scale_fill_gradient2(low = pal[4], mid = pal[3], high = pal[1], midpoint = 10, trans = "sqrt") +
  labs(fill = "Biomass") +
  facet_grid(cols = vars(str_to_title(SEASON), EST_YEAR)) 

# ggsave("fall-preds-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# main effects only
fall_fixed <- plot_map(coastline, strata_utm, dat = fall_preds |> filter(EST_YEAR %in% c(2016:2021)), column = exp(est_non_rf)) +
  scale_fill_gradient2(low = pal[4], mid = pal[5], high = pal[3], midpoint = 0.10, trans = "sqrt") + 
  labs(fill = "Main effect estimates") +
  facet_grid(cols = vars(str_to_title(SEASON), EST_YEAR)) 
# ggsave("fall_ME_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)
# 
# # spatial random effects
fall_omega <- plot_map(coastline, strata_utm, dat = fall_preds |> filter(EST_YEAR %in% c(2016:2021)), column = omega_s) + 
  scale_fill_gradient2(low = pal[4], mid = "white", high = pal[2], midpoint = 0) + 
  labs(fill = "Biomass") +
  facet_grid(cols = vars(str_to_title(SEASON))) 

# ggsave("fall-SRE_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)
# 
# # spatiotemporal random effects
fall_epsilon <- plot_map(coastline, strata_utm, dat = fall_preds |> filter(EST_YEAR %in% c(2016:2021)), column = epsilon_st) + 
  scale_fill_gradient2(low = pal[4], mid = "white", high = pal[2], midpoint = 0) + 
  labs(fill =  "Spatiotemporal RE\nestimates") + 
  facet_grid(cols = vars(str_to_title(SEASON), EST_YEAR)) 
# ggsave("fall_ST-RE_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)


## SPRING MAPS ####
# estimates 
spring_pred_map <- plot_map(coastline, strata_utm, dat = spring_preds |> filter(EST_YEAR %in% c(2017:2021)), column = exp(est)) + 
  scale_fill_gradient2(low = pal[4], mid = pal[3], high = pal[1], midpoint = 10, trans = "sqrt") +
  labs(fill = "Biomass") +
  facet_grid(cols = vars(str_to_title(SEASON), EST_YEAR)) 

# ggsave("spring-preds-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# main effects only
spring_fixed <- plot_map(coastline, strata_utm, dat = spring_preds |> filter(EST_YEAR %in% c(2017:2021)), column = exp(est_non_rf)) +
  scale_fill_gradient2(low = pal[4], mid = pal[5], high = pal[3], midpoint = 0.10, trans = "sqrt") + 
  labs(fill = "Main effect estimates") +
  facet_grid(cols = vars(str_to_title(SEASON), EST_YEAR)) 
# ggsave("spring-ME_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)
# 
# # spatial random effects
spring_omega <- plot_map(coastline, strata_utm, dat = spring_preds |> filter(EST_YEAR %in% c(2017:2021)), column = omega_s) + 
  scale_fill_gradient2(low = pal[4], mid = "white", high = pal[2], midpoint = 0) + 
  labs(fill = "Biomass") +
  facet_grid(cols = vars(str_to_title(SEASON))) 
# ggsave("spring-SRE_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)
# 
# # spatiotemporal random effects
spring_epsilon <- plot_map(coastline, strata_utm, dat = spring_preds |> filter(EST_YEAR %in% c(2017:2021)), column = epsilon_st) + 
  scale_fill_gradient2(low = pal[4], mid = "white", high = pal[2], midpoint = 0) + 
  labs(fill =  "Spatiotemporal RE\nestimates") + 
  facet_grid(cols = vars(str_to_title(SEASON), EST_YEAR)) 
# ggsave("spring-ST-RE_ests-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 6, height = 8)

# patchwork
pred_maps <- (fall_pred_map / spring_pred_map) + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")
# ggsave("seasonal-preds-map.png", device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 9, height = 10)
fixed_maps <- (fall_fixed / spring_fixed) + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

omega_maps <- (fall_omega + spring_omega) + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

epsilon_maps <- (fall_epsilon / spring_epsilon) + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

plots <- list("prediction_map.png" = pred_maps, 
              "main_effects_map.png" = fixed_maps, 
              "spatial_re_map.png" = omega_maps, 
              "spatiotemporal_map.png" = epsilon_maps)
pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = .y, device = "png", path = here(plot.files), width = 9, height = 10))

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

