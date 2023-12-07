### created: 08/18/2023
### last updated: 

# 02 - PLOT SUMMER FLOUNDER DISTRIBUTION ####


## OBJECTIVE ####
# create maps of the spatial footprint that will be used to filter for seasonal summer flounder observations and its overlap with planned and leased offshore wind areas

### LOAD PACKAGES ####
# library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

### LOAD DATA ####
# active strata shapefile 
strata <- readRDS(here("data", "rds", "active_strata.rds"))

# lease and planning wind areas 
wind_areas <- readRDS(here("data", "rds", "wind_areas_062022", "all_wind_areas_Jun2022.rds"))

# read in the northeast coastline
ne_coast <- sf::st_read(dsn = here("gis", "eastern_coast_WGS84.shp"))

# fall footprint
fall_footprint <- readRDS(here("data", "sumflounder", "sf_fall_footprint.rds"))

# spring footprint
spring_footprint <- readRDS(here("data", "sumflounder", "sf_spring_footprint.rds"))


## PLOT THE DISTRIBUTION ####
# create a color palette for creating a legend
pal <- c("#5dc5e9", "#0a4c8a", "#3f7f00")

### FALL MAP ####
fall_map <- ggplot() +
  geom_sf(data = ne_coast, fill = "#efe5c7", color = "#816c62") +
  geom_sf(data = strata, fill = NA) +
  geom_sf(data = fall_footprint, aes(fill = TYPE), alpha = 0.6)+ 
  geom_sf(data = wind_areas, aes(fill = Area_Type, color = Area_Type), alpha = 0.7) +
  scale_fill_manual(name = "Legend", values = pal, aesthetics = c("color", "fill"), labels = c("Distribution Areas", "Leased areas", "Planning Areas"))+
  coord_sf() + 
  xlim(c(80, 65)) +
  ylim(c(32, 46)) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom", 
        panel.grid = element_blank(), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10)) +
  facet_wrap(~SEASON)


ggsave("sumflounder-fall-wind-overlap.png", plot = fall_map, device = "png", path = here("outputs", "sumflounder"), width = 8, height = 6)


### SPRING MAP ####
spring_map <- ggplot() +
  geom_sf(data = ne_coast, fill = "#efe5c7", color = "#816c62") +
  geom_sf(data = strata, fill = NA) +
  geom_sf(data = spring_footprint, aes(fill = TYPE), alpha = 0.6)+ 
  geom_sf(data = wind_areas, aes(fill = Area_Type, color = Area_Type), alpha = 0.7) +
  scale_fill_manual(name = "Legend", values = pal, aesthetics = c("color", "fill"), labels = c("Distribution Areas", "Leased areas", "Planning Areas"))+
  coord_sf() + 
  xlim(c(80, 65)) +
  ylim(c(32, 46)) +
  labs(x = "Longitude", y = "Latitude", fill = "Proportion") +
  theme(legend.position = "bottom", 
        panel.grid = element_blank(), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10)) +
  facet_wrap(~SEASON)


ggsave("sumflounder-spring-wind-overlap.png", plot = spring_map, device = "png", path = here("outputs", "sumflounder"), width = 10, height = 8)

### BOTH MAPS ####

fall_map + spring_map + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


ggsave("sumflounder-wind-overlap.png", plot = last_plot(), device = "png", path = here("outputs", "sumflounder"), width = 10, height = 8)
