### created: 08/21/2023
### last updated: 12/07/2023

# 02 - PLOT ATLANTIC MACKEREL DISTRIBUTION ####


## OBJECTIVE ####
# create a map of the spatial footprint that will be used to filter for spring Atlantic mackerel observations and its overlap with planned and leased offshore wind areas


### LOAD PACKAGES ####
# library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

#sdmtmb.dir <- "../sseep-analysis/sdmtmb"
#sseep.dir <- "../sseep-analysis"

### LOAD DATA ####
# read the strata shapefile
# add in area type similar to the summer flounder map to represent that the full survey area will constitute Atlantic mackerel's distribution used in the analyses 
strata <- readRDS(here("data", "rds", "active_strata.rds")) |> 
  mutate(TYPE = "Distribution Areas")

# Read in the lease and planning wind areas 
wind_areas <- readRDS(here("data", "rds", "wind_areas_062022", "all_wind_areas_Jun2022.rds"))

# read in the northeast coastline
ne_coast <- sf::st_read(dsn = here("gis", "eastern_coast_WGS84.shp"))


## PLOT THE DISTRIBUTION ####
# create a color palette for creating a legend
pal <- c("#5dc5e9", "#0a4c8a", "#3f7f00")

ggplot() +
  geom_sf(data = ne_coast, fill = "#efe5c7", color = "#816c62") +
  geom_sf(data = strata, aes(fill = TYPE), alpha = 0.6) +
  geom_sf(data = wind_areas, aes(color = Area_Type,  fill = Area_Type), alpha = 0.7)+ 
  scale_fill_manual(name = "Legend", values = pal, aesthetics = c("color", "fill"), labels = c("Distribution Areas", "Leased areas", "Planning Areas"))+
  coord_sf() + 
  xlim(c(80, 65)) +
  ylim(c(32, 46)) +
  labs(x = "Longitude", y = "Latitude", fill = "Proportion") +
  theme(legend.position = "bottom", 
        panel.grid = element_blank()) 

ggsave("atlmackerel-wind-overlap.png", plot = last_plot(), device = "png", path = here("outputs", "atlmackerel"), width = 10, height = 8)
