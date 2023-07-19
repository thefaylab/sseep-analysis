### created: 11/09/2022
### last updated: 02/24/2022

# 05b - INDEX WIND TOWS ####

## OBJECTIVE ####
# find the area of overlap imposed by offshore wind area on the bottom trawl survey strata

# script will: 
# find the intersection between offshore wind area and a given survey strata
# calculate the percent of area overlapped, or percent of area impacted upon survey strata by offshore wind areas


### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(sf)
library(here)
suppressPackageStartupMessages(library(tidyverse))
#install.packages()

### LOAD DATA ####
strata <- readRDS(here("data", "rds", "active_strata.rds"))

# read in wind areas where they are one large polygon
wind_areas <- readRDS(here("data", "rds", "wind_areas_082021", "merged_wind_areas_Aug2021.rds"))


### DATA WRANGLE ####
# turn into UTMs first
strata_utm <- sf::st_transform(strata, crs = 32618)
wind_utm <- sf::st_transform(wind_areas, crs = 32618)

# check that the crs transformation was agreeable
sf::st_crs(strata_utm)
sf::st_crs(wind_utm)

ggplot() +
  geom_sf(data = strata_utm) + 
  geom_sf(data = wind_utm, fill = "lightblue", alpha = 0.5)

# calculates the area of each polygon 
strata_utm <- strata_utm |>
  mutate(AREA_SqM = st_area(strata_utm)) # in meters

# wind_area <- st_area(wind_utm)

# merge both the survey area and wind areas to create one polygon each
strat_union <- st_union(strata_utm)
wind_union <- st_union(wind_utm)

# removes the spatial area of the wind energy areas from the single survey area
shp_intersect <- sf::st_difference(strat_union, wind_union)

# remove the wind energy areas from each strata polygon based on the intersection with the full survey area minus the wind areas 
strat_crop <- st_intersection(strata_utm, shp_intersect) 

# calculate the remaining area of each strata and the percent difference to represent the area impacted 
area_impacted <- strat_crop |> 
  mutate(AREA_SqM_remain = st_area(strat_crop), # calculate remaining areas in each polygon
         percent_area_impacted = round(as.numeric(((AREA_SqM - AREA_SqM_remain)/AREA_SqM)*100), 0)) # subtract remaining area from total area and divide by total area to find percent impacted 

# plot it
ggplot() + geom_sf(data = area_impacted, aes(fill = percent_area_impacted)) + scale_fill_distiller(palette = "Spectral")

# create an sf only of the strata and the percent impacted for joining to original strata dataframe
impacts <- area_impacted |>
  select(STRATUM, percent_area_impacted)

# spatial join both sf's, returning "strata_utm" augmented with fields in "impacts" that have the largest overlap
strata_impacts <- st_join(strata_utm, impacts, largest = TRUE) |>
  select(!STRATUM.y) |> 
  rename(STRATUM = STRATUM.x)

#plot it
ggplot() + geom_sf(data = strata_impacts, aes(fill = percent_area_impacted)) + scale_fill_distiller(palette = "Spectral") +
  labs(x = "Longitude", y = "Latitude", fill = "Percent Area Impacted") +
  theme_bw() + 
  theme(legend.position = "bottom")

ggsave(filename = "strata_impacts.png", plot = last_plot(), device = "png", width = 6, height = 8, path = here("outputs"))

# pull out only the strata that are impacted
impacted_strata <- strata_impacts |> 
  filter(percent_area_impacted > 0) |> 
  select(STRATUM) |> 
  sf::st_set_geometry(NULL)


# save the new shapefiles and rds objects 
saveRDS(strata_impacts, here("data", "rds", "strata_impacts.rds"))
saveRDS(impacted_strata, here("data", "rds", "impacted_strata.rds"))
# sf_save(strata_shp, here("gis", ))
# sf_save(wind_shp, here("gis", ))
