### created: 04/01/2023
### last updated: 04/17/2023

#### 03 - MAKE GRID OUTSIDE WIND AREAS ####

###################
#### OBJECTIVE ####
###################
# create a grid of our survey area outside of the wind areas  
# identify a grid cell based on its occurrence within a given stratum
# find the depth within each grid cell

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


#### LOAD DATA ####
# read in the survey grid created from here(sseep.dir, "sdmtmb", "grids", "01-make-survey-grid.R")
survey_grid <- readRDS(file = here(sdmtmb.dir, "data", "survey_grid.rds"))

# read in the wind grid created from here(sseep.dir, "sdmtmb", "grids", "02-make-wind-grid.R")
wind_grid <- readRDS(file = here(sdmtmb.dir, "data", "wind_grid.rds"))

# read the single wind area polygon created from here(sseep.dir, "tidy-data", "clean-wind-areas.R")
wind_areas <- sf::st_read(here(sseep.dir, "gis", "wind_areas_merge2023.shp")) 

# read in survey strata
strata <- sf::st_read(here(sseep.dir, "gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) |>
  rename(STRATUM = "Strata_Num") 

#### DATA WRANGLING ####
# transform the strata crs 
strata <- sf::st_transform(strata, "WGS84")
strata_utm <- sf::st_transform(strata, crs = 32618)

# dissolve strata lines to create one large survey area polygon 
strata_utm_union <- st_union(strata_utm)

# transform the crs of the wind areas 
wind_utm <- sf::st_transform(wind_areas, crs = 32618)
# remove further inner boundaries of wind areas
wind_utm_union <- st_union(wind_utm)

# plot it  
ggplot() +
  geom_sf(data = strata_utm_union) + 
  geom_sf(data = wind_utm, color = "blue")

# subtract the wind areas from the survey area 
strat_diff <- sf::st_difference(strata_utm_union, wind_utm_union)

# plot it 
ggplot() + 
  geom_sf(data = strat_diff)

#### MAKE GRID ####
# choose a grid size in units of our polygon shape file
grid_spacing <- 10000 # same resolution as our survey and wind grids

# create grid over the bounding box of the polygon
full_grid <- sf::st_make_grid(
  strat_diff,
  cellsize = c(grid_spacing, grid_spacing),
  square = FALSE #creates hexagonal grid rather than square grid
) |>
  sf::st_sf()

# plot the grid
ggplot(full_grid) + geom_sf()

full_grid|>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  ggplot(aes(X, Y)) + geom_tile(width = grid_spacing, height = grid_spacing, colour = "grey40", fill = "white")

# subset our grid to cells that intersect our polygon:
intersected <- sf::st_intersects(full_grid, strat_diff)

selected_grid <- full_grid[lengths(intersected) > 0, ]

# plot it
selected_grid|>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  ggplot(aes(X, Y)) + geom_tile(width = grid_spacing, height = grid_spacing, colour = "grey40", fill = "white")

ggplot() +
  geom_sf(data = strat_diff) + 
  geom_sf(data = selected_grid, fill = NA) 

# join grid cells with strata by st_intersects to identify the strata in which each cell falls 
join <- sf::st_join(selected_grid, strata_utm, largest = TRUE)

ggplot(join) + geom_sf()


# find the center points of each grid cell, extract the coordinates, and add the area values
coord <- join|>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  bind_cols(join) |>
  select(X, Y, STRATUM) |>
  mutate(across(c(X, Y), round, digits = 2)) #|>


ggplot(coord, aes(X, Y)) +
  geom_tile(width = grid_spacing, height = grid_spacing, colour = "grey10") +
  scale_fill_viridis_c() +
  coord_equal()

saveRDS(coord, here(sdmtmb.dir, "data", "outside_coords.rds"))

#### BATHY ####
# use the marmap package to pull the bathymetric data from NOAA's database based on our survey area
bts <- getNOAA.bathy(lon1 = -80, lon2 = -60,
                     lat1 = 32, lat2 = 46,
                     resolution = 0.25,
                     keep = TRUE)
# convert utms back to lat longs in order to extract the depth data
grid_crs <- utm2lonlat(coord$X, coord$Y, zone = 18, hemisphere = "N")
#sf::st_crs(fall_strat_utm)

# extract depth data
depths <- get.depth(bts, grid_crs, locator = FALSE)

# bind depth to coordinate grid
grid <- bind_cols(coord, depths) |>
  select(X, Y, STRATUM, lon, lat, depth)

### save the data 
saveRDS(grid, here(sdmtmb.dir, "data", "outside_grid.rds"))




