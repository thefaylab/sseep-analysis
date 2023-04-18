### created: 03/15/2023
### last updated: 04/17/2023

#### 01 - MAKE SURVEY GRID ####

###################
#### OBJECTIVE ####
###################
# create a grid of the wind areas 
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

#### LOAD DATA ####
strata <- sf::st_read(here("gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) %>%
  rename(STRATUM = "Strata_Num")

#### DATA WRANGLE ####
# IMPORTANT STEP: turn into UTMs first
strata_utm <- sf::st_transform(strata, crs = 32618)

# check that the crs transformation was agreeable
sf::st_crs(strata_utm)

# plot 
ggplot() +
  geom_sf(data = strata_utm)

#### Make Grid with SF ####
# choose a grid size in units of our polygon shape file
grid_spacing <- 10000 

# create grid over the bounding box of the polygon
full_grid <- sf::st_make_grid(
  strata_utm,
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
intersected <- sf::st_intersects(full_grid, strata_utm)

selected_grid <- full_grid[lengths(intersected) > 0, ]

nrow(selected_grid) #4064

selected_grid|>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  ggplot(aes(X, Y)) + geom_tile(width = grid_spacing, height = grid_spacing, colour = "grey40", fill = "white")

# plot it
ggplot() +
  geom_sf(data = strata_utm) +
  geom_sf(data = selected_grid, fill = NA)

# join the grid cells with the strata in order to obtain a stratum for each cell 
join <- sf::st_join(selected_grid, strata_utm, largest = TRUE)

# if using to predict and calculate an index, calculate the area of each grid cell
# find how much of each grid cell is within the outer polygon:
# IMPORTANT STEP HERE!! take the union of your strata:
# strata_utm_union <- sf::st_union(strata_utm)
# 
# ggplot(strata_utm_union) + geom_sf()
# 
# overlap <- sf::st_intersection(selected_grid, strata_utm_union) 
# nrow(overlap)
# nrow(selected_grid)
# 
# ggplot(overlap) + geom_sf()
# 
# calculated_area <- sf::st_area(overlap) 
# length(calculated_area)

# find the center points of each grid cell, extract the coordinates, and add the area values
coord <- join |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  mutate(across(c(X, Y), round, digits = 2))

ggplot(coord, aes(X, Y)) + #, fill = area)) +
  geom_tile(width = grid_spacing, height = grid_spacing, colour = "grey10") +
  scale_fill_viridis_c() +
  coord_equal()
# 
# ggplot(coord, aes(X, Y, fill = area)) +
#   geom_raster() +
#   scale_fill_viridis_c() +
#   coord_equal()

saveRDS(coord, here(sdmtmb.dir, "data", "strata_coords.rds"))

#### EXTRACT BATHYMETRIC DATA ####
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
grid <- bind_cols(coord, depths, join) |>
  dplyr::select(X, Y, lon, lat, depth, STRATUM)

### save the data
saveRDS(grid, here(sdmtmb.dir, "data", "survey_grid.rds"))


