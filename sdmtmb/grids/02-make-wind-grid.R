### created: 04/01/2023
### last updated:04/17/2023

#### 02 - MAKE GRID INSIDE WIND AREAS ####

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

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

#### LOAD DATA ####
strata <- sf::st_read(here(sseep.dir, "gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) |>
  rename(STRATUM = "Strata_Num")

# read in wind areas where they are one large polygon
wind_areas <- sf::st_read(here(sseep.dir, "gis", "wind_areas_merge2023.shp")) 

#### DATA WRANGLING ####
# transform the strata crs to match the crs of the wind shapefiles
strata_tran <- st_transform(strata, "WGS84")

# IMPORTANT STEP: turn into UTMs first
wind_utm <- sf::st_transform(wind_areas, crs = 32618)
 
strata_utm <- sf::st_transform(strata_tran, crs = 32618)

sf::st_crs(wind_utm)

ggplot() +
  geom_sf(data = wind_utm)

#### Make Grid ####
# choose a grid size in units of our polygon shape file
grid_spacing <- 10000 # also matches survey grid resolution

# create grid over the bounding box of the polygon
full_grid <- sf::st_make_grid(
  wind_utm,
  cellsize = c(grid_spacing, grid_spacing), 
  square = FALSE #creates hexagonal grid rather than square grid
) |>
  sf::st_sf()


# plot the grid
ggplot(full_grid) + geom_sf()

full_grid|>
  sf::st_centroid() |> # find the centroid of each cell
  sf::st_coordinates() |> # obtain the coordinates 
  as_tibble() |> # create tibble 
  ggplot(aes(X, Y)) + geom_tile(width = grid_spacing, 
                                height = grid_spacing, 
                                colour = "grey40", fill = "white") # plot

# subset our grid to cells that intersect our polygon:
intersected <- sf::st_intersects(full_grid, wind_utm)
selected_grid <- full_grid[lengths(intersected) > 0, ] #1171
 
selected_grid|>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  ggplot(aes(X, Y)) + geom_tile(width = grid_spacing, height = grid_spacing, colour = "grey40", fill = "white")

# selected_grid2 <- st_intersection(full_grid, wind_utm) #1190

# plot it
ggplot() +
  geom_sf(data = wind_utm) + 
  geom_sf(data = selected_grid, fill = NA)

# join grid cells with strata by st_intersects to identify the strata in which each cell falls 
join_strat <- sf::st_join(selected_grid, strata_utm, largest = TRUE)

# if using to predict and calculate an index, calculate the area of each grid cell
# find how much of each grid cell is within the outer polygon:
# IMPORTANT STEP HERE!! take the union of your strata:
wind_utm_union <- sf::st_union(wind_utm)
# 
# ggplot(strata_utm_union) + geom_sf()
# 
overlap <- sf::st_intersection(selected_grid, wind_utm_union) 
nrow(overlap)
nrow(selected_grid)
 
ggplot(overlap) + geom_sf()
 
calculated_area <- sf::st_area(overlap) 
length(calculated_area)

# find the center points of each grid cell, extract the coordinates, and add the area values
coord <- join_strat|>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as_tibble() |>
  mutate(area_m2 = as.integer(calculated_area), 
         across(c(X, Y), round, digits = 2)) 

# plot it 
ggplot(coord, aes(X, Y, fill = area_m2)) +
  geom_tile(width = grid_spacing, height = grid_spacing, colour = "grey10") +
  scale_fill_viridis_c() +
  coord_equal()

### save the data 
saveRDS(coord, here(sdmtmb.dir, "data", "wind_coords.rds"))

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
wind_grid <- bind_cols(coord, depths, join_strat) |> 
  select(X, Y, lon, lat, depth, STRATUM, area_m2)

### save the data 
saveRDS(wind_grid, here(sdmtmb.dir, "data", "wind_grid.rds"))


