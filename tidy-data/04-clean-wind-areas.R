### created: 04/01/2023
### last updated: 07/03/2023

# 04 - PREPARE WIND AREAS ####

## OBJECTIVE ####
# read and prepare shapefiles of the East Coast wind areas, using the current wind area shapefiles as of January 2023 and downloaded from the BOEM website 

# Script should:
# read in the lease and planning wind area shapefiles
# create one multipolygon shapefile of all planning areas
# create one multipolygon shapefile of all leased areas
# create one multipolygon shapefile of both lease and planning areas
# write an rds, and shapefile for each object 


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
#source()


#### LOAD DATA ####
strata <- readRDS(here("data", "rds", "current_strata.rds")) 
leases <- sf::st_read(here("gis", "WindLeases_2022Dec06.shp")) 
planning <- sf::st_read(here("gis", "WindPlanningAreas_2023Jan10.shp"))

#### DATA WRANGLING ####
# transform the strata crs to match the crs of the wind shapefiles
strata_tran <- st_transform(strata, "WGS84")

#### PLANNING AREAS
# extract the planning areas that only intersect the survey strata; i.e remove areas on the west coast
ne_plan <- planning |>
  st_make_valid() |> #input geometries are wrong and bad spherically; rather than turning off spherical geometry, we correct the geometries  
  sf::st_intersection(st_make_valid(strata_tran))

# create a single multipolygon that encompasses all individual planning areas
ne_plan_union <- st_union(ne_plan)

# plot it 
ggplot(ne_plan_union) + geom_sf()

### save the individual planning areas 
st_write(ne_plan, here("gis", "ne_wind_planning2023.shp"))

#### LEASE AREAS
# filter out lease areas occurring on the west coast 
ne_leases <- leases |>
  filter(STATE != "CA") |>
  st_make_valid()

# create a single polygon that encompasses all individual lease areas
ne_leases_union <- st_union(ne_leases)

# plot it 
ggplot(ne_leases_union) + geom_sf()

### save the individual lease areas 
st_write(ne_leases, here("gis", "ne_wind_leases2023.shp"))

#### MERGE WIND AREAS 
# combine unioned planning and lease areas to create one full wind area reference object
wind_areas1 <- append(ne_leases_union, ne_plan_union) |>
  st_as_sf()

# bind planning and lease areas to have in one dataframe 
wind_areas2 <- bind_rows(ne_leases, ne_plan) |>
  st_as_sf()

#plot it 
ggplot() +
  geom_sf(data = wind_areas1)
ggplot() +
  geom_sf(data = wind_areas2)

### save the data 
st_write(wind_areas1, here("gis", "wind_areas_merge2023.shp"))
st_write(wind_areas2, here("gis", "all_wind_areas_2023.shp"))
