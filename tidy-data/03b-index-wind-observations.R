### created: 11/09/2022
### last updated: 07/24/2022

# 03b - INDEX WIND OBSERVATIONS ####

## OBJECTIVE ####
# identify historical observations as spatially-intersecting offshore wind energy areas by creating a wind id index and/or code. 

# script will: 
# find the historical survey observations that spatially intersect with offshore wind energy areas 
# add a wind ID and/or value to the data for the observations that were found as intersect with wind energy areas 
# add an ID and/or value to the data for observations that were found as not intersecting offshore wind energy areas


### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(sf)
library(here)
suppressPackageStartupMessages(library(tidyverse))
#install.packages()

### LOAD DATA ####

# dataset created from `02-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow. 
data <- readRDS(file = here("data", "rds", "tidy-data", "tidy-full-bts.rds"))

# read in the currently sampled BTS strata shapefile, saved as rds here("tidy-data", "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata.rds"))

# read in the merged wind energy areas 
wind_areas <- readRDS(here("data", "rds", "wind_areas_062022", "merged_wind_areas_Jun2022.rds"))


### DATA WRANGLE ####

# convert data to simple feature for manipulation and give same coordinate system as wind areas and strata
data_sf <- st_as_sf(data, coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"))
# assign same coordinate system to observation data as strata data 
st_crs(data_sf) <- st_crs(strata)
st_crs(wind_areas) <- st_transform(wind_areas, crs = st_crs(strata))

# plot it 
ggplot() + 
  geom_sf(data = strata) + 
  geom_sf(data = wind_areas)


# merge both the survey area and wind areas to create one polygon each
strat_union <- st_union(strata)
wind_union <- st_union(wind_areas)


## IDENTIFY WIND-INTERSECTING OBSERVATIONS ####

# identify bts observations that intersect a given wind energy areas; if intersects, values > 0
data_sf$AREA_CODE <- st_intersects(data_sf, wind_union) |> 
  as.integer() |>
  dplyr::coalesce(2L) # find NAs within column and replace with the number 2 to represent the "outside wind area" ID

# add column that characterizes the AREA value 
data_sf <- data_sf |>
  mutate(AREA = case_when(
    AREA_CODE == 1 ~ "WIND", 
    TRUE ~ "OUTSIDE"
  ))

# plot it 
ggplot() + 
  geom_sf(data = strata) + 
  geom_sf(data = wind_areas, fill = "red") +
  geom_sf(data = data_sf |> filter(AREA_CODE ==1), color = "white", alpha = 0.5)


# coerce to a dataframe 
data <- sf::st_set_geometry(data_sf, NULL) 


# save the new shapefiles and rds objects
saveRDS(data, here("data", "rds", "tidy-data", "full-bts-indexed.rds"))

