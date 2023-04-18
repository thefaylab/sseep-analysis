### created: 04/12/2023
### last updated:04/17/2023

#### 04 - MERGE GRIDS ####

###################
#### OBJECTIVE ####
###################
# identify grid cells as occuring inside or outside wind areas. 
# combine outside and wind grids to have a grid over the full survey extent

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
outside_grid <- readRDS(file = here(sdmtmb.dir, "data", "outside_grid.rds"))
wind_grid <- readRDS(file = here(sdmtmb.dir, "data", "wind_grid.rds"))

survey_grid <- readRDS(file = here(sdmtmb.dir, "data", "survey_grid.rds"))

#### DATA WRANGLE ####

outside_grid <- outside_grid |>
  mutate(AREA_CODE = 2,
         AREA = "OUTSIDE") 

wind_grid <- wind_grid |>
  mutate(AREA_CODE = 1,
         AREA = "WIND") 


#### BIND GRIDS ####

impacted_grid <- bind_rows(outside_grid, wind_grid) #|>
  #sdmTMB::add_utm_columns(ll_names = c("lon", "lat"), utm_crs = 32618)

# impacted_grid_sf <- impacted_grid |>
#   st_as_sf(coords = c("X", "Y")) |>
#   st_set_crs(32618)


ggplot(impacted_grid, aes(X, Y)) +
  geom_point() +
  scale_fill_viridis_c() +
  coord_equal()

saveRDS(impacted_grid, here(sdmtmb.dir, "data", "impacted_grid.rds"))
