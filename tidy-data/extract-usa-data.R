### created:      4/20/2023
### last update:  
###

# EXTRACT EAST COAST DATA FOR MAPS ####

#### OBJECTIVE ####
# create a shapefilfe of the east coast for mapping and plotting 


# LOAD PACKAGES #####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(rnaturalearth)
library(rnaturalearthdata)


# DATA SET UP #####
# extract data only from the United States
state_data <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf") |>
  filter(region %in% c("Northeast", "South"))

# crop the state data to the eastern coast
ne_states <- suppressWarnings(suppressMessages(
  st_crop(state_data,
          c(xmin = -80, ymin = 32, xmax = -60, ymax = 46))))

# project the coast to UTMs
utm_zone18 <- 32618
ne_states_proj <- sf::st_transform(ne_states, crs = utm_zone18)
sf::st_boundary(ne_states_proj)

# SAVE DATA #####
st_write(state_data, dsn = here("gis", "us_states.shp"))
st_write(ne_states, dsn = here("gis", "eastern_coast_WGS84.shp"))
st_write(ne_states_proj, dsn = here("gis", "eastern_coast_UTM.shp"))




