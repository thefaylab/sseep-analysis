### created: 12/10/2022
### last updated: 

#### 01 - PREPARE DATA ####

###################
#### OBJECTIVE ####
###################
# prepare data for sdmTMB model fits and predicting

####################


#### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
# library(marmap)
# library(raster)

here()

#### LOAD DATA ####
# tidied bottom trawl data created from `01-tidy-data-script.R` here("tidy-data"). Contains present only observations and will be used to pull depth values for each station below.  
bts <- readRDS(here("data", "rds", "tidy-bts.rds"))

# dataset created from `02-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow. 
data <-readRDS(here("data", "rds", "merged_data_complete.rds"))

# Catalina's version of the tidied data containing management areas. Will be used to filter summer flounder distributions and tows by MAB
geounits <- read.csv(here("data", "temp", "mergedpresence3.csv"), sep = ";")

# grid of points covering the spatial footprint of the NEFSC Survey strata. Created in ArcGIS Pro, with the survey strata shapefile and the Create fishnet tool.
#nefsc_grid <- sf::st_read(dsn = here("gis", "temp", "nefsc_grid", "nefsc_grid.shp"))


#### SOME DATA TIDYING ####
# extract depth values by unique tow and year 
depth_vals <- bts %>% 
  group_by(STRATUM, CRUISE6, STATION, YEAR) %>%
  select(STRATUM, CRUISE6, STATION, AVGDEPTH) %>% 
  unique() %>% # find unique observations of tow and depth combination
  mutate(code = str_c(STRATUM, CRUISE6, STATION)) # create code for unique tow for joining data later

# extract management area units by unique tow 
geounits <- geounits %>% 
  select(STRATUM, CRUISE6, STATION, GEO_AREA) %>% 
  group_by(STRATUM, CRUISE6, STATION) %>% 
  distinct() %>% # find unique observations of tow and depth combination 
  mutate(code = str_c(STRATUM, CRUISE6, STATION)) # create code for unique tow for joining data later

# join depth values and geounits together to have stratum, station, and tow depths according to management area
add_info <- left_join(depth_vals, geounits, by = "code") %>% 
  select(!c(STRATUM.y, CRUISE6.y, STATION.y)) %>% # remove duplicate columns
  rename(STRATUM = STRATUM.x, # rename columns
         CRUISE6 = CRUISE6.x, 
         STATION = STATION.x)

#### PREPARE SUMMER FLOUNDER DATA ####
sumflounder <- data %>% 
  filter(SVSPP == 103) %>% # filter by summer flounder species code
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) %>% # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION) %>% 
  mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>% # create code for unique tow
  left_join(add_info, by = "code") %>% # add depth and geounit data by code
  ungroup() %>% 
  select(!c(STRATUM.y, CRUISE6.y, STATION.y, YEAR)) %>% # remove duplicate columns
  rename(STRATUM = STRATUM.x, # rename columns
         CRUISE6 = CRUISE6.x, 
         STATION = STATION.x, 
         depth = AVGDEPTH,
         year = EST_YEAR) %>% 
  filter(GEO_AREA == "MAB") # filter observations for those occurring in the Mid-Atlantic Bight 

# remove NA values in depth
sumflounder <- sumflounder %>% 
  filter(!is.na(depth))

# create fall and spring datasets 
# sf_fall <- sumflounder %>% filter(SEASON == "FALL")
# sf_spring <- sumflounder %>% filter(SEASON == "SPRING")

# save data 
saveRDS(sumflounder, here("sdmtmb", "data", "sumflounder.rds"))
# saveRDS(sf_fall, here("sdmtmb", "data", "sumflounder_fall.rds"))
# saveRDS(sf_spring, here("sdmtmb", "data", "sumflounder_spring.rds"))


#### CONSTRUCT MESH #### 
mesh <- make_mesh(sumflounder, xy_cols = c("X", "Y"), cutoff = 10) 
#cutoff defines the minimum allowed distance between points in the units of X and Y (km)

#mesh$mesh$n 
plot(mesh)

# save mesh
saveRDS(mesh, here("sdmtmb", "data", "mesh.rds"))


#### MAKE GRID #### 
# # grid to be used for predicting from best model fit 
# nefsc_grid <- nefsc_grid %>%
#   sf::st_set_geometry(NULL) %>% # drop the geometry in order to create dataframe
#   sdmTMB::add_utm_columns(c("LONG", "LAT")) # convert coordinates to UTM
# 
# # view the grid
# ggplot(nefsc_grid) +
#   geom_point(aes(LONG, LAT))
# 
# ggsave(filename = "nefsc_grid.png", device = "png", path = here("sdmtmb", "plots"), width = 12, height = 10)
# 
# # create a fall grid
# fall_grid <- nefsc_grid %>%
#   inner_join(sf_fall, by = "STRATUM") %>%
#   #rename(depth = AVGDEPTH,
#   #       year = YEAR) %>%
#   filter(SEASON == "FALL") %>%
#   select(LONG, LAT, X.x, Y.y, CRUISE6, STATION, year, SEASON, depth) %>%
#   rename(X = X.x,
#          Y = Y.y)
# # create a spring grid
# spring_grid <- nefsc_grid %>%
#   inner_join(sf_spring, by = "STRATUM") %>%
#   #rename(depth = AVGDEPTH,
#   #       year = YEAR) %>%
#   filter(SEASON == "SPRING") %>%
#   select(LONG, LAT, X.x, Y.y, CRUISE6, STATION, year, SEASON, depth) %>%
#   rename(X = X.x,
#          Y = Y.y)
# # save grid
# saveRDS(nefsc_grid, here("sdmtmb", "data", "nefsc_grid.rds"))
# # 
