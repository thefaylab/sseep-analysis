### created: 12/10/2022
### last updated: 03/02/2023

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
# raw bottom trawl data; contains present only observations and will be used to pull depth, bottom temperature, and area swept values for each station below.  
raw_data <- read_csv(here("data", "raw-data", "NEFSC_BTS_ALLCATCHES.csv")) |>
  filter(EST_YEAR %in% c(2009:2021)) |>
  mutate(SVSPP = as.integer(SVSPP), 
         STATION = as.integer(STATION), 
         STRATUM = as.integer(STRATUM))

# dataset created from `03-spatial-filter.R` here("tidy-data"). Contains complete observations for summer flounder that makes up 95% of their cumulative biomass. 
data <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |> filter(SVSPP == 103) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# grid of points covering the spatial footprint of the NEFSC Survey strata. Created in ArcGIS Pro, with the survey strata shapefile and the Create fishnet tool.
#nefsc_grid <- sf::st_read(dsn = here("gis", "temp", "nefsc_grid", "nefsc_grid.shp"))


#### SOME DATA TIDYING ####
# extract depth, bottom temperature, and area swept values from the raw  data by using the unique tows that occur in the summer flounder data 
add_info <- semi_join(raw_data, data, by =c("STRATUM", "CRUISE6", "STATION", "SEASON", "EST_YEAR"))  |>
  select(CRUISE6, STATION, STRATUM, AVGDEPTH, BOTTEMP, AREA_SWEPT_WINGS_MEAN_KM2, SEASON, EST_YEAR) |>
  unique()

# extract management area units by unique tow 
# geounits <- geounits %>% 
#   select(STRATUM, CRUISE6, STATION, GEO_AREA) %>% 
#   group_by(STRATUM, CRUISE6, STATION) %>% 
#   distinct() %>% # find unique observations of tow and depth combination 
#   mutate(code = str_c(STRATUM, CRUISE6, STATION)) # create code for unique tow for joining data later

# add_info <- left_join(depth_vals, geounits, by = "code") %>% 
#   select(!c(STRATUM.y, CRUISE6.y, STATION.y)) %>% # remove duplicate columns
#   rename(STRATUM = STRATUM.x, # rename columns
#          CRUISE6 = CRUISE6.x, 
#          STATION = STATION.x)

# join depth, bottom temperature, and area swept values to the summer flounder data
# data <- data |> left_join(add_info, by = c("STRATUM", "CRUISE6", "STATION", "SEASON", "EST_YEAR"))

#### PREPARE SUMMER FLOUNDER DATA ####
data <- data |>
  #filter(SVSPP == 103) %>% # filter by summer flounder species code
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR) |> 
  #mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>% # create code for unique tow
  left_join(add_info, by = c("STRATUM", "CRUISE6", "STATION", "SEASON", "EST_YEAR")) |> # add depth, bottom temp, and area swept
  ungroup() #%>% 
  #select(-SVSPP.y) %>% # remove duplicate columns
  #rename(SVSPP = SVSPP.x)
         # STRATUM = STRATUM.x, # rename columns
         # CRUISE6 = CRUISE6.x, 
         # STATION = STATION.x, 
         # depth = AVGDEPTH,
         # year = EST_YEAR) #%>% 
  #filter(GEO_AREA == "MAB") # filter observations for those occurring in the Mid-Atlantic Bight 

# remove the NA values in depth
na <- filter(data, is.na(AVGDEPTH)) # check for any na values
data <- data |> 
 filter(!is.na(AVGDEPTH)) # keep everything that is not NA

# create fall and spring datasets 
sf_fall <- data %>% filter(SEASON == "FALL")
sf_spring <- data %>% filter(SEASON == "SPRING")

# save data 
saveRDS(data, here("sdmtmb", "data", "sumflounder.rds"))
saveRDS(sf_fall, here("sdmtmb", "data", "sumflounder_fall.rds"))
saveRDS(sf_spring, here("sdmtmb", "data", "sumflounder_spring.rds"))


#### CONSTRUCT MESH #### 
mesh <- make_mesh(data, xy_cols = c("X", "Y"), cutoff = 10) 
fall_mesh <- make_mesh(sf_fall, xy_cols = c("X", "Y"), cutoff = 10)
spring_mesh <- make_mesh(sf_spring, xy_cols = c("X", "Y"), cutoff = 10)
#cutoff defines the minimum allowed distance between points in the units of X and Y (km)

#mesh$mesh$n 
plot(mesh)
plot(fall_mesh)
plot(spring_mesh)

# save mesh
saveRDS(mesh, here("sdmtmb", "data", "mesh.rds"))
saveRDS(fall_mesh, here("sdmtmb", "data", "fall_mesh.rds"))
saveRDS(spring_mesh, here("sdmtmb", "data", "spring_mesh.rds"))

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
