### created: 12/10/2022
### last updated: 10/15/2024

# 02 - PREPARE DATA ####

## OBJECTIVE ####
# prepare data for sdmTMB model fits and predicting


### Load packages ####
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

### Environment Set-Up ####
# season
season <- "fall"

# species
species <- "sumflounder"


### File locations ####
dat.files <- here("data", "rds", "sdmtmb", species) #

### Read in data ####
# dataset containing complete observations for species based on their generated spatial footprint. 
data <- readRDS(here("data", species, str_c(species, "rds", sep = ".")))

# read in the grid with the area predictor created here("sdmtmb", "01-make-survey-grid.R")
grid <- readRDS(here("data", "rds", "sdmtmb", "survey_grid_Jun2022.rds")) 


## PREPARE MODEL FITTING DATA ####
data <- data |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) #|> # convert lat long; default units are km 


# remove the NA values in depth
na <- filter(data, is.na(AVGDEPTH)) # check for any na values
data <- data |> 
 filter(!is.na(AVGDEPTH)) # keep everything that is not NA


# create fall and spring datasets 
season_dat <- data %>% filter(SEASON == str_to_upper(season))


## CONSTRUCT MESH #### 
#cutoff defines the minimum allowed distance between points in the units of X and Y (km)
season_mesh <- make_mesh(season_dat, xy_cols = c("X", "Y"), cutoff = 10)

#mesh$mesh$n 
# plot(mesh)
plot(season_mesh)


## SUBSET GRIDS FOR PREDICTIONS ####
# if seasonal footprint differs from overall survey area, complete this section to create a grid in which spatial distribution predictions will be extrapolated across to match the sampling frame

# convert the data frame to a simple feature to crop the grid to areas of historical catch rates
grid_sf <- grid |> 
  sf::st_as_sf(coords = c("X", "Y")) # convert to sf using X and Y UTM coords

# establish coordinate system as UTM 18N zone
st_crs(grid_sf) <-32618  

# filter strata based on seasonal catch data 
season_strat <- right_join(strata, season_dat, by = "STRATUM") 

# transform coordinate system to UTM 18N
strat_utm <- st_transform(season_strat, crs = 32618) 

# merge strata to create one polygon
strat_union <- st_union(strat_utm)

# identify cells that intersect with the seasonal strata polygon 
intersected <- sf::st_intersects(grid_sf, strat_union) 

# filter the grid for only intersecting cells, where intersected has a value greater than 0. 
selected_grid <- grid_sf[lengths(intersected) > 0, ] 

# retrieve the coordinates from the selected grid matrix
coords <- selected_grid |>
  st_coordinates()

# bind the coordinates with the selected grid 
seasonal_grid <- bind_cols(coords, selected_grid) |> 
  select(-geometry) |> # coerce to dataframe
  mutate(X = X/1000, # convert coordinate to km to match coordinates in bts data
         Y = Y/1000, 
         SEASON = season)

# plot it 
ggplot(seasonal_grid, aes(X, Y, fill = AVGDEPTH)) + 
  geom_tile(width = 10, height = 10) +
  scale_fill_viridis_c() +
  coord_equal()

## SAVE ALL DATA ####
save.dat <- list(data,
                 season_dat, 
                 season_mesh,
                 season_grid)

names(save.dat) <- c(species, 
                     str_c(species, "_", season, ".rds", sep = ""), 
                     str_c(season, "mesh.rds", sep = "_"), 
                     str_c(season, "grid.rds", sep = "_"))

pmap(list(save.dat, names(save.dat)), ~saveRDS(.x, here(dat.files, .y)))
