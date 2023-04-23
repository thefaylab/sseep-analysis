### created: 12/10/2022
### last updated: 03/02/2023

# 01a - PREPARE DATA ####

## OBJECTIVE ####
# prepare data for sdmTMB model fits and predicting


## LOAD PACKAGES ####
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

## LOAD DATA ####
# raw bottom trawl data; contains present only observations and will be used to pull depth, bottom temperature, and area swept values for each station below.  
raw_data <- read_csv(here("data", "raw-data", "NEFSC_BTS_ALLCATCHES.csv")) |>
  filter(EST_YEAR %in% c(2009:2021)) |>
  mutate(SVSPP = as.integer(SVSPP), 
         STATION = as.integer(STATION), 
         STRATUM = as.integer(STRATUM))

# dataset created from `03-spatial-filter.R` here("tidy-data"). Contains complete observations for summer flounder that makes up 95% of their cumulative biomass. 
data <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |> filter(SVSPP == 103) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))



### SOME DATA TIDYING ####
# extract depth, bottom temperature, and area swept values from the raw  data by using the unique tows that occur in the summer flounder data 
add_info <- semi_join(raw_data, data, by =c("STRATUM", "CRUISE6", "STATION", "SEASON", "EST_YEAR"))  |>
  select(CRUISE6, STATION, STRATUM, AVGDEPTH, BOTTEMP, AREA_SWEPT_WINGS_MEAN_KM2, SEASON, EST_YEAR) |>
  unique()


## PREPARE SUMMER FLOUNDER DATA ####
data <- data |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR) |> 
  #mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>% # create code for unique tow
  left_join(add_info, by = c("STRATUM", "CRUISE6", "STATION", "SEASON", "EST_YEAR")) |> # add depth, bottom temp, and area swept
  ungroup() 

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


## CONSTRUCT MESH #### 
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
