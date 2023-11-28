### created: 11/02/2023
### last updated: 11/27/2023

# 01a - PREPARE ATLANTIC MACKEREL DATA ####

## OBJECTIVE ####
# prepare Atlantic mackerel data for sdmTMB model fits and predicting


## LOAD PACKAGES ####
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)


here()

## LOAD DATA ####
# raw bottom trawl data; contains present only observations and will be used to pull depth, bottom temperature, and area swept values for each station below.  
raw_data <- read_csv(here("data", "raw-data", "NEFSC_BTS_ALLCATCHES.csv")) |>
  filter(EST_YEAR %in% c(2009:2021)) |>
  mutate(SVSPP = as.integer(SVSPP), 
         STATION = as.integer(STATION), 
         STRATUM = as.integer(STRATUM))

# dataset created from `05b-spatial-filter-data.R` here("tidy-data"). Contains complete observations for Atlantic mackerel that makes up 95% of their cumulative biomass. 
data <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |> filter(SVSPP == 121) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# read in strata for plot check
#strata <- readRDS(here("data", "rds", "active_strata.rds"))
strata_utm <- readRDS(here("data", "rds", "active_strata_utm.rds"))

### SOME DATA TIDYING ####
# extract depth, bottom temperature, and area swept values from the raw  data by using the unique tows that occur in the Atlantic mackerel data 
add_info <- semi_join(raw_data, data, by =c("STRATUM", "CRUISE6", "STATION", "SEASON", "EST_YEAR"))  |>
  select(CRUISE6, STATION, STRATUM, BOTTEMP, AREA_SWEPT_WINGS_MEAN_KM2, SEASON, EST_YEAR) |>
  unique()


## PREPARE ATLANTIC MACKEREL DATA ####
data <- data |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT"), utm_crs = 32618) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR) |> 
  #mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>% # create code for unique tow
  left_join(add_info, by = c("STRATUM", "CRUISE6", "STATION", "SEASON", "EST_YEAR")) |> # add depth, bottom temp, and area swept
  ungroup() 

# plot it
ggplot() + 
  geom_sf(data = strata_utm) + 
  geom_point(data = data, aes(X*1000, Y*1000))

# remove the NA values in depth
na <- filter(data, is.na(AVGDEPTH)) # check for any na values
data <- data |> 
 filter(!is.na(AVGDEPTH)) # keep everything that is not NA

# create fall and spring datasets 
#sf_fall <- data %>% filter(SEASON == "FALL")
am_spring <- data |> filter(SEASON == "SPRING")

# identify years with incomplete surveys
am_spring |> group_by(YEAR, SEASON) |> summarise(ntow = length(unique(TOWID)))
am_spring <- am_spring |> 
  filter(EST_YEAR != 2020) # remove 2020 incomplete survey


# save data 
saveRDS(data, here("sdmtmb", "atlmackerel", "data", "AtlMackerel.rds"))
#saveRDS(sf_fall, here("sdmtmb", "sumflounder", "data", "sumflounder_fall.rds"))
saveRDS(am_spring, here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring.rds"))


## CONSTRUCT MESH #### 
mesh <- make_mesh(data, xy_cols = c("X", "Y"), cutoff = 10) 

spring_mesh <- make_mesh(am_spring, xy_cols = c("X", "Y"), cutoff = 10)
#cutoff defines the minimum allowed distance between points in the units of X and Y (km)

#mesh$mesh$n 
#plot(mesh)

plot(spring_mesh)

# save mesh
saveRDS(mesh, here("sdmtmb", "atlmackerel", "data", "mesh.rds"))

saveRDS(spring_mesh, here("sdmtmb", "atlmackerel", "data", "spring_mesh.rds"))
