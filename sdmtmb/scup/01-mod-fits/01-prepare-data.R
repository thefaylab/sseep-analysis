### created: 11/22/2023
### last updated: 11/22/2023

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

svsta_fall = read.csv(here("data","raw-data","BTS_data_Oct2023","22560_UNION_FSCS_SVSTA.csv"))
svsta_spring = read.csv(here("data","raw-data","BTS_data_Oct2023","22561_UNION_FSCS_SVSTA.csv")) 
svcat_fall = read.csv(here("data","raw-data","BTS_data_Oct2023","22560_UNION_FSCS_SVCAT.csv"))
svcat_spring = read.csv(here("data","raw-data","BTS_data_Oct2023","22561_UNION_FSCS_SVCAT.csv"))

svsta_spring <- svsta_spring |> mutate(ID = as.character(ID))

svcat_spring <- svcat_spring |> mutate(ID = as.character(ID)) |>
  filter_at(vars(ID), all_vars(!is.na(.)))
  
raw_data_fall <- svsta_fall |>
  dplyr::select(ID, STRATUM, CRUISE6, STATION, EST_YEAR , EST_MONTH, TOW, TOWDUR, SETDEPTH, ENDDEPTH, MINDEPTH ,MAXDEPTH, AVGDEPTH,      SURFTEMP,SURFSALIN,BOTTEMP,BOTSALIN,DECDEG_BEGLAT, DECDEG_BEGLON,DECDEG_ENDLAT,DECDEG_ENDLON) |>
  left_join(svcat_fall, by="ID") |>
  select(-STRATUM.y,-CRUISE6.y,-STATION.y,-TOW.y) |>
  rename(CRUISE6 = CRUISE6.x,
         STRATUM = STRATUM.x,
         STATION = STATION.x,
         TOW = TOW.x) |>
  mutate(STRATUM = str_sub(STRATUM,start=2, end=5)) |>
  filter(EST_YEAR %in% c(2009:2022)) |>
  mutate(SVSPP = as.integer(SVSPP), 
         STATION = as.integer(STATION))

  
raw_data_spring <- svsta_spring |>
  dplyr::select(ID, STRATUM, CRUISE6, STATION, EST_YEAR , EST_MONTH, TOW, TOWDUR, SETDEPTH, ENDDEPTH, MINDEPTH ,MAXDEPTH, AVGDEPTH, SURFTEMP,SURFSALIN,BOTTEMP,BOTSALIN,DECDEG_BEGLAT, DECDEG_BEGLON,DECDEG_ENDLAT,DECDEG_ENDLON) |>
  left_join(svcat_spring, by="ID") |>
  select(-STRATUM.y,-CRUISE6.y,-STATION.y,-TOW.y) |>
  rename(CRUISE6 = CRUISE6.x,
         STRATUM = STRATUM.x,
         STATION = STATION.x,
         TOW = TOW.x) |>
  filter(EST_YEAR %in% c(2009:2022)) |>
  mutate(SVSPP = as.integer(SVSPP), 
         STATION = as.integer(STATION))


# dataset created from `03-spatial-filter.R` here("tidy-data"). Contains complete observations for summer flounder that makes up 95% of their cumulative biomass. 
data_fall <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |> 
  filter(SVSPP == 143, SEASON=="FALL") |> 
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT),
         STRATUM = as.character(STRATUM))


data_spring <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |> 
  filter(SVSPP == 143, SEASON=="SPRING") |> 
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))


### SOME DATA TIDYING ####
# extract depth and bottom temperature  from the raw  data by using the unique tows that occur in the scup data 
add_info_fall <- semi_join(raw_data_fall, data_fall, by =c("STRATUM", "CRUISE6", "STATION", "EST_YEAR"))  |>
  select(CRUISE6, STATION, STRATUM, AVGDEPTH, BOTTEMP, EST_YEAR) |>
    unique()

add_info_spring <- semi_join(raw_data_spring, data_spring, by =c("STRATUM", "CRUISE6", "STATION", "EST_YEAR"))  |>
  select(CRUISE6, STATION, STRATUM, AVGDEPTH, BOTTEMP, EST_YEAR) |>
  unique()



## PREPARE SCUP DATA ####
data_fall <- data_fall |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, EST_YEAR) |> 
  #mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>% # create code for unique tow
  left_join(add_info_fall, by = c("STRATUM", "CRUISE6", "STATION", "EST_YEAR")) |> 
  mutate(STRATUM = as.integer(STRATUM)) |>
  ungroup() 

data_spring <- data_spring |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, EST_YEAR) |> 
  #mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>% # create code for unique tow
  left_join(add_info_spring, by = c("STRATUM", "CRUISE6", "STATION", "EST_YEAR")) |> # add depth, bottom temp, and area swept
  ungroup() 

# remove the NA values in depth
# only one na value for fall
na.f <- filter(data_fall, is.na(AVGDEPTH)) # check for any na values
data_fall <- data_fall |> filter(!is.na(AVGDEPTH)) # keep everything that is not NA


# save data 
saveRDS(data_fall, here("sdmtmb", "scup", "data", "scup_fall.rds"))
saveRDS(data_spring, here("sdmtmb", "scup", "data", "scup_spring.rds"))


## CONSTRUCT MESH #### 
fall_mesh <- make_mesh(data_fall, xy_cols = c("X", "Y"), cutoff = 10)
spring_mesh <- make_mesh(data_spring, xy_cols = c("X", "Y"), cutoff = 10)
#cutoff defines the minimum allowed distance between points in the units of X and Y (km)

#mesh$mesh$n 
plot(fall_mesh)
plot(spring_mesh)

# save mesh
saveRDS(fall_mesh, here("sdmtmb", "scup", "data", "fall_mesh.rds"))
saveRDS(spring_mesh, here("sdmtmb", "scup", "data", "spring_mesh.rds"))
