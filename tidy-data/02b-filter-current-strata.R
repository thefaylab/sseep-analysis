### created:      07/03/2023
### last update:  07/19/2023
###

# 02b - FILTER FOR CURRENTLY SAMPLED STRATA ####

## OBJECTIVE #### 
# Filter the BTS strata for all the currently sampled strata for subsequent analysis. 

# Script should:
# read in the bottom trawl strata shapefile and tow data 
# filter for unique strata currently sampled using strata recorded in the tow data  
# write a csv, rds, and shapefile containing the filtered strata object
# calculate the relative weight of strata area for stratified mean calculations

### LOAD PACKAGES ####
library(sf)
library(here)
suppressPackageStartupMessages(library(tidyverse))

## OPTION A ####
# read active bottom trawl survey strata from geodatabase 
strata <- sf::st_read(here("gis", "NEFSC-BTS_strata_Jun2022.gdb"), layer = "NEFSC_BTS_ActiveStrata_Jun2022") |> 
  rename(STRATUM = Strata_Num)

saveRDS(strata, here("data", "rds", "active_strata.rds"))

## OPTION B ####
### LOAD DATA ####
# read all bottom trawl survey strata from geodatabase 
all_strata <- sf::st_read(dsn = here("gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) |>
  dplyr::rename(STRATUM = "Strata_Num")

# completed tow data created from here("tidy-data", "02-complete-datasets.R")
merged_data <- readRDS(here("data", "rds", "merged_data_complete.rds"))

### DATA WRANGLE ####
# identify unique strata sampled from 2009-2021 tow data
tow_strata <- unique(merged_data$STRATUM)

# filter strata shapefile by unique strata values 
current <- strata |> 
  filter(STRATUM %in% tow_strata) 
current_tbl <- as_tibble(current)

# verify unique strata sampled includes all current strata from the strata shapefile 
anti <- as_tibble(strata) |> anti_join(current, by = "STRATUM") # 177 - 82

# save current strata
write.csv(current_tbl, here("data", "clean-data", "active_strata.csv"))
write_sf(current, dsn = here("gis", "active_strata.shp"))
saveRDS(current, here("data", "rds", "active_strata.rds"))

## CALCULATE RELATIVE AREA ####
strata <- strata |> #read in data
  dplyr::select(STRATUM, Area_SqNm) |> # select variables to be used in calculations below
  sf::st_set_geometry(NULL) |> # remove the coordinates; changes the sf to a df
  unique() |> # identify unique stratum only 
  mutate(RelWt = Area_SqNm / sum(Area_SqNm)) # calculate the relative weight of each stratum based on its proportion of the total survey footprint area; to be used in later calculations.

# save the data
saveRDS(strata, here("data", "rds", "active_strata_wts.rds"))
