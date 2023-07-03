### created:      07/03/2023
### last update:  
###

# CURRENTLY SAMPLED STRATA ####

## OBJECTIVE #### 
# Filter the BTS strata for all the currently sampled strata for subsequent analysis. 

# Script should:
# read in the bottom trawl strata shapefile and tow data 
# filter for unique strata currently sampled using strata recorded in the tow data  
# write a csv, rds, and shapefile containing the filtered strata object

## LOAD PACKAGES ####
library(sf)
library(here)
suppressPackageStartupMessages(library(tidyverse))


## LOAD DATA ####
# all bottom trawl survey strata 
strata <- sf::st_read(dsn = here("gis", "NEFSC_BTS_AllStrata_Jun2022.shp"))

# completed tow data created from here("tidy-data", "02-complete-datasets.R")
merged_data <- readRDS(here("data", "rds", "merged_data_complete.rds"))

## DATA WRANGLE ####
# identify unique strata sampled from 2009-2021 tow data
tow_strata <- unique(merged_data$STRATUM)

# filter strata shapefile by unique strata values 
current <- strata |> filter(Strata_Num %in% tow_strata)
current_tbl <- as_tibble(current)

# verify unique strata sampled includes all current strata from the strata shapefile 
anti <- as_tibble(strata) |> anti_join(current, by = "Strata_Num") # 177 - 82

# save current strata
write.csv(current_tbl, here("data", "clean-data", "current_strata.csv"))
write_sf(current, dsn = here("gis", "current_strata.shp"))
saveRDS(current, here("data", "rds", "current_strata.rds"))


