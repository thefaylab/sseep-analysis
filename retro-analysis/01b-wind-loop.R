### created: 11/11/2022
### last updated: 11/21/2022

#### RETROSPECTIVE ANALYSIS: WITH WIND AREAS PRECLUDED ####

###################
#### OBJECTIVE ####
###################
# run a retrospective analysis loop that calculates stratified means for each species, strata, and year based on the historical time series data. The loop contained herein calculates the stratified means for the time series minus tows that overlapped in potential wind energy areas. 

# awaiting new full raw dataset from Catherine to run in here(...); when received run and check code. Derived data from tidy-data script will be called to run in the loops. 
####################

#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

#### LOAD DATA ####
#year <- seq(1960, 2021, 1) # create the year vector to loop over based on the sequence of years in the historical datasets 
data <- read.csv(here("data", "clean-data", "merged_data_clean.csv")) 
year <- c(2009, 2021) 
strata <- sf::st_read(dsn = here("gis", "temp", "BTS_Strata", "BTS_Strata.shp")) %>% 
  dplyr::select(Strata_Num, Area_SqNm) %>%
  sf::st_set_geometry(NULL) %>% 
  unique() %>% 
  mutate(StratSum = sum(Area_SqNm)) %>%
  rename(STRATUM = Strata_Num)
source(here("R", "StratMeanFXs.R"))


# WITH WIND #
#withwind_areas <- readRDS("complete_merged_data.rds") # check for exact locations of this data derived from 01-tidy-data.R
withwind_areas <- data %>% filter(AREA == "OUTSIDE") %>% mutate(EXPCATCHWT = as.integer(EXPCATCHWT)) 

withwind_species <- unique(withwind_areas$SVSPP) # create a species vector to loop over based on the dataset minus tows occurring in potential wind energy areas  
withwind_means <- array() # create an empty array for storing values from the loop calculating means for the dataset minus the tows occurring in potential wind energy areas



##############################
#### 01b - WITH WIND LOOP ####
##############################
# calculations for stratified means are looped over every species, year, and stratum combinations using the dataset where tows/observations were subtracted if they occurred in potential wind energy areas. 

for(i in length(year)){
  for(j in length(unique(strata$STRATUM))){ # check column name reference
    for(k in length(unique(withwind_areas$SVSPP))){ # check column name reference
      
      stratprop <- strata$Area_SqNm/ strata$StratSum # check column name reference
      
      biomean <- sum(withwind_areas$EXPCATCHWT) / length(unique(withwind_areas$STATION))  # check column name reference
      
      withwind_means <- as.array(stratprop * biomean)
      
      withwind_means[i, j, k] <- stratmean #incorrect number of subscripts error
      
    }
  }
}
  
# save the new rds object
saveRDS(withwind_means, here("")) # check location


