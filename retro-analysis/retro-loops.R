### created: 11/11/2022
### last updated: 

#### RETROSPECTIVE ANALYSIS ####

###################
#### OBJECTIVE ####
###################
# run a retrospective analysis loop that calculates stratified means for each species, strata, and year based on the historical time series data. Two loops are run to compare the magnitude of change in stratified means when all historical tows are used versus when a subset of tows are subtracted to serve as a proxy of tows lost in potential wind energy areas. 

# awaiting new full raw dataset from Catherine to run in here("tidy-data", "clean.R"); when received run and check code. Derived data from tidy-data script will be called to run in the loops. 
####################

#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

#### LOAD DATA ####
year <- seq(1960, 2021, 1) # create the year vector to loop over based on the sequence of years in the historical datasets 

# WIITHOUT WIND #
nowind_areas <- readRDS("complete_outsidetows.rds") # check for exact locations of this data derived from 01-tidy-data.R
nowind_species <- unique(nowind_areas$SVSPP) # create a species vector to loop over based on the species in the full historical time series data 
nowind_means <- array() # create an empty array for storing values from the loop calculating means with the full historical times series data

# WITH WIND #
withwind_areas <- readRDS("complete_merged_data.rds") # check for exact locations of this data derived from 01-tidy-data.R
withwind_species <- unique(withwind_areas$SVSPP) # create a species vector to loop over based on the dataset minus tows occurring in potential wind energy areas  
withwind_means <- array() # create an empty array for storing values from the loop calculating means for the dataset minus the tows occurring in potential wind energy areas


#############################
#### 01 - WITH WIND LOOP ####
#############################
# calculations for stratified means are looped over every species, year, and stratum combinations using the dataset where tows/observations were subtracted if they occurred in potential wind energy areas. 

for(i in length(year)){
  for(j in length(unique(withwind_areas$STRATUM))){ # check column name reference
    for(k in length(withwind_species$SVSPP)){ # check column name reference
      
      stratprop <- withwind_areas$AREA / sum(withwind_areas$AREA) # check column name reference
      
      biomean <- sum(withwind_species$EXPCATCHWT) / length(unique(withwind_areas$STATION))  # check column name reference
      
      stratmean <- stratprop * biomean
      
      withwind_means[i, j, k] <- stratmean
      
    }
  }
}
  
# save the new rds object
saveRDS(withwind_means, here("")) # check location



################################
#### 02 - WITHOUT WIND LOOP ####
################################
# calculations for stratified means are looped over every species, year, and stratum combinations using the full historical times series dataset. No tows/observations have been subtracted from this dataset 

for(i in length(year)){
  for(j in length(unique(nowind_areas$STRATUM))){ # check column name reference
    for(k in length(nowind_species$SVSPP)){ # check column name reference
      
      stratprop <- nowind_areas$AREA / sum(nowind_areas$AREA) # check column name reference
      
      biomean <- sum(nowind_species$EXPCATCHWT) / length(unique(nowind_areas$STATION))  # check column name reference
      
      stratmean <- stratprop * biomean
      
      nowind_means[i, j, k] <- stratmean
      
    }
  }
}

# save the new rds object
saveRDS(nowind_means, here("")) # check location