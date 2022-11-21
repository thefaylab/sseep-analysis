### created: 11/11/2022
### last updated: 11/21/2022

#### RETROSPECTIVE ANALYSIS: WITH WIND AREAS INCLUDED ####

###################
#### OBJECTIVE ####
###################
# run a retrospective analysis loop that calculates stratified means for each species, strata, and year based on the historical time series data. The loop contained herein calculates the stratified means for the full time series without any change to represent the base/control scenario. 

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

# WIITHOUT WIND #
#nowind_areas <- readRDS("complete_outsidetows.rds") # check for exact locations of this data derived from 01-tidy-data.R

# filter data for smaller subset in order to test merge and loop
nowind_areas <- data %>% 
  filter(EST_YEAR %in% c(2009, 2021), SVSPP %in% c(141, 73)) %>% 
  left_join(strata, by = "STRATUM")


# nowind_areas <- data %>% 
#   select(CRUISE6, STATION, STRATUM, EST_YEAR, SVSPP, COMNAME, EXPCATCHNUM, EXPCATCHWT) %>%
#   left_join(nowind_areas, strata, by = "STRATUM")
# Error: cannot allocate vector of size 34.4 Gb

#nowind_species <- unique(nowind_areas$SVSPP) # create a species vector to loop over based on the species in the full historical time series data 
nowind_species <- unique(nowind_areas$SVSPP)

#this works to create empty list
#nowind_means <-  vector(mode = "list", length = length(nowind_species))
# r.names <- 
# c.names <- c("YEAR", "STRATUM", "STRATMEAN")
# m.names <- as.character(nowind_species)

#creates empty array with 2 arrays (ideally one for each species)
nowind_means <- array(dim = 4:2) #, dimnames = list(c.names, m.names))




#################################
#### 01a - WITHOUT WIND LOOP ####
#################################
# calculations for stratified means are looped over every species, year, and stratum combinations using the full historical times series dataset. No tows/observations have been subtracted from this dataset 

##### Summarising Attempts ####
nowind_means1 <- nowind_areas %>% 
  group_by(EST_YEAR, STRATUM, SVSPP) %>% 
  summarise(stratmu = StratMean(EXPCATCHWT, STATION, Area_SqNm, StratSum))

#much smaller dataset to double check calculations
nowind_means2 <- nowind_areas %>% 
  filter(EST_YEAR == 2009, SVSPP == 141) %>% 
  group_by(EST_YEAR, STRATUM) %>%
  summarise(stratmu = StratMean(EXPCATCHWT, STATION, Area_SqNm, StratSum))


##### Loop Attempts ####

# original drafted loop script
for(i in length(year)){
  for(j in length(unique(nowind_areas$STRATUM))){ # check column name reference
    for(k in length(unique(nowind_areas$SVSPP))){ # check column name reference
      
      stratprop <- strata$Area_SqNm/ strata$StratSum # check column name reference
      
      biomean <- sum(nowind_areas$EXPCATCHWT) / length(unique(nowind_areas$STATION))  # check column name reference
      
      stratmean <- stratprop * biomean
      
      #nowind_means <- as.array(stratprop * biomean) #results in num [1:176(1d)]
      
      nowind_means[i, j, k] <- stratmean #error: incorrect number of subscripts
      
    }
  }
}

# this loop creates an array with 176 obs
for(i in 1:length(nowind_species)){
  for(j in 1:length(unique(year))){ # check column name reference
    for(k in 1:length(unique(nowind_areas$STRATUM))){ # check column name reference
      
      stratprop <- strata$Area_SqNm/ strata$StratSum # check column name reference
      
      biomean <- sum(nowind_areas$EXPCATCHWT) / length(unique(nowind_areas$STATION))  # check column name reference
      
      stratmean <- stratprop * biomean
      
      nowind_means <- array(data = stratmean)
      #nowind_means[k,j,i] <- stratmean
        #list(nowind_species[i], unique((nowind_areas$STRATUM)), stratmean)
      
    }
  }
}



#this works to create empty list
nowind_means <-  vector(mode = "list", length = length(nowind_species))

for(i in 1:length(nowind_species)){
  for(j in 1:length(unique(year))){ # check column name reference
    for(k in 1:length(unique(nowind_areas$STRATUM))){ # check column name reference

      stratmean <- StratMean(nowind_areas$EXPCATCHWT, nowind_areas$STATION, nowind_areas$Area_SqNm, nowind_areas$StratSum)
      
      
      
    }
  }
}

# save the new rds object
saveRDS(nowind_means, here("")) # check location