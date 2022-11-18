### created: 11/09/2022
### last updated: 11/18/2022

## STRATIFIED MEAN FUNCTIONS ####

###################
### OBJECTIVE #####
###################
# write stratified mean functions for sourcing in future analyses. 

# to test the functions, the time series data running from 2009 to 2021 that was generated and completed for ~10 species for presence and absence analysis is read into the script. This data set was derived from the 02-complete-datasets.R script found here("tidy-data", "02-complete-datasets.R"). 

# this script could be revised when the new raw data is received from Catherine Foley and cleaned in preceding scripts. 

####################

### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

### LOAD DATA ####
data <- read.csv(here("data", "clean-data", "merged_data_clean.csv"))
data <- data %>% 
  filter(SVSPP == 141, EST_YEAR %in% c(2018, 2021)) 
strata <- sf::st_read(dsn = here("gis", "temp", "BTS_Strata", "BTS_Strata.shp"))


##########################################
### 01 - RELATIVE WEIGHT OF A STRATA #####
##########################################
# calculates a relative proportion or weight of a given strata based on the area that strata represents as a proportion of the whole survey area 

#### Write Function ####
RelStratWt <- function(strata_area, total_survey_area){
 
   rs <- strata_area / total_survey_area
  
   return(rs)
}

#### Test Function ####
# 01 - subset strata data and call relative weight function
StratAreas <- strata %>% 
  dplyr::select(Strata_Num, Area_SqNm) %>%
  sf::st_set_geometry(NULL) %>% 
  unique()

StratAreas <- StratAreas %>% 
  mutate(StratSum = sum(StratAreas$Area_SqNm), 
         RelWts = RelStratWt(Area_SqNm, StratSum)) %>% 
  rename(STRATUM = Strata_Num)

###########################
### 02 - MEAN BIOMASS #####
###########################
# calculates the biomass at a given station divided by the total number of stations in a single stratum.

#### Write Function ####
MeanBiomass <- function(expectcatchwt, station){
  
  mb <- sum(expectcatchwt) / length(station)
  
  return(mb)
}

#### Test Function ####
# 01 - subset tow data and call MeanBiomass()
Biomass <- data %>% 
  select(EST_YEAR, STRATUM, SVSPP, COMNAME, CRUISE6, SEASON, STATION, EXPCATCHWT) %>% 
  group_by(EST_YEAR, STRATUM, SVSPP) %>%
  mutate(Biomass_mu = MeanBiomass(EXPCATCHWT, STATION))

# 02 - create smaller subset of data to verify function is working
test <- Biomass %>%
  filter(STRATUM == 1010, EST_YEAR == 2018) 
# 02a -  calculate mean biomass by hand to compare to function output
mb <- sum(test$EXPCATCHWT) / length(test$STATION)

######################################
#### 03 - STRATIFIED MEAN BIOMASS ####
######################################
# calculates the biomass at a given station divided by the total number of stations in a single stratum.

#### Wrire Function ####
StratMean <- function(expectcatchwt, station, strata_area, total_survey_area){
  
  rs <- RelStratWt(unique(strata_area), unique(total_survey_area))
  mb <- MeanBiomass(expectcatchwt, station)
  
  sm <- sum(rs * mb)
  
  return(sm)
}
  
#### Test Function ####
# 01 - merge data to contain tow and strata area in one dataframe
merged_data <- inner_join(Biomass, StratAreas, by = "STRATUM") 

# 02 - subset merged data and call Stratified Mean function
StratMeans <- merged_data %>%  
  group_by(SVSPP, STRATUM, EST_YEAR) %>% 
  summarise(strat_mean = StratMean(EXPCATCHWT, STATION, Area_SqNm, StratSum))


# 03 - create smaller subset of data to verify function is working
test2 <- merged_data %>% 
  filter(STRATUM == 1010, EST_YEAR == 2018) %>% 
  arrange(STRATUM)
# 03a -  calculate stratified mean by hand to compare to function output
mb <- sum(test2$EXPCATCHWT) / length(test2$STATION)
rw <- unique(test2$Area_SqNm) / unique(test2$StratSum)
x <- mb * rw


