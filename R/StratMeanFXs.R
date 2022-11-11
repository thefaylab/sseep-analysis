### created: 11/09/2022
### last updated: 11/11/2022

#### STRATIFIED MEAN FUNCTIONS ####

###################
#### OBJECTIVE ####
###################
# write stratified mean functions for sourcing in future analyses. 

# awaiting new full raw dataset from Catherine to run in here("tidy-data", "01-clean-raw-data.R"); when received run and check code. Derived data from tidy-data script will be called here to test stratified mean functions. 
####################

#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

#### LOAD DATA ####
subdata <- readRDS("complete_merged_data.rds") # check location of complete data produced from 01-clean-raw-data.R

subdata <- subdata %>%
  filter(YEAR %in% c(2019, 2020, 2021), SVSPP %in% c()) # select small subset of years and species 


##########################################
#### 01 - RELATIVE WEIGHT OF A STRATA ####
##########################################
# calculates a relative proportion or weight of a given strata based on the area that strata represents as a proportion of the whole survey area 

RelStratWt <- function(strata_area, total_survey_area){
 
   rs <- strata_area / total_survey_area
  
   return(rs)
}

###########################
#### 02 - MEAN BIOMASS ####
###########################
# calculates the biomass at a given station divided by the total number of stations in a single stratum.

MeanBiomass <- function(expectcatchwt, station){
  
  mb <- sum(expectcatchwt) / length(station)
  
  return(mb)
}

######################################
#### 03 - STRATIFIED MEAN BIOMASS ####
######################################
# calculates the biomass at a given station divided by the total number of stations in a single stratum.

StratMean <- function(expectcatchwt, station, strata_area, total_survey_area){
  
  rs <- RelStratWt(strata_area, total_survey_area)
  mb <- MeanBiomass(expectcatchwt, station)
  
  sm <- sum(rs * mb)
  
  return(sm)
}
  

