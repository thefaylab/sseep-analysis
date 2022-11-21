### created: 11/09/2022
### last updated: 11/21/2022

## STRATIFIED MEAN FUNCTIONS ####

###################
### OBJECTIVE #####
###################
# write stratified mean functions for sourcing in future analyses. 

####################


##########################################
### 01 - RELATIVE WEIGHT OF A STRATA #####
##########################################
# calculates a relative proportion or weight of a given strata based on the area that strata represents as a proportion of the whole survey area 

#### Write Function ####
RelStratWt <- function(strata_area, total_survey_area){
 
   rs <- strata_area / total_survey_area
  
   return(rs)
}



###########################
### 02 - MEAN BIOMASS #####
###########################
# calculates the biomass at a given station divided by the total number of stations in a single stratum.

#### Write Function ####
MeanBiomass <- function(expectcatchwt, station){
  
  mb <- sum(expectcatchwt) / length(station)
  
  return(mb)
}


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
  

