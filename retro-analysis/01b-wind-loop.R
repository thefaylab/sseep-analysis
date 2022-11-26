### created: 11/11/2022
### last updated: 11/26/2022

#### 01b - STRATIFIED CALCULATIONS: WITH WIND AREAS PRECLUDED ####

###################
#### OBJECTIVE ####
###################
# calculate stratified means for each species, strata, and year combination based on the historical time series data. The analysis contained herein calculates the stratified means for the time series minus tows that overlapped in potential wind energy areas. 

# awaiting new full raw dataset from Catherine to run in here(...); when received run and check code. Derived data from tidy-data script will be called to run in the loops. 
####################


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


#### LOAD DATA ####
# dataset created from `02-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow. 
data <- readRDS(here("data", "rds", "merged_data_complete.rds")) 
 
# species dataframe for adding to final dataset 
species <- data %>% 
  select(SVSPP, COMNAME, SCINAME) %>%
  unique()

# load and manipulate the bottom trawl survey strata shapefile
strata <- sf::st_read(dsn = here("gis", "temp", "BTS_Strata", "BTS_Strata.shp")) %>% #read in data
  dplyr::select(Strata_Num, Area_SqNm) %>% # select variables to be used in calculations below
  sf::st_set_geometry(NULL) %>% # remove the coordinates; changes the sf to a df
  unique() %>% # identify unique stratum only
  mutate(StratSum = sum(Area_SqNm)) %>% # rename Strat_Num to STRATUM for further analyses below
  rename(STRATUM = Strata_Num)# calculate the relative weight of each stratum based on its proportion of the total survey footprint area; to be used in later calculations.

# calculate total survey area for use in future calculations  
BTSArea <- as.integer(sum(strata$Area_SqNm))


#### CALCULATE INDIVIDUAL MEANS AND VARIANCES ####
# calculate individual means and variances for each combinations of species, year, and stratum
wind_means <- data %>% 
  group_by(STRATUM, EST_YEAR, SVSPP) %>% 
  summarise(towct = length(unique(STATION)), # calculate unique tows
            mu = sum(EXPCATCHWT)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
            var = sum((EXPCATCHWT - mu)^2)/(length(EXPCATCHWT) - 1)) %>% # find the variance of biomass 
  left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
  mutate(mu = ifelse(is.na(mu), 0, mu), #replace NA values with 0 to avoid na abundance indices; 
         var = ifelse(is.na(var), 0, var), #replace NA values with 0 to avoid na abundance indices; 
         wt_mu = Area_SqNm * mu, # part one of the stratified mean formula
         wt_var = (((((RelWt)^2) * (var^2)) / towct) * (1 - (towct / Area_SqNm)))) # part one of the stratified variance formula


### save the data 
saveRDS(wind_means, here("data", "rds", "indiv-mu_preclusion.rds"))


#### COMPLETE STRATIFIED MEAN AND VARIANCE CALCULATIONS ####
# calculate stratified means and variances for each combinations of species and year based on individual stratum means and variances
wind_stratmu <- wind_means %>% 
  group_by(SVSPP, EST_YEAR) %>% 
  summarise(stratmu = (sum(wt_mu)) / BTSArea, # part two of the stratified mean formula
            stratvar = sum(wt_var)) %>% # part two of the stratified variance formula
  mutate(TYPE = paste("With Wind Precluded")) # paste identifying information of means and variances for joining and plotting in later scripts


### save the data
saveRDS(wind_stratmu, here("data", "rds", "strat-mu_preclusion.rds"))

