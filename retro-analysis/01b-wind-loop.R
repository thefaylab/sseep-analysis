### created: 11/11/2022
### last updated: 12/08/2022

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
data <- readRDS(here("data", "rds", "merged_data_complete.rds")) %>% filter(AREA == "OUTSIDE") %>% mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))
 
# species dataframe for adding to final dataset 
species <- readRDS(here("data", "rds", "species.rds"))

# load and manipulate the bottom trawl survey strata shapefile
strata <- readRDS(here("data", "rds", "strata.rds"))

# calculate total survey area for use in future calculations  
BTSArea <- as.integer(sum(strata$Area_SqNm))

# statistical management areas 
#geounits <- readRDS(here("data", "rds", "geounits.rds"))


#### CALCULATE INDIVIDUAL MEANS AND VARIANCES ####
# calculate individual means and variances for each combinations of species, year, and stratum
wind_means <- data %>%   
  mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>% 
  left_join(geounits, by = "code") %>% 
  rename(STRATUM = STRATUM.x, 
         STATION = STATION.x) %>% 
  group_by(STRATUM, EST_YEAR, SVSPP, SEASON) %>% #, GEO_AREA) %>% 
  summarise(towct = length(unique(STATION)), # calculate unique tows
            mu = sum(EXPCATCHWT)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
            var = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
                         sum((EXPCATCHWT - mu)^2)/(towct - 1))) %>% # if tow count does not equal 1, then find the variance of biomass
  left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
  mutate(wt_mu = Area_SqNm * mu, # part one of the stratified mean formula
         wt_var = (((((RelWt)^2) * var) / towct) * (1 - (towct / Area_SqNm)))) # part one of the stratified variance formula


### save the data 
saveRDS(wind_means, here("data", "rds", "indiv-mu_preclusion.rds"))


#### COMPLETE STRATIFIED MEAN AND VARIANCE CALCULATIONS ####
# calculate stratified means and variances for each combinations of species and year based on individual stratum means and variances
wind_stratmu <- wind_means %>% 
  group_by(SVSPP, EST_YEAR, SEASON) %>% #, GEO_AREA) %>% 
  summarise(stratmu = (sum(wt_mu)) / BTSArea, # part two of the stratified mean formula
            stratvar = sum(wt_var)) %>% # part two of the stratified variance formula
  mutate(TYPE = paste("With Wind Precluded")) # paste identifying information of means and variances for joining and plotting in later scripts


### save the data
saveRDS(wind_stratmu, here("data", "rds", "strat-mu_preclusion.rds"))

