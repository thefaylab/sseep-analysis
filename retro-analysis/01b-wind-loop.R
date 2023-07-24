### created: 11/11/2022
### last updated: 07/24/2023

# 01b - STRATIFIED CALCULATIONS: WITH WIND AREAS PRECLUDED ####


## OBJECTIVE ####
# calculate stratified means for each species, strata, and year combination based on the historical time series data. The analysis contained herein calculates the stratified means for the time series minus tows that overlapped in potential wind energy areas. 


## LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


## LOAD DATA ####
# dataset created from `05b-spatial-filter-data.R` here("tidy-data"). Contains complete observations for each species and unique tow filtered based on 95% cumulative distribution of biomass. 
data <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |> filter(AREA == "OUTSIDE")
 
# species dataframe for adding to final dataset 
species <- readRDS(here("data", "rds", "95filtered-species.rds"))

# # load relative area weights of survey strata created here("tidy-data", "03a-find-wind-intersection.R")
strata <- readRDS(here("data", "rds", "preclusion_wts.rds"))

# calculate total survey area for use in future calculations  
BTSArea <- sum(strata$Area_SqM_remain) #in meters

# statistical management areas 
#geounits <- readRDS(here("data", "rds", "geounits.rds"))


#### CALCULATE INDIVIDUAL MEANS AND VARIANCES ####
# calculate individual means and variances for each combinations of species, year, and stratum
wind_means <- data %>%   
  #mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>% 
  #left_join(geounits, by = "code") %>% 
  #rename(STRATUM = STRATUM.x, 
  #       STATION = STATION.x) %>% 
  group_by(STRATUM, EST_YEAR, SVSPP, SEASON) %>% #, GEO_AREA) %>% 
  summarise(towct = length(unique(STATION)), # calculate unique tows
            mu = sum(EXPCATCHWT)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
            var = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
                         sum((EXPCATCHWT - mu)^2)/(towct - 1))) %>% # if tow count does not equal 1, then find the variance of biomass
  left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
  mutate(wt_mu = Area_SqM_remain * mu, # part one of the stratified mean formula
         wt_var = (((((RelWt)^2) * var) / towct) * (1 - (towct / Area_SqM_remain)))) # part one of the stratified variance formula


### save the data 
saveRDS(wind_means, here("data", "rds", "retro-analysis", "indiv-mu_preclusion.rds"))


#### COMPLETE STRATIFIED MEAN AND VARIANCE CALCULATIONS ####
# calculate stratified means and variances for each combinations of species and year based on individual stratum means and variances
wind_stratmu <- wind_means %>% 
  group_by(SVSPP, EST_YEAR, SEASON) %>% #, GEO_AREA) %>% 
  summarise(stratmu = (sum(wt_mu)) / BTSArea, # part two of the stratified mean formula
            stratvar = sum(wt_var)) %>% # part two of the stratified variance formula
  mutate(TYPE = paste("With Wind Precluded")) # paste identifying information of means and variances for joining and plotting in later scripts


### save the data
saveRDS(wind_stratmu, here("data", "rds", "retro-analysis", "strat-mu_preclusion.rds"))

