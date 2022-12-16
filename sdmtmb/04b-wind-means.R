### created: 12/10/2022
### last updated:

#### 04b - STRATIFIED CALCULATIONS: WITH WIND AREAS PRECLUDED ####

###################
#### OBJECTIVE ####
###################
# calculate stratified means for each strata, and year combination based on historical and simulated data for summer flounder. The analysis contained herein calculates the stratified means for the time series minus tows that overlapped in potential wind energy areas. 


####################


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


#### LOAD DATA ####
# summer flounder data created in `01-prepare-data.R` here("sdmtmb").
sumflounder <- readRDS(file = here("sdmtmb", "data", "sumflounder.rds")) %>% filter(AREA == "OUTSIDE")

# dataset created from `02-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow. 
sf_m6sim <- readRDS(here("sdmtmb", "data", "sumflounder-m6sims.rds")) %>% filter(AREA == "OUTSIDE")
sf_m1sim <- readRDS(here("sdmtmb", "data", "sumflounder-m1sims.rds")) %>% filter(AREA == "OUTSIDE")
 

# load and manipulate the bottom trawl survey strata shapefile
strata <- readRDS(here("data", "rds", "strata.rds"))

# calculate total survey area for use in future calculations  
BTSArea <- as.integer(sum(strata$Area_SqNm))


#### CALCULATE INDIVIDUAL MEANS AND VARIANCES ####
# calculate individual means and variances for each combination of year and stratum using historical summer flounder data
sf_wind_means <- sumflounder %>%  
  group_by(STRATUM, year) %>% 
  summarise(towct = length(unique(STATION)), # calculate unique tows
            mu1 = sum(EXPCATCHWT)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
            var1 = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
                         sum((EXPCATCHWT - mu1)^2)/(towct - 1))) %>% # if tow count does not equal 1, then find the variance of biomass
  left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
  mutate(wt_mu1 = Area_SqNm * mu1, # part one of the stratified mean formula
         wt_var1 = ((((RelWt)^2) * var1) / towct) * (1 - (towct / Area_SqNm))) # part one of the stratified variance formula


### save the data 
saveRDS(sf_wind_means, here("data", "rds", "sf-mu_preclusion.rds"))



# calculate individual means and variances for each combinations year, and stratum using m6 simulations
m6_wind_mu <- sf_m6sim %>%  
  group_by(STRATUM, year) %>%
  summarise(towct = length(unique(STATION)), # calculate unique tows
            mu1 = sum(sim1)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
            var1 = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
                          sum((sim1 - mu1)^2)/(towct - 1)), # if tow count does not equal 1, then find the variance of biomass
            mu2 = sum(sim2)/towct,
            var2 = ifelse(towct == 1, 0,
                          sum((sim2 - mu2)^2)/(towct - 1))) %>% 
  left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
  mutate(wt_mu1 = Area_SqNm * mu1, # part one of the stratified mean formula
         wt_var1 = ((((RelWt)^2) * var1) / towct) * (1 - (towct / Area_SqNm)), # part one of the stratified variance formula
         wt_mu2 = Area_SqNm * mu2,
         wt_var2 = ((((RelWt)^2) * var2) / towct) * (1 - (towct / Area_SqNm)))


### save the data 
saveRDS(m6_wind_mu, here("data", "rds", "m6-mu_preclusion.rds"))



# calculate individual means and variances for each combinations year, and stratum using m1 simulations
m1_wind_mu <- sf_m1sim %>%  
  group_by(STRATUM, year) %>%
  summarise(towct = length(unique(STATION)), # calculate unique tows
            mu1 = sum(sim1)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
            var1 = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
                          sum((sim1 - mu1)^2)/(towct - 1)), # if tow count does not equal 1, then find the variance of biomass
            mu2 = sum(sim2)/towct,
            var2 = ifelse(towct == 1, 0,
                          sum((sim2 - mu2)^2)/(towct - 1))) %>% 
  left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
  mutate(wt_mu1 = Area_SqNm * mu1, # part one of the stratified mean formula
         wt_var1 = ((((RelWt)^2) * var1) / towct) * (1 - (towct / Area_SqNm)), # part one of the stratified variance formula
         wt_mu2 = Area_SqNm * mu2,
         wt_var2 = ((((RelWt)^2) * var2) / towct) * (1 - (towct / Area_SqNm)))

### save the data 
saveRDS(m1_wind_mu, here("data", "rds", "m1-mu_preclusion.rds"))


#### COMPLETE STRATIFIED MEAN AND VARIANCE CALCULATIONS ####
# calculate individual means and variances for each combination of year and stratum using historical summer flounder data
sf_wind_stratmu <- sf_wind_means %>% 
  group_by(year) %>%  
  summarise(stratmu1 = (sum(wt_mu1)) / BTSArea, # part two of the stratified mean formula
            stratvar1 = sum(wt_var1)) %>% # part two of the stratified variance formula
  mutate(TYPE = paste("With Wind Precluded"), # paste identifying information of means and variances for joining and plotting in later scripts
         SOURCE = paste("Historical"))

### save the data 
saveRDS(sf_wind_stratmu, here("data", "rds", "sf_stratmu_preclusion.rds"))



# calculate stratified means and variances for each year based on individual stratum means and variances from m6 simulated data
m6_wind_stratmu <- m6_wind_mu %>% 
  group_by(year) %>% 
  summarise(stratmu1 = (sum(wt_mu1)) / BTSArea, # part two of the stratified mean formula
            stratvar1 = sum(wt_var1), # part two of the stratified variance formula
            stratmu2 = (sum(wt_mu2)) / BTSArea,
            stratvar2 = sum(wt_var2)) %>% 
  mutate(TYPE = paste("With Wind Precluded"), # paste identifying information of means and variances for joining and plotting in later scripts
         SOURCE = paste("m6"))

### save the data
saveRDS(m6_wind_stratmu, here("data", "rds", "m6_stratmu_preclusion.rds"))



# calculate stratified means and variances for each year based on individual stratum means and variances from m1 simulated data
m1_wind_stratmu <- m1_wind_mu %>% 
  group_by(year) %>% 
  summarise(stratmu1 = (sum(wt_mu1)) / BTSArea, # part two of the stratified mean formula
            stratvar1 = sum(wt_var1), # part two of the stratified variance formula
            stratmu2 = (sum(wt_mu2)) / BTSArea,
            stratvar2 = sum(wt_var2)) %>% 
  mutate(TYPE = paste("With Wind Precluded"), # paste identifying information of means and variances for joining and plotting in later scripts
         SOURCE = paste("m1"))

### save the data 
saveRDS(m1_wind_stratmu, here("data", "rds", "m1_stratmu_preclusion.rds"))

