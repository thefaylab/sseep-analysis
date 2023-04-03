### created: 12/10/2022
### last updated: 03/05/2023

#### 06a - STRATIFIED CALCULATIONS: WITH WIND AREAS INCLUDED ####

###################
#### OBJECTIVE ####
###################
# calculate stratified means for each strata and year combination based on historical and simulated data for summer flounder. The analysis contained herein calculates the stratified means for the full time series without any change to represent the base/control scenario. No tows/observations have been subtracted from this dataset 

 
####################


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


#### LOAD DATA ####
# summer flounder data created in `01-prepare-data.R` here("sdmtmb").
#sumflounder <- readRDS(file = here("sdmtmb", "data", "sumflounder.rds"))

# dataset created from `03-model-simulations.R` here("sdmtmb").  
m6fall_sim <- readRDS(here("sdmtmb", "data", "sf-fall-with-m6sims.rds"))
m6spr_sim <- readRDS(here("sdmtmb", "data", "sf-spr-with-m6sims.rds"))

#sf_m1sim <- readRDS(here("sdmtmb", "data", "sumflounder-m1sims.rds"))

# load and manipulate the bottom trawl survey strata shapefile
strata <- readRDS(here("data", "rds", "strata_wts.rds"))

# calculate total survey area for use in future calculations  
BTSArea <- as.integer(sum(strata$Area_SqNm))

source(here("R", "StratMeanFXs_v2.R")) #removed SEASON group for the purposes of the resampling


#### CALCULATE INDIVIDUAL MEANS AND VARIANCES ####
FallSimMu_ww <- m6fall_sim %>% 
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") %>% 
  group_by(nsim) %>% 
  nest() %>%
  mutate(stratmu = map(data, ~strata.mean(.)))
saveRDS(FallSimMu_ww, here("sdmtmb", "data", "sf-fall_sim-ww-means.rds"))

SprSimMu_ww <- m6spr_sim %>% 
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") %>% 
  group_by(nsim) %>% 
  nest() %>%
  mutate(stratmu = map(data, ~strata.mean(.)))
saveRDS(SprSimMu_ww, here("sdmtmb", "data", "sf-spr_sim-ww-means.rds"))

#####
# calculate individual means and variances for each combination of year and stratum using historical summer flounder data
# sf_nowind_means <- sumflounder %>%  
#   group_by(STRATUM, year) %>% 
#   summarise(towct = length(unique(STATION)), # calculate unique tows
#             mu1 = sum(EXPCATCHWT)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
#             var1 = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
#                          sum((EXPCATCHWT - mu1)^2)/(towct - 1))) %>% # if tow count does not equal 1, then find the variance of biomass
#   left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
#   mutate(wt_mu1 = Area_SqNm * mu1, # part one of the stratified mean formula
#          wt_var1 = ((((RelWt)^2) * var1) / towct) * (1 - (towct / Area_SqNm))) # part one of the stratified variance formula
# 
# 
# ### save the data 
# saveRDS(sf_nowind_means, here("data", "rds", "sf_indiv-mu_included.rds"))


# calculate individual means and variances for each combinations year, and stratum using m6 simulations
# m6_nowind_mu <- sf_m6sim %>%  
#   group_by(STRATUM, year) %>%
#   summarise(towct = length(unique(STATION)), # calculate unique tows
#             mu1 = sum(sim1)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
#             var1 = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
#                          sum((sim1 - mu1)^2)/(towct - 1)), # if tow count does not equal 1, then find the variance of biomass
#             mu2 = sum(sim2)/towct,
#             var2 = ifelse(towct == 1, 0,
#                           sum((sim2 - mu2)^2)/(towct - 1))) %>% 
#   left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
#   mutate(wt_mu1 = Area_SqNm * mu1, # part one of the stratified mean formula
#          wt_var1 = ((((RelWt)^2) * var1) / towct) * (1 - (towct / Area_SqNm)), # part one of the stratified variance formula
#          wt_mu2 = Area_SqNm * mu2,
#          wt_var2 = ((((RelWt)^2) * var2) / towct) * (1 - (towct / Area_SqNm)))

# ### save the data 
# saveRDS(m6_nowind_mu, here("data", "rds", "m6-mu_included.rds"))


# calculate individual means and variances for each combinations year, and stratum using m1 simulations
# m1_nowind_mu <- sf_m1sim %>%  
#   group_by(STRATUM, year) %>%
#   summarise(towct = length(unique(STATION)), # calculate unique tows
#             mu1 = sum(sim1)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
#             var1 = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
#                           sum((sim1 - mu1)^2)/(towct - 1)), # if tow count does not equal 1, then find the variance of biomass
#             mu2 = sum(sim2)/towct,
#             var2 = ifelse(towct == 1, 0,
#                           sum((sim2 - mu2)^2)/(towct - 1))) %>% 
#   left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
#   mutate(wt_mu1 = Area_SqNm * mu1, # part one of the stratified mean formula
#          wt_var1 = ((((RelWt)^2) * var1) / towct) * (1 - (towct / Area_SqNm)), # part one of the stratified variance formula
#          wt_mu2 = Area_SqNm * mu2,
#          wt_var2 = ((((RelWt)^2) * var2) / towct) * (1 - (towct / Area_SqNm)))
# 
# ### save the data 
# saveRDS(m1_nowind_mu, here("data", "rds", "m1-mu_included.rds"))
# 
# 
# 
#### COMPLETE STRATIFIED MEAN AND VARIANCE CALCULATIONS ####
# # calculate individual means and variances for each combination of year and stratum using historical summer flounder data
# sf_nowind_stratmu <- sf_nowind_means %>% 
#   group_by(year) %>%  
#   summarise(stratmu1 = (sum(wt_mu1)) / BTSArea, # part two of the stratified mean formula
#             stratvar1 = sum(wt_var1)) %>% # part two of the stratified variance formula
#   mutate(TYPE = paste("With Wind Included"), # paste identifying information of means and variances for joining and plotting in later scripts
#          SOURCE = paste("Historical"))
# 
# ### save the data 
# saveRDS(sf_nowind_stratmu, here("data", "rds", "sf_stratmu_included.rds"))
# 
# 
# 
# # calculate stratified means and variances for each year based on individual stratum means and variances from m6 simulated data
# m6_nowind_stratmu <- m6_nowind_mu %>% 
#   group_by(year) %>% 
#   summarise(stratmu1 = (sum(wt_mu1)) / BTSArea, # part two of the stratified mean formula
#             stratvar1 = sum(wt_var1), # part two of the stratified variance formula
#             stratmu2 = (sum(wt_mu2)) / BTSArea,
#             stratvar2 = sum(wt_var2)) %>% 
#   mutate(TYPE = paste("With Wind Included"), # paste identifying information of means and variances for joining and plotting in later scripts
#          SOURCE = paste("m6"))
# 
# ### save the data 
# saveRDS(m6_nowind_stratmu, here("data", "rds", "m6_stratmu_included.rds"))
# 
# 
# 
# # calculate stratified means and variances for each year based on individual stratum means and variances from m1 simulated data
# m1_nowind_stratmu <- m1_nowind_mu %>% 
#   group_by(year) %>% 
#   summarise(stratmu1 = (sum(wt_mu1)) / BTSArea, # part two of the stratified mean formula
#             stratvar1 = sum(wt_var1), # part two of the stratified variance formula
#             stratmu2 = (sum(wt_mu2)) / BTSArea,
#             stratvar2 = sum(wt_var2)) %>% 
#   mutate(TYPE = paste("With Wind Included"), # paste identifying information of means and variances for joining and plotting in later scripts
#          SOURCE = paste("m1"))
# 
# ### save the data 
# saveRDS(m1_nowind_stratmu, here("data", "rds", "m1_stratmu_included.rds"))
# 
#####