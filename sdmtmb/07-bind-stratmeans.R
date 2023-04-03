### created: 12/10/2022
### last updated: 

#### 05 - BIND STRATIFIED MEANS DATASETS ####

###################
#### OBJECTIVE ####
###################
# create a full dataset of stratified means for plotting. Dataset will contain stratified means and variances for summer flounder over time and identifiers of whether tows occurring in wind areas were incoporated in the calculation as well as if the data is from the historical dataset or the simulated datasets generated from models m1 and m6. 

####################

#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


#### LOAD DATA ####
# datasets where tows occurring in wind areas were incorporated into mean and variance calculations
sf_nowind_strat <- readRDS(here("data", "rds", "sf_stratmu_included.rds")) 
m6_nowind_strat <- readRDS(here("data", "rds", "m6_stratmu_included.rds")) 
m1_nowind_strat <- readRDS(here("data", "rds", "m1_stratmu_included.rds")) 

# datasets where tows occurring in wind areas were removed from mean and variance calculations
sf_wind_strat <- readRDS(here("data", "rds", "sf_stratmu_preclusion.rds"))
m6_wind_strat <- readRDS(here("data", "rds", "m6_stratmu_preclusion.rds"))
m1_wind_strat <- readRDS(here("data", "rds", "m1_stratmu_preclusion.rds"))



#### BIND DATASETS ####
sf_stratmeans <- bind_rows(sf_nowind_strat, sf_wind_strat) %>%  #bind rows of the two datasets
  mutate(stratmu2 = stratmu1, 
         stratvar2 = stratvar1)
m6_stratmeans <- bind_rows(m6_nowind_strat, m6_wind_strat) #bind rows of the two datasets
m1_stratmeans <- bind_rows(m1_nowind_strat, m1_wind_strat) #bind rows of the two datasets


stratmeans <- bind_rows(sf_stratmeans, m6_stratmeans, m1_stratmeans)


#### SAVE DATASETS ####
saveRDS(sf_stratmeans, here("data", "rds", "sf_stratmu_all.rds"))
saveRDS(m6_stratmeans, here("data", "rds", "m6_stratmu_all.rds"))
saveRDS(m1_stratmeans, here("data", "rds", "m1_stratmu_all.rds"))
saveRDS(stratmeans, here("data", "rds", "stratmu_all.rds"))
