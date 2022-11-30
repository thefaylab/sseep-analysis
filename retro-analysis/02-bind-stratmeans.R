### created: 11/25/2022
### last updated: 11/25/2022

#### 02 - BIND STRATIFIED MEANS DATASETS ####

###################
#### OBJECTIVE ####
###################
# create a full dataset of stratified means for plotting. Dataset will contain stratified means and variances for species over time and an identifier of whether tows occurring in wind areas were incoporated in the calculation  

####################

#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


#### LOAD DATA ####
# dataset where tows occurring in wind areas were incorporated into mean and variance calculations
nowind_strat <- readRDS(here("data", "rds", "strat-mu_included.rds")) 

# dataset where tows occurring in wind areas were removed from mean and variance calculations
wind_strat <- readRDS(here("data", "rds", "strat-mu_preclusion.rds"))

# species dataframe for adding to final dataset 
species <- readRDS(here("data", "rds", "species.rds"))


#### BIND DATASETS ####
stratmeans <- bind_rows(nowind_strat, wind_strat) %>% #bind rows of the two datasets
  left_join(species, by = "SVSPP") %>% # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)


#### BIND DATASETS ####
saveRDS(stratmeans, here("data", "rds", "strat-mu_all.rds"))
