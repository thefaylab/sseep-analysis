### created: 1/23/2022
### last updated: 01/26/2024

#  01 - NULL HYPOTHESIS TESTING: RESAMPLE OBSERVED DATA ####

## OBJECTIVE ####
# Two different proportions of observed tows were resampled using bootstrap resampling. The first resampling included a sampling size equivalent to the full dataset, emulating a calculation that would include wind tows. Thus the random sample size with the same proportion of wind tows included is 100$ of the original dataset.

# The second included a random resampling with a sampling size equivalent to the dataset when wind tows were precluded from the survey observations. Wind tows made up 10% of the original dataset, thus the random sample size with the same proportion of wind tows excluded is equal to 90% of the original dataset.   


### LOAD PACKAGES ####
library(tidyverse)
library(here)
library(sf)
library(patchwork)
library(boot)
library(infer)
library(broom)

null.hyp.data <- here("data", "sumflounder", "null-hypothesis") 


### LOAD DATA ####
set.seed(120)

# summer flounder data consisting of observations filtered based on seasonal footprints created here("sumflounder", "04-filter-summer-flounder.R")
data <- readRDS(here("data", "sumflounder", "sumflounder.rds"))

## CALCULATE THE PROPORTION OF WIND TOWS ####
#count the number of wind tows and divide by the total number of tows to find the proportion made up by wind tows  
length(which(data$AREA == "WIND"))/length(data$AREA) 
# ~10% of tows are wind tows 


## RESAMPLE FOR THE FULL DATA ####
# resample from the historical observations to create a dataframe of the same size, and perform 1000 times. 
full_reps <- data |>
  rep_slice_sample(reps = 1000, replace = TRUE, prop = 1)


## RESAMPLE FOR THE PRECLUSION DATA ####
# resample from the historical observations to create a dataframe that is the same size as when you remove offshore wind tows to emulate a precluded dataset, and perform 1000 times.  
wind_reps <- data |>
  rep_slice_sample(reps = 1000, replace = TRUE, prop = 0.90)



### save the data 
saveRDS(wind_reps, here(null.hyp.data, "winddat_resample.rds"))
saveRDS(full_reps, here(null.hyp.data, "fulldat_resample.rds"))
