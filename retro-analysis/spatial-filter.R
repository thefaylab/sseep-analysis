### created: 1/27/2022
### last updated: 

#### 00 - SPATIAL FILTERING ####

###################
#### OBJECTIVE ####
###################
# 

####################


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


#### LOAD DATA ####
# dataset created from `02-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow. 
data <- readRDS(here("data", "rds", "merged_data_complete.rds")) %>% mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))


#### FILTER DATA BASED ON STRATA ####
#find the total biomass for each species and season combination by strata 
total_bio <- data %>% 
  group_by(COMNAME, SEASON, STRATUM) %>% 
  summarise(EXPCATCHWT = sum(EXPCATCHWT), .groups = "drop") %>%  # sum biomass by groups, and drop groups once calculation is complete 
  group_by(COMNAME, SEASON) %>% #regroup by common name and season
  arrange(desc(EXPCATCHWT), desc(STRATUM)) #arrange dataframe in descending order based on total biomass and stratum.

### save data 
saveRDS(total_bio, here("data", "rds", "total_biomass.rds"))


# FIND CUMULATIVE DISTRIBUTION 
# test data
summerflounder <- total_bio %>% 
  filter(COMNAME == "SUMMER FLOUNDER")


# cumulative distribution function 
cd <- ecdf(summerflounder$EXPCATCHWT)

plot(cd, verticals = TRUE, col.points = "blue", col.hor = "red", col.vert = "bisque")

# call function against biomass; returns the percentiles for biomass
x <- cd(summerflounder$EXPCATCHWT)

# attach to test dataframe 
summerflounder <- bind_cols(summerflounder, x)



  
