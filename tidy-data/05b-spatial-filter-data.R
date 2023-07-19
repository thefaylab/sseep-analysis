### created: 02/20/2023
### last updated: 07/19/2023

# 05b - SPATIALLY FILTER DATASETS ####


## OBJECTIVE ####
# filter the full complete bottom trawl dataset based on the 95% and 99% cumulative distribution of biomass for each species and season in a given strata. Filtered data will be used for the remaining analyses. 

# script will:
# read in cumulative distribution datasets 
# filter the bottom trawl survey data using a filtering join based on species code, season, and stratum 

# outputs include: 
# two minimized dataframes of the bottom trawl survey data containing observations occurring in strata that make up the 95% and 99% cumulative biomass of each species in a given season.


### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


### LOAD DATA ####
# dataset created from `04-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow. 
data <- readRDS(here("data", "rds", "merged_data_complete.rds")) %>% mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# 95% cumulative distribution dataset created from `05a-spatial-filter.R` here("tidy-data"). 
filter95 <- readRDS(here("data", "rds", "spatial-filter", "cumul-biomass95.rds")) 

# 99% cumulative distribution dataset created from `03a-spatial-filter.R` here("tidy-data"). 
filter99 <- readRDS(here("data", "rds", "spatial-filter", "cumul-biomass99.rds")) 



## FILTER AND JOIN DATASETS ####
# filter the full dataset based on the same observations that appear in the 95% cumulative distribution dataset
data95 <- semi_join(data, filter95, by = c("SVSPP", "SEASON", "STRATUM"))

### save data 
saveRDS(data95, here("data", "rds", "95filtered_complete_bts.rds"))



# filter the full dataset based on the same observations that appear in the 99% cumulative distribution dataset
data99 <- semi_join(data, filter99, by = c("SVSPP", "SEASON", "STRATUM"))
# filters from 3962124 to 287533 -- 10% of the full data set? 

### save data 
saveRDS(data99, here("data", "rds", "99filtered_complete_bts.rds"))


#### COMPARE TOW COUNTS ####
# to compare the number of tows filtered to the number of tows in the dataset 
#data %>% filter(SVSPP == 141, ) %>% count() #8102
#1569/8102 # 20% of data 

#check the number of tows in each strata - how many tows were in that strata compared to the total number of tows 

#total number of tows 
# data %>% 
#   filter(SVSPP == 141) %>%
#   # mutate(code = str_c(STRATUM, SEASON)) %>%
#   #group_by(STRATUM, SEASON) %>%
#   summarise(ct = length(unique(str_c(CRUISE6, STRATUM, SEASON, STATION)))) #8102
# 
# # number of tows were in each strata
# bsba<- data %>% 
#   filter(SVSPP == 141) %>%
#  # mutate(code = str_c(STRATUM, SEASON)) %>%
#   group_by(STRATUM, SEASON) %>%
#   summarise(ct = length(unique(str_c(CRUISE6, STRATUM, SEASON, STATION))))
# 
# bsbb<-data95 %>% 
#   filter(SVSPP == 141) %>%
#   # mutate(code = str_c(STRATUM, SEASON)) %>%
#   group_by(STRATUM, SEASON) %>%
#   summarise(ct95 = length(unique(str_c(CRUISE6, STRATUM, SEASON, STATION))))
# 
# bsbc<-data99 %>%
#   filter(SVSPP == 141) %>%
#   # mutate(code = str_c(STRATUM, SEASON)) %>%
#   group_by(STRATUM, SEASON) %>%
#   summarise(ct99 = length(unique(str_c(CRUISE6, STRATUM, SEASON, STATION))))
# 
# # join to compare/see which strata and how many tows were selected 
# bsba <- bsba %>% 
#   left_join(bsbb, by = c("STRATUM", "SEASON")) %>% 
#   left_join(bsbc, by = c("STRATUM", "SEASON"))
# 
# bsba %>% 
#   ungroup() %>%
#   summarise(ct = sum(ct), 
#          ct95 = sum(ct95, na.rm = TRUE), 
#          ct99 = sum(ct99, na.rm = TRUE))
# 
# # bsb <- csum_dist95 %>%  
# #   filter(SVSPP == 141, SEASON == "FALL") %>%
# #   select(STRATUM)
# # bsb2 <- csum_dist99 %>%  
# #   filter(SVSPP == 141) %>%
# #   select(STRATUM)
# 
