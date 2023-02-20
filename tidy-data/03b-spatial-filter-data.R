### created: 2/20/2023
### last updated: 

#### 03b - SPATIALLY FILTER DATASETS ####

###################
#### OBJECTIVE ####
###################
# filter the full complete bottom trawl datasets based on the 95% and 99% cumulative distribution of biomass for each species and season in a given strata

# filtered data will be used for the remaining analyses. 

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

# 95% cumulative distribution dataset created from `03a-spatial-filter.R` here("tidy-data"). 
csum_dist95 <- readRDS(here("data", "rds", "cumul-dist-biomass95.rds")) 

# 99% cumulative distribution dataset created from `03a-spatial-filter.R` here("tidy-data"). 
csum_dist99 <- readRDS(here("data", "rds", "cumul-dist-biomass99.rds")) 



#### FILTER AND JOIN DATASETS ####
# filter the full dataset based on the same observations that appear in the 95% cumulative distribution dataset
data95 <- semi_join(data, csum_dist95, by = c("SVSPP", "SEASON", "STRATUM"))
# filters from 3962124 to 287533 -- 7% of the full data set? 

### save data 
saveRDS(data95, here("data", "rds", "95filtered_complete_bts.rds"))

# filter the full dataset based on the same observations that appear in the 99% cumulative distribution dataset
data99 <- semi_join(data, csum_dist99, by = c("SVSPP", "SEASON", "STRATUM"))
# filters from 3962124 to 287533 -- 10% of the full data set? 

### save data 
saveRDS(data99, here("data", "rds", "99filtered_complete_bts.rds"))


#### COMPARE TOW COUNTS ####
# to compare the number of tows filtered to the number of tows in the dataset 
#data %>% filter(SVSPP == 141, ) %>% count() #8102
#1569/8102 # 20% of data 

#check the number of tows in each strata - how many tows were in that strata compared to the total number of tows 

#total number of tows 
data %>% 
  filter(SVSPP == 141) %>%
  # mutate(code = str_c(STRATUM, SEASON)) %>%
  #group_by(STRATUM, SEASON) %>%
  summarise(ct = length(unique(str_c(CRUISE6, STRATUM, SEASON, STATION)))) #8102

# number of tows were in each strata
bsba<- data %>% 
  filter(SVSPP == 141) %>%
 # mutate(code = str_c(STRATUM, SEASON)) %>%
  group_by(STRATUM, SEASON) %>%
  summarise(ct = length(unique(str_c(CRUISE6, STRATUM, SEASON, STATION))))

bsbb<-data95 %>% 
  filter(SVSPP == 141) %>%
  # mutate(code = str_c(STRATUM, SEASON)) %>%
  group_by(STRATUM, SEASON) %>%
  summarise(ct95 = length(unique(str_c(CRUISE6, STRATUM, SEASON, STATION))))

bsbc<-data99 %>%
  filter(SVSPP == 141) %>%
  # mutate(code = str_c(STRATUM, SEASON)) %>%
  group_by(STRATUM, SEASON) %>%
  summarise(ct99 = length(unique(str_c(CRUISE6, STRATUM, SEASON, STATION))))

# join to compare/see which strata and how many tows were selected 
bsba <- bsba %>% 
  left_join(bsbb, by = c("STRATUM", "SEASON")) %>% 
  left_join(bsbc, by = c("STRATUM", "SEASON"))

bsba %>% 
  ungroup() %>%
  summarise(ct = sum(ct), 
         ct95 = sum(ct95, na.rm = TRUE), 
         ct99 = sum(ct99, na.rm = TRUE))

# bsb <- csum_dist95 %>%  
#   filter(SVSPP == 141, SEASON == "FALL") %>%
#   select(STRATUM)
# bsb2 <- csum_dist99 %>%  
#   filter(SVSPP == 141) %>%
#   select(STRATUM)

