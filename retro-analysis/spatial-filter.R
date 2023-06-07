### created: 1/27/2023
### last updated: 2/6/2023 

#### 01 - SPATIAL FILTERING ####

###################
#### OBJECTIVE ####
###################
# find the strata that make up 95% of the cumulative distribution of biomass for each species and season, and use it to filter the dataset so that the stratified means are only calculated with the strata that have 95% of the cumulative biomass. 

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


#### FIND TOTAL BIOMASS ####
#find the total biomass for each species and season combination by strata 
total_bio <- data %>% 
  group_by(SVSPP, COMNAME, SEASON, STRATUM) %>% 
  summarise(EXPCATCHWT = sum(EXPCATCHWT), .groups = "drop") %>%  # sum biomass by groups, and drop groups once calculation is complete 
  group_by(SVSPP, COMNAME, SEASON) %>% #regroup by common name and season
  arrange(desc(EXPCATCHWT)) #arrange dataframe in descending order based on total biomass and stratum.

### save data 
saveRDS(total_bio, here("data", "rds", "total_biomass.rds"))


#### FIND CUMULATIVE DISTRIBUTION ####
# 

#### Test Data ####
# summerflounder_fall <- total_bio %>% 
#   filter(COMNAME == "SUMMER FLOUNDER", SEASON == "FALL") %>% 
#   mutate(csum_bio = cumsum(EXPCATCHWT),
#          prop = csum_bio/max(csum_bio))

#####
csum_dist <- total_bio %>% 
  group_by(SVSPP, COMNAME, SEASON) %>% 
  mutate(csum_bio = cumsum(EXPCATCHWT), # calculate the cumulative sum of biomass
         prop = round(csum_bio/max(csum_bio), 2)) # divide each value of cumulative sum by the maximum sum for each COMNAME and SEASON combination to find the proportion each sum makes up of the total distribution of sums

# filter 95% of the distribution 
csum_dist95 <- csum_dist |>
  filter(prop <= 0.95) # filter the data where the proportion is less than or equal to 95% of the total distribution of biomass for each COMNAME and SEASON group to identify which strata make up the majority of the biomass.
  

### save data 
#saveRDS(csum_dist95, here("data", "rds", "cumul-dist-biomass95.rds"))

# filter 99% of the distribution
csum_dist99 <- csum_dist |> 
  filter(prop <= 0.99) # filter the data where the proportion is less than or equal to 99% of the total distribution of biomass for each COMNAME and SEASON group to identify which strata make up the majority of the biomass.

### save data 
#saveRDS(csum_dist99, here("data", "rds", "cumul-dist-biomass99.rds"))


#### FILTER FULL DATASET ####
# filter the full dataset based on the same observations that appear in the cumulative distribution dataset
data95 <- semi_join(data, csum_dist95, by = c("SVSPP", "SEASON", "STRATUM"))

# filters from 3962124 to 287533 -- 7% of the full data set? 

data99 <- semi_join(data, csum_dist99, by = c("SVSPP", "SEASON", "STRATUM"))
# filters from 3962124 to 287533 -- 10% of the full data set? 

### save data 
#saveRDS(data, here("data", "rds", "filtered_complete_bts.rds"))