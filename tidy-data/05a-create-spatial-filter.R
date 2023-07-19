### created: 01/27/2023
### last updated: 07/19/2023 

# 05a - CREATE SPATIAL FILTER ####

## OBJECTIVE ####
# find the strata that make up 95%  and 99% of the cumulative distribution of biomass for each species and season

# script will:
# find species that were observed in 3 or more strata and over 3 or more years for each season
# find species that were observed in strata proposed for overlap by wind each season
# find the total biomass for each species in each season
# find the cumulative biomass for each species in each season
# find the proportion of cumulative biomass for each species in each season
# remove biomass observations and strata that make up more than 95% and 99% of the cumulative biomass of each species

# output includes: 
# a dataframe containing information about species that meet the minimum criteria for stratified analyses (3 years and 3 strata)
# a dataframe containing information about the species most likely to interact with offshore wind areas 
# two dataframes that contain information about the proportion of biomass observations that make up no more than 95% or 99% cumulative distribution of total observations over the full time series for each species in a given season

### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


### LOAD DATA ####
# dataset created from `04-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow. 
data <- readRDS(here("data", "rds", "merged_data_complete.rds")) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# dataset created from `03a-find-wind-intersection.R` here("tidy-data)
impacted_strata <- readRDS(here("data", "rds", "impacted_strata.rds"))
strata <- impacted_strata$STRATUM


## CREATE FILTER ####
### FILTER BY NUMBER OF STRATA AND YEARS ####
# find the number of strata where each species was present and observed by the survey
filter_n <- data |> 
  group_by(SVSPP, SEASON) |>
  filter(PRESENCE == 1) |> # filter for observations where species were caught
  summarise(strat_n = length(unique(STRATUM)),  # calculate number of strata species were observed 
            year_n = length(unique(EST_YEAR)))

# filter species where strat_n >= 3 and year_n >= 3 for each season
filter_sp <- filter_n |> 
  group_by(SVSPP, SEASON) |>
  filter(strat_n >= 3 & year_n >= 3) |> 
  mutate(code = str_c(SVSPP, SEASON))

# number of species observed with this filter 
#filtered_species <- unique(filter_sp$SVSPP)  # n = 311 spp

### save data 
saveRDS(filter_n, here("data", "rds", "spatial-filter", "filtering-summaries.rds"))
saveRDS(filter_sp, here("data", "rds", "spatial-filter", "criteria-filtered-species.rds"))

### FILTER BY IMPACTED STRATA ####
# find the species that were observed in strata proposed for wind overlap
species_imp <- data |>
  dplyr::group_by(SVSPP, SEASON) |>
  filter(PRESENCE == 1, # filter for observations where species were caught
         STRATUM %in% strata) |> # filter for observations occurring in impacted strata 
  mutate(code = str_c(SVSPP, SEASON))
  
# number of species observed with this filter 
#impacted_species <- unique(species_imp$code) # n = 241

### save data 
saveRDS(species_imp, here("data", "rds", "spatial-filter", "wind-impacted-species.rds"))

### FIND SPECIES INTERSECTION ####
# find the species that appear in 3 or more strata, 3 or more years, and also any strata impacted by offshore wind
species_df <- semi_join(filter_sp, species_imp, by = "code") # filtering join, filter x, by matches in y
species <- unique(species_df$SVSPP)

### save data 
saveRDS(species_df, here("data", "rds", "spatial-filter", "spatial-filter-species.rds"))


## CALCULATE TOTAL BIOMASS ####
#find the total biomass for each species and season combination by strata 
total_bio <- data |> 
  filter(SVSPP %in% species) |> 
  group_by(SVSPP, COMNAME, SEASON, STRATUM) |> 
  summarise(EXPCATCHWT = sum(EXPCATCHWT), .groups = "drop") |>  # sum biomass by groups, and drop groups once calculation is complete 
  group_by(SVSPP, COMNAME, SEASON) |> #regroup by common name and season
  arrange(desc(EXPCATCHWT)) #arrange dataframe in descending order based on total biomass and stratum.

### save data 
saveRDS(total_bio, here("data", "rds", "spatial-filter", "total_biomass.rds"))


## CALCULATE CUMULATIVE DISTRIBUTION ####

#### Test Data ###
# summerflounder_fall <- total_bio |> 
#   filter(COMNAME == "SUMMER FLOUNDER", SEASON == "FALL") |> 
#   mutate(csum_bio = cumsum(EXPCATCHWT),
#          prop = csum_bio/max(csum_bio))

csum_dist <- total_bio |> 
  group_by(SVSPP, COMNAME, SEASON) |> 
  mutate(csum_prop = round((cumsum(EXPCATCHWT)/max(cumsum(EXPCATCHWT))),2)) # calculate the cumulative sum of biomass; divide each value of cumulative sum by the maximum sum for each COMNAME and SEASON combination to find the proportion each sum makes up of the total distribution of sums

### save the data
saveRDS(csum_dist, here("data", "rds", "spatial-filter", "cumulative_biomass.rds"))

### 95% of the distribution ####
filter95 <- csum_dist |> 
  filter(csum_prop <= 0.95) |> # filter the data where the proportion is less than or equal to 95% of the total distribution of biomass for each COMNAME and SEASON group to identify which strata make up the majority of the biomass
  arrange(SVSPP)

### save data 
saveRDS(filter95, here("data", "rds", "spatial-filter", "cumul-biomass95.rds"))

### 99% of the distribution ####
filter99 <- csum_dist |> 
  filter(csum_prop <= 0.99) |> # filter the data where the proportion is less than or equal to 99% of the total distribution of biomass for each COMNAME and SEASON group to identify which strata make up the majority of the biomass
  arrange(SVSPP)
  

### save data 
saveRDS(filter99, here("data", "rds", "spatial-filter", "cumul-biomass99.rds"))

