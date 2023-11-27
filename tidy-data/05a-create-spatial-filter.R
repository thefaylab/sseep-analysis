### created: 01/27/2023
### last updated: 11/27/2023 

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
# data <- readRDS(here("data", "rds", "merged_data_complete.rds")) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))
data <- readRDS(here("data", "rds", "completed_bts_data.rds")) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# dataset created from `03a-find-wind-intersection.R` here("tidy-data)
impacted_strata <- readRDS(here("data", "rds", "impacted_strata.rds"))
strata <- impacted_strata$STRATUM

# specieslookup <- data %>% 
#   select(SVSPP, COMNAME, SCINAME) %>%
#   unique()


## CREATE FILTER ####
### FILTER BY NUMBER OF STRATA AND YEARS ####
# find the number of strata where each species were present and observed by the survey
strat_filter <- data |>
  group_by(SVSPP, COMNAME, EST_YEAR, SEASON) |>
  filter(PRESENCE == 1) |> # filter for observations where species were caught
  summarise(strat_n = length(unique(STRATUM))) |>#,  # calculate number of strata species were observed 
            #year_n = length(unique(EST_YEAR))) 
  filter(strat_n >= 3)

# filter species where strat_n >= 3 and year_n >= 3 for each season
year_filter <- strat_filter |> 
  group_by(SVSPP, COMNAME, SEASON) |>
  summarise(year_n = length(EST_YEAR)) |>
  filter(year_n >= 3) |> 
  mutate(code = str_c(SVSPP, SEASON))


criteria_species <- year_filter |> 
   select(!year_n)

# number of species observed with this filter 
length(unique(criteria_species$SVSPP))  # n = 199 spp


### save data 
saveRDS(strat_filter, here("data", "rds", "spatial-filter", "min-strata_filtered-species.rds"))
saveRDS(criteria_species, here("data", "rds", "spatial-filter", "min-year-strat_filtered-species.rds"))


### FILTER BY IMPACTED STRATA ####
# find the species that were observed in strata proposed for wind overlap
impacted_species <- data |>
  filter(PRESENCE == 1, # filter for observations where species were caught
         #SVSPP %in% species,
         STRATUM %in% strata) |> # filter for observations occurring in impacted strata 
  mutate(code = str_c(SVSPP, SEASON)) |> 
  select(c(SVSPP, COMNAME, SEASON, code))
  
# number of species observed with this filter
length(unique(impacted_species$SVSPP)) # n = 241

### save data 
saveRDS(impacted_species, here("data", "rds", "spatial-filter", "wind-impacted-species.rds"))


### FIND SPECIES INTERSECTION ####
# find the species that appear in 3 or more strata, 3 or more years, and also any strata impacted by offshore wind
species_df <- semi_join(impacted_species, criteria_species, by = "code") # filtering join, filter x, by matches in y
species <- unique(species_df$SVSPP)

#### REMOVED SPECIES ####
# identify the species that was removed by the criteria above.
# anti_species_df <- anti_join(impacted_species, criteria_species, by = "code") # filtering join, filter x, by matches in y
# anti_species <- unique(anti_species_df$SVSPP)
# removed <-filter(specieslookup, SVSPP %in% anti_species)

### save data 
saveRDS(species_df, here("data", "rds", "spatial-filter", "spatial-filter-species.rds"))
# saveRDS(removed, here("data", "rds", "spatial-filter", "removed-species.rds"))



## CALCULATE TOTAL BIOMASS ####
#find the total biomass for each species and season combination by strata 
total_bio <- data |> 
  filter(SVSPP %in% species) |> 
  group_by(SVSPP, COMNAME, SEASON, STRATUM) |> 
  summarise(EXPCATCHWT = sum(EXPCATCHWT), .groups = "drop") |>  # sum biomass by groups, and drop groups once calculation is complete 
  group_by(SVSPP, COMNAME, SEASON) |> #regroup by common name and season
  arrange(desc(EXPCATCHWT)) #arrange dataframe in descending order based on total biomass and stratum.

### save data 
saveRDS(total_bio, here("data", "rds", "spatial-filter", "total-biomass_impacted-species.rds"))


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
saveRDS(csum_dist, here("data", "rds", "spatial-filter", "cumul-biomass_impacted-species.rds"))

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

