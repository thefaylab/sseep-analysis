### created:      07/11/2023
### last update:  01/26/2024
###

# 01c - SUMMARISE OBSERVATIONS IN OVERLAPPING STRATA ####

## Objective ####
# create summary statistics based on catch rates that occurred in strata that have potential overlap from offshore wind energy areas during the time series 

# Script will summarise quantities in strata with overlapping wind areas only:
# total catch 
# total biomass
# total number of tows

### LOAD PACKAGES ###
library(stringr)
library(patchwork)
library(here)
library(tidyverse)

here()
# location to store data 
init.analysis.dat <- here("data", "rds", "init-analysis")

### LOAD DATA ###
# presence/absence data created in `04-complete-datasets.R` here("tidy-data")
data <- readRDS(here("data", "rds", "completed_bts_data.rds"))
# data <- readRDS(here("data", "rds", "merged_data_complete.rds")) |>
#   mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT),  # fills expcatch wt values with 0 if missing
#          EXPCATCHNUM = ifelse(is.na(EXPCATCHNUM), 0, EXPCATCHNUM),
#          CODE = str_c(STRATUM, CRUISE6, STATION))

### DATA WRANGLE ###
# filter for wind observations only
wind <- filter(data, AREA == "WIND")

# pull out unique strata in wind area dataset indicating potentially impacted strata ###
impacted_strata <- unique(wind$STRATUM)

# filter observations by impacted strata only 
overlap <- data |>
  group_by(SVSPP, COMNAME) |>
  filter(STRATUM %in% impacted_strata)


## Summaries ####

### By species, year, season, time of day, and strata ###
overlap_sum <- overlap |>
  group_by(SVSPP, COMNAME, EST_YEAR, SEASON, STRATUM, DAYTIME, CODE) |>
  summarize(OVERLAP_CATCH = sum(EXPCATCHNUM),
            OVERLAP_BIO = sum(EXPCATCHWT),
            OVERLAP_TOW = length(unique(CODE)))|>
  mutate(TYPE = "IMPACTED STRATA") |>
  arrange(STRATUM)

### BY SPECIES ####
overlap_sp <- overlap_sum |>
  group_by(SVSPP, COMNAME) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH), 
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW)) |>
  mutate(TYPE = "IMPACTED STRATA")


### & year, season, and strata ###
overlap_sp_str_ssn_yr <- overlap_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, SEASON, STRATUM) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH),
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW)) |> 
  mutate(TYPE = "IMPACTED STRATA") |>
  arrange(SVSPP)


### & year, and season ###
overlap_sp_ssn_yr <- overlap_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, SEASON) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH),
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW)) |> 
  mutate(TYPE = "IMPACTED STRATA") |>
  arrange(SVSPP, EST_YEAR)


### & year, and strata ###
overlap_sp_str_yr <- overlap_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, STRATUM) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH),
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW)) |> 
  mutate(TYPE = "IMPACTED STRATA") |>
  arrange(SVSPP, EST_YEAR)


### & season, and strata ###
overlap_sp_ssn_strat <- overlap_sum |>
  group_by(SVSPP, COMNAME, SEASON, STRATUM) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH),
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW))|>
  mutate(TYPE = "IMPACTED STRATA") |>
  arrange(STRATUM)
#overlap_ssn_strat$CODE <- paste(overlap_ssn_strat$SVSPP, overlap_ssn_strat$SEASON, overlap_ssn_strat$STRATUM)


### & strata ###
overlap_sp_strat <- overlap_sum |>
  group_by(SVSPP, COMNAME, STRATUM) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH), 
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW))|>
  mutate(TYPE = "IMPACTED STRATA", 
         CODE = str_c(SVSPP, STRATUM)) |>
  arrange(STRATUM)
#overlap_sp_strat$CODE <- paste(overlap_sp_strat$SVSPP, overlap_sp_strat$STRATUM)


### & season ###
overlap_sp_ssn <- overlap_sum |>
  group_by(SVSPP, COMNAME, SEASON) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH), 
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW))|>
  mutate(TYPE = "IMPACTED STRATA") |>
  arrange(SVSPP)
#overlap_sp_ssn$CODE <- paste(overlap_sp_ssn$SVSPP, overlap_sp_ssn$SEASON)


### & year ###
overlap_sp_yr <- overlap_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH), 
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW))|>
  mutate(TYPE = "IMPACTED STRATA") |>
  arrange(SVSPP)
#overlap_sp_yr$CODE <- paste(overlap_sp_yr$SVSPP, overlap_sp_yr$YEAR)

### & season ###
overlap_sp_time <- overlap_sum |>
  group_by(SVSPP, COMNAME, DAYTIME) |> 
  summarize(OVERLAP_CATCH = sum(OVERLAP_CATCH), 
            OVERLAP_BIO = sum(OVERLAP_BIO),
            OVERLAP_TOW = sum(OVERLAP_TOW))|>
  mutate(TYPE = "IMPACTED STRATA") |>
  arrange(SVSPP)


### BY DAYTIME ####
overlap_time <- overlap |>
  group_by(DAYTIME) |>
  summarise(OVERLAP_CATCH = sum(EXPCATCHNUM), 
            OVERLAP_BIO = sum(EXPCATCHWT),
            OVERLAP_TOW = length(unique(CODE))) |>
  mutate(TYPE = "IMPACTED STRATA") 




### BY YEAR ####
overlap_yr <- overlap |>
  group_by(EST_YEAR) |>
  summarise(OVERLAP_CATCH = sum(EXPCATCHNUM), 
            OVERLAP_BIO = sum(EXPCATCHWT),
            OVERLAP_TOW = length(unique(CODE))) |>
  mutate(TYPE = "IMPACTED STRATA") 


### BY SEASON ####
overlap_ssn <- overlap |>
  group_by(SEASON) |>
  summarise(OVERLAP_CATCH = sum(EXPCATCHNUM), 
            OVERLAP_BIO = sum(EXPCATCHWT),
            OVERLAP_TOW = length(unique(CODE))) |>
  mutate(TYPE = "IMPACTED STRATA")  


### BY STRATA ####
overlap_strat <- overlap |>
  group_by(STRATUM) |>
  summarise(OVERLAP_CATCH = sum(EXPCATCHNUM), 
            OVERLAP_BIO = sum(EXPCATCHWT),
            OVERLAP_TOW = length(unique(CODE))) |>
  mutate(TYPE = "IMPACTED STRATA") 


## SAVE THE DATA ####
saveRDS(overlap_sum, here(init.analysis.dat, "overlap_summaries.rds"))
saveRDS(overlap_sp, here(init.analysis.dat, "overlap_species_summaries.rds"))
saveRDS(overlap_sp_str_ssn_yr, here(init.analysis.dat, "overlap_sp-str-ssn-yr_summary.rds"))
saveRDS(overlap_sp_ssn_yr, here(init.analysis.dat, "overlap_sp_ssn_yr_summary.rds"))
saveRDS(overlap_sp_ssn_strat, here(init.analysis.dat, "overlap_sp_ssn_strat_summary.rds"))
saveRDS(overlap_sp_str_yr, here(init.analysis.dat, "overlap_sp_yr_strat_summary.rds"))
saveRDS(overlap_sp_strat, here(init.analysis.dat, "overlap_sp-str_summary.rds"))
saveRDS(overlap_sp_ssn, here(init.analysis.dat, "overlap_sp-ssn_summary.rds"))
saveRDS(overlap_sp_yr, here(init.analysis.dat, "overlap_sp-yr_summary.rds"))
saveRDS(overlap_sp_time, here(init.analysis.dat, "overlap_sp-time_summary.rds"))
saveRDS(overlap_yr, here(init.analysis.dat, "overlap_yr_summary.rds"))
saveRDS(overlap_time, here(init.analysis.dat, "overlap_time_summary.rds"))
saveRDS(overlap_strat, here(init.analysis.dat, "overlap_strata_summary.rds"))
saveRDS(overlap_ssn, here(init.analysis.dat, "overlap_ssn_summary.rds"))