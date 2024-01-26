### created:      07/11/2023     
### last update:  01/26/2024
###

# 01b - SUMMARISE ALL WIND OBSERVATIONS ####

## Objective ####
# create summary statistics based on catch rates that have the potential to have occurred in offshore wind energy TYPEs during the time series 

# Script will summarise indexed wind tows to calculate: 
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
# presence data only created in `03b-index-wind-observations.R` here("tidy-data")
data <- readRDS(here("data", "rds", "tidy-data", "full-bts-indexed.rds"))  |>
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT), # fills expcatch wt values with 0 if missing
         EXPCATCHNUM = ifelse(is.na(EXPCATCHNUM), 0, EXPCATCHNUM))
# #          CODE = str_c(STRATUM, CRUISE6, STATION))  

# filter for wind observations only
wind <- filter(data, AREA == "WIND")


## Summaries ####

### By species, year, season, time of day, and strata ###
wind_sum <- wind |>
  group_by(SVSPP, EST_YEAR, SEASON, COMNAME, STRATUM, DAYTIME, CODE) |>
  summarize(WIND_CATCH = sum(EXPCATCHNUM), 
            WIND_BIO = sum(EXPCATCHWT),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") |>
  arrange(SVSPP, EST_YEAR)

### BY SPECIES ####
wind_sp <- wind_sum |>
  group_by(SVSPP, COMNAME) |> 
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND")

### & season, and strata ###
wind_sp_strat_ssn_yr <- wind_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, SEASON, STRATUM) |> 
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") |>
  arrange(STRATUM)

### & year, and season ###
wind_sp_ssn_yr <- wind_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, SEASON) |>
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") |>
  arrange(SVSPP, EST_YEAR)

### & season, and strata ###
wind_sp_ssn_strat <- wind_sum |>
  group_by(SVSPP, COMNAME, SEASON, STRATUM) |> 
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") |>
  arrange(STRATUM)

### & year, and strata ### 
wind_sp_yr_strat <- wind_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, STRATUM) |> 
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") |>
  arrange(STRATUM)


### & strata ###
wind_sp_strat <- wind_sum |>
  group_by(SVSPP, COMNAME, STRATUM) |> 
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND", 
         CODE = str_c(SVSPP, STRATUM)) |>
  arrange(STRATUM)


### & season ###
wind_sp_ssn <- wind_sum |>
  group_by(SVSPP, COMNAME, SEASON) |> 
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") |>
  arrange(SVSPP)

### & year ###
wind_sp_yr <- wind_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR) |> 
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") |>
  arrange(SVSPP)

### & time of day ###
wind_sp_time <- wind_sum |>
  group_by(SVSPP, COMNAME, DAYTIME) |>
  summarize(WIND_CATCH = sum(WIND_CATCH), 
            WIND_BIO = sum(WIND_BIO),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND")



### BY YEAR ####
wind_yr <- wind |>
  group_by(EST_YEAR) |> 
  summarize(WIND_CATCH = sum(EXPCATCHNUM), 
            WIND_BIO = sum(EXPCATCHWT),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND")



### BY DAYTIME ####
wind_time <- wind |>
  group_by(DAYTIME) |>
  summarise(WIND_CATCH = sum(EXPCATCHNUM), 
            WIND_BIO = sum(EXPCATCHWT),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") 


### BY STRATA ####
wind_strat <- wind |>
  group_by(STRATUM) |> 
  summarize(WIND_CATCH = sum(EXPCATCHNUM), 
            WIND_BIO = sum(EXPCATCHWT),
            WIND_TOW = length(unique(CODE)))|>
  mutate(TYPE = "WIND") |>
  arrange(STRATUM)
#bts_sp_strat$CODE <- paste(bts_sp_strat$SVSPP, bts_sp_strat$STRATUM)


### BY SEASON ####
wind_ssn <- wind |>
  group_by(SEASON) |> 
  summarize(WIND_CATCH = sum(EXPCATCHNUM), 
            WIND_BIO = sum(EXPCATCHWT),
            WIND_TOW = length(unique(CODE))) |>
  mutate(TYPE = "WIND") 
#bts_sp_ssn$CODE <- paste(bts_sp_ssn$SVSPP, bts_sp_ssn$SEASON)

## SAVE THE DATA ####
saveRDS(wind_sum, here(init.analysis.dat, "wind_summaries.rds"))
saveRDS(wind_sp, here(init.analysis.dat, "wind_species_summaries.rds"))
saveRDS(wind_sp_strat_ssn_yr, here(init.analysis.dat, "wind_sp-str-ssn-yr_summary.rds"))
saveRDS(wind_sp_ssn_yr, here(init.analysis.dat, "wind_sp_ssn_yr_summary.rds"))
saveRDS(wind_sp_ssn_strat, here(init.analysis.dat, "wind_sp_ssn_strat_summary.rds"))
saveRDS(wind_sp_yr_strat, here(init.analysis.dat, "wind_sp_yr_strat_summary.rds"))
saveRDS(wind_sp_strat, here(init.analysis.dat, "wind_sp-str_summary.rds"))
saveRDS(wind_sp_ssn, here(init.analysis.dat, "wind_sp-ssn_summary.rds"))
saveRDS(wind_sp_yr, here(init.analysis.dat, "wind_sp-yr_summary.rds"))
saveRDS(wind_sp_time, here(init.analysis.dat, "wind_sp-time_summary.rds"))
saveRDS(wind_yr, here(init.analysis.dat, "wind_yr_summary.rds"))
saveRDS(wind_time, here(init.analysis.dat, "wind_time_summary.rds"))
saveRDS(wind_strat, here(init.analysis.dat, "wind_strata_summary.rds"))
saveRDS(wind_ssn, here(init.analysis.dat, "wind_ssn_summary.rds"))

