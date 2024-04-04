### created:      2/15/2022
### last update:  04/04/2024
###

# 01a - SUMMARISE ALL BTS OBSERVATIONS ####

## Objective ####
# create summary statistics based on all the catch rate data during the time series

# Script will summarise: 
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
# presence data indexed according to wind area overlap created in `03b-index-wind-observations.R` here("tidy-data")
data <- readRDS(here("data", "rds", "tidy-data", "full-bts-indexed.rds")) |>
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT), # fills expcatch wt values with 0 if missing
         EXPCATCHNUM = ifelse(is.na(EXPCATCHNUM), 0, EXPCATCHNUM))
# #          CODE = str_c(STRATUM, CRUISE6, STATION)) 

## Summaries ####

### By species, year, season, time of day, and strata
bts_sum <- data |>
  group_by(SVSPP, EST_YEAR, SEASON, COMNAME, STRATUM, TOWID, DAYTIME) |> 
  summarize(BTS_CATCH = sum(EXPCATCHNUM), 
            BTS_BIO = sum(EXPCATCHWT),
            BTS_TOW = length(unique(TOWID))) |>
  mutate(TYPE = "WHOLE") |>
  arrange(SVSPP, EST_YEAR)

### BY SPECIES ####

bts_sp <- bts_sum |>
  group_by(SVSPP, COMNAME) |> 
  summarize(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW)) |>
  mutate(TYPE = "WHOLE") |> 
  arrange(SVSPP)

### & year, strata, and season ###
bts_sp_str_ssn_yr <- bts_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, SEASON, STRATUM) |> 
  summarize(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW))|>
  mutate(TYPE = "WHOLE") |>
  arrange(SVSPP, EST_YEAR)

### & year, and season ###
bts_sp_ssn_yr <- bts_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, SEASON) |> 
  summarize(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW))|>
  mutate(TYPE = "WHOLE") |>
  arrange(SVSPP, EST_YEAR)


### & season, and strata ###
bts_sp_ssn_strat <- bts_sum |>
  group_by(SVSPP, COMNAME, SEASON, STRATUM) |> 
  summarize(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW))|>
  mutate(TYPE = "WHOLE") |>
  arrange(STRATUM)
#bts_ssn_strat$CODE <- paste(bts_ssn_strat$SVSPP, bts_ssn_strat$SEASON, bts_ssn_strat$STRATUM)

### & year, and strata ###
bts_sp_yr_strat <- bts_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR, STRATUM) |> 
  summarize(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW))|>
  mutate(TYPE = "WHOLE") |>
  arrange(EST_YEAR, STRATUM)
#bts_ssn_strat$CODE <- paste(bts_ssn_strat$SVSPP, bts_ssn_strat$SEASON, bts_ssn_strat$STRATUM)

### & year ###
bts_sp_yr <- bts_sum |>
  group_by(SVSPP, COMNAME, EST_YEAR) |> 
  summarize(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW))|>
  mutate(TYPE = "WHOLE") |>
  arrange(SVSPP)
#bts_sp_yr$CODE <- paste(bts_sp_yr$SVSPP, bts_sp_yr$YEAR)

### & strata ###
bts_sp_strat <- bts_sum |>
  group_by(SVSPP, COMNAME, STRATUM) |> 
  summarize(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW))|>
  mutate(TYPE = "WHOLE", 
         CODE = str_c(SVSPP, STRATUM)) |>
  arrange(SVSPP)
#bts_sp_strat$CODE <- paste(bts_sp_strat$SVSPP, bts_sp_strat$STRATUM)

### & season ###
bts_sp_ssn <- bts_sum |>
  group_by(SVSPP, COMNAME, SEASON) |> 
  summarize(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW)) |>
  mutate(TYPE = "WHOLE") |>
  arrange(SVSPP)
#bts_sp_ssn$CODE <- paste(bts_sp_ssn$SVSPP, bts_sp_ssn$SEASON)

### & time of day ###
bts_sp_time <- bts_sum |>
  group_by(SVSPP, COMNAME, DAYTIME) |>
  summarise(BTS_CATCH = sum(BTS_CATCH), 
            BTS_BIO = sum(BTS_BIO),
            BTS_TOW = sum(BTS_TOW)) |>
  mutate(TYPE = "WHOLE")
#bts_sp_time$CODE <- paste(bts_sp_time$SVSPP, bts_sp_time$DAYTIME)



### BY YEAR ####
bts_yr <- data |>
  group_by(EST_YEAR) |> 
  summarize(BTS_CATCH = sum(EXPCATCHNUM), 
            BTS_BIO = sum(EXPCATCHWT),
            BTS_TOW = length(unique(TOWID))) |>
  mutate(TYPE = "WHOLE")



### BY TIME OF DAY ####
bts_time <- data |>
  group_by(DAYTIME) |>
  summarise(BTS_CATCH = sum(EXPCATCHNUM), 
            BTS_BIO = sum(EXPCATCHWT),
            BTS_TOW = length(unique(TOWID))) |>
  mutate(TYPE = "WHOLE") 



### BY STRATA ####
bts_strat <- data |>
  group_by(STRATUM) |> 
  summarize(BTS_CATCH = sum(EXPCATCHNUM), 
            BTS_BIO = sum(EXPCATCHWT),
            BTS_TOW = length(unique(TOWID)))|>
  mutate(TYPE = "WHOLE") |>
  arrange(STRATUM)
#bts_sp_strat$CODE <- paste(bts_sp_strat$SVSPP, bts_sp_strat$STRATUM)



### BY SEASON ####
bts_ssn <- data |>
  group_by(SEASON) |> 
  summarize(BTS_CATCH = sum(EXPCATCHNUM), 
            BTS_BIO = sum(EXPCATCHWT),
            BTS_TOW = length(unique(TOWID))) |>
  mutate(TYPE = "WHOLE")
#bts_sp_ssn$CODE <- paste(bts_sp_ssn$SVSPP, bts_sp_ssn$SEASON)


## SAVE THE DATA ####
saveRDS(bts_sum, here(init.analysis.dat, "bts_summaries.rds"))
saveRDS(bts_sp, here(init.analysis.dat, "bts_species_summaries.rds"))
saveRDS(bts_sp_str_ssn_yr, here(init.analysis.dat, "bts_sp-str-ssn-yr_summary.rds"))
saveRDS(bts_sp_ssn_yr, here(init.analysis.dat, "bts_sp-ssn-yr_summary.rds"))
saveRDS(bts_sp_ssn_strat, here(init.analysis.dat, "bts_sp-ssn-str_summary.rds"))
saveRDS(bts_sp_yr_strat, here(init.analysis.dat, "bts_sp-yr-str_summary.rds"))
saveRDS(bts_sp_yr, here(init.analysis.dat, "bts_sp-yr_summary.rds"))
saveRDS(bts_sp_strat, here(init.analysis.dat, "bts_sp-str_summary.rds"))
saveRDS(bts_sp_ssn, here(init.analysis.dat, "bts_sp-ssn_summary.rds"))
saveRDS(bts_sp_time, here(init.analysis.dat, "bts_sp-time_summary.rds"))
saveRDS(bts_yr, here(init.analysis.dat, "bts_yr_summary.rds"))
saveRDS(bts_time, here(init.analysis.dat, "bts_time_summary.rds"))
saveRDS(bts_strat, here(init.analysis.dat, "bts_strata_summary.rds"))
saveRDS(bts_ssn, here(init.analysis.dat, "bts_ssn_summary.rds"))