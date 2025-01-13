### created:      07/11/2023
### last update:  11/11/2024
###

# 02a - COMPARE SUMMARIES ####

## Objective ####
# create dataframes for comparing percent loss by combining summary statistic dataframes created in 01a-01c scripts.

# Script will: 
# join the whole time series summary, indexed wind tows only summary, and the overlapped strata only summary data frames by strata, year, species, season, and time of day
# join the whole time series summary, indexed wind tows only summary, and the overlapped strata only summary data frames by strata
# join the whole time series summary, indexed wind tows only summary, and the overlapped strata only summary data frames by year
# join the whole time series summary, indexed wind tows only summary, and the overlapped strata only summary data frames by species
# join the whole time series summary, indexed wind tows only summary, and the overlapped strata only summary data frames by year and species
# join the whole time series summary, indexed wind tows only summary, and the overlapped strata only summary data frames by season
# join the whole time series summary, indexed wind tows only summary, and the overlapped strata only summary data frames by species and season
# join the whole time series summary, indexed wind tows only summary, and the overlapped strata only summary data frames by time of day


### LOAD PACKAGES ###
library(stringr)
library(patchwork)
library(here)
library(tidyverse)

here()
init.analysis.dat <- here("data", "rds", "init-analysis")

### LOAD DATA ###
# indexed presence data created in `03b-index-wind-observations.R` here("tidy-data")
# data <- readRDS(here("data", "rds", "tidy-data", "full-bts-indexed.rds"))

# summarised data for the bottom trawl survey observations, created here("init-analysis", "01a-summarise-all-tows.R")
bts_sum <- readRDS(here(init.analysis.dat, "bts_summaries.rds"))
bts_sp <- readRDS(here(init.analysis.dat, "bts_species_summaries.rds"))
bts_yr <- readRDS(here(init.analysis.dat, "bts_yr_summary.rds"))
bts_time <- readRDS(here(init.analysis.dat, "bts_time_summary.rds"))
bts_strat <- readRDS(here(init.analysis.dat, "bts_strata_summary.rds"))
bts_ssn <- readRDS(here(init.analysis.dat, "bts_ssn_summary.rds"))
bts_sp_ssn_yr <- readRDS(here(init.analysis.dat, "bts_sp-ssn-yr_summary.rds"))

# bts_sp_strat <- readRDS(here(init.analysis.dat, "bts_sp-str_summary.rds"))
# 
bts_sp_yr <- readRDS(here(init.analysis.dat, "bts_sp-yr_summary.rds"))

# summarised data for observations occurring in potentially impacted strata by offshore wind energy areas, created here("init-analysis", "01c-summarise-overlap-tows.R")


overlap_sum <- readRDS(here(init.analysis.dat, "overlap_summaries.rds"))
overlap_sp <- readRDS(here(init.analysis.dat, "overlap_species_summaries.rds"))
# overlap_sp_strat <- readRDS(here(init.analysis.dat, "overlap_sp-str_summary.rds"))
overlap_sp_ssn_yr <- readRDS(here(init.analysis.dat, "overlap_sp_ssn_yr_summary.rds"))
overlap_sp_yr <- readRDS(here(init.analysis.dat, "overlap_sp-yr_summary.rds"))

overlap_yr <- readRDS(here(init.analysis.dat, "overlap_yr_summary.rds"))
overlap_time <- readRDS(here(init.analysis.dat, "overlap_time_summary.rds"))
overlap_strat <- readRDS(here(init.analysis.dat, "overlap_strata_summary.rds"))
overlap_ssn <- readRDS(here(init.analysis.dat, "overlap_ssn_summary.rds"))

# summarised data for observations identified as potentially occurring in offshore wind energy areas, created here("init-analysis", "01b-summarise-wind-tows.R")
wind_sum <- readRDS(here(init.analysis.dat, "wind_summaries.rds"))
 
# wind_sp_strat <- readRDS(here(init.analysis.dat, "wind_sp-str_summary.rds"))
# 
wind_sp_yr <- readRDS(here(init.analysis.dat, "wind_sp-yr_summary.rds"))
wind_sp <- readRDS(here(init.analysis.dat, "wind_species_summaries.rds"))
wind_yr <- readRDS(here(init.analysis.dat, "wind_yr_summary.rds"))
wind_time <- readRDS(here(init.analysis.dat, "wind_time_summary.rds"))
wind_strat <- readRDS(here(init.analysis.dat, "wind_strata_summary.rds"))
wind_ssn <- readRDS(here(init.analysis.dat, "wind_ssn_summary.rds"))
wind_sp_ssn_yr <- readRDS(here(init.analysis.dat, "wind_sp_ssn_yr_summary.rds"))

# species dataframe created here("tidy-data", "04-complete-datasets.R")
species <- readRDS(here("data", "rds", "species.rds"))

impacted_strata <- unique(wind_sum$STRATUM)

## COMPARE #####

### Total Tows ####
### Sums ###
# join full bts summary with summaries of overlapped strata only 
compare_sum <- full_join(bts_sum, overlap_sum, by = c("SVSPP", "SEASON", "DAYTIME", "TOWID", "EST_YEAR", "STRATUM", "COMNAME")) |>
  select(!c(TYPE.x, TYPE.y)) |>
  group_by(SVSPP, EST_YEAR, COMNAME, STRATUM, SEASON, DAYTIME, TOWID) |>
  summarise(BTS_CATCH = BTS_CATCH, 
            OVERLAP_CATCH = sum(ifelse(is.na(OVERLAP_CATCH), 0, OVERLAP_CATCH)), 
            BTS_BIO = BTS_BIO, 
            OVERLAP_BIO = sum(ifelse(is.na(OVERLAP_BIO), 0, OVERLAP_BIO)), 
            BTS_TOW = BTS_TOW, 
            OVERLAP_TOW = sum(ifelse(is.na(OVERLAP_TOW), 0, OVERLAP_TOW))) #|>
 
# join compare sum with summaries of wind observations only 
compare_sum <- full_join(compare_sum, wind_sum, by = c("SVSPP", "SEASON", "DAYTIME", "TOWID", "EST_YEAR", "STRATUM", "COMNAME")) |>
  group_by(SVSPP, EST_YEAR, COMNAME, STRATUM, SEASON, DAYTIME, TOWID) |>
  summarise(BTS_CATCH = BTS_CATCH, 
            OVERLAP_CATCH = OVERLAP_CATCH,
            WIND_CATCH = sum(ifelse(is.na(WIND_CATCH), 0, WIND_CATCH)), 
            BTS_BIO = BTS_BIO, 
            OVERLAP_BIO = OVERLAP_BIO, 
            WIND_BIO = sum(ifelse(is.na(WIND_BIO), 0, WIND_BIO)), 
            BTS_TOW = BTS_TOW, 
            OVERLAP_TOW = OVERLAP_TOW,
            WIND_TOW = sum(ifelse(is.na(WIND_TOW), 0, WIND_TOW))) #|>
  

# create "TOTAL" dataset
# GROUP <- "TOTAL TOWS"
# BTS_CATCH <- sum(data$EXPCATCHNUM)
# OVERLAP_CATCH <- sum(overlap$EXPCATCHNUM)
# WIND_CATCH <- sum(wind$EXPCATCHNUM)
# BTS_TOW <- length(unique(data$CODE))
# OVERLAP_TOW <- length(unique(data$CODE))
# WIND_TOW <- length(unique(data$CODE))
# CLASS <- "all tows"
# total_tows <- data.frame(GROUP = "TOTAL TOWS", 
#                          BTS_CATCH = sum(data$EXPCATCHNUM),
#                          OVERLAP_CATCH = sum(overlap$EXPCATCHNUM), 
#                          WIND_CATCH = sum(wind$EXPCATCHNUM),
#                          BTS_BIO = sum(data$EXPCATCHWT),
#                          OVERLAP_BIO = sum(overlap$EXPCATCHWT), 
#                          WIND_BIO = sum(wind$EXPCATCHWT),
#                          BTS_TOW = length(unique(data$CODE)), 
#                          OVERLAP_TOW = length(unique(overlap$CODE)), 
#                          WIND_TOW = length(unique(wind$CODE)), 
#                          CLASS = "ALL TOWS")
# 
# saveRDS(total_tows, here(init.analysis.dat, "total_tows.rds"))

### By Strata #####
### sum total catch, bio, and tows for impacted strata ###
# compare_strat <- compare_sum |> 
#   filter(STRATUM %in% impacted_strata) |>
#   group_by(STRATUM) |>
#   summarise(BTS_CATCH = sum(BTS_CATCH), 
#             OVERLAP_CATCH = sum(OVERLAP_CATCH),
#             WIND_CATCH = sum(WIND_CATCH),
#             BTS_BIO = sum(BTS_BIO), 
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             WIND_BIO = sum(WIND_BIO),
#             BTS_TOW = sum(BTS_TOW), 
#             OVERLAP_TOW = sum(OVERLAP_TOW),
#             WIND_TOW = sum(WIND_TOW)) |> 
#   arrange(desc(BTS_CATCH))

compare_strat <- full_join(bts_strat, overlap_strat, by = "STRATUM") |> 
  select(!c(TYPE.x, TYPE.y)) |> #, COMNAME.y)) |> 
  mutate(OVERLAP_CATCH = ifelse(is.na(OVERLAP_CATCH), 0, OVERLAP_CATCH),
         OVERLAP_BIO = ifelse(is.na(OVERLAP_BIO), 0, OVERLAP_BIO),
         OVERLAP_TOW = ifelse(is.na(OVERLAP_TOW), 0, OVERLAP_TOW)) #|> 
#rename(COMNAME = COMNAME.x)

compare_strat <- full_join(compare_strat, wind_strat, by =  "STRATUM") |> 
  mutate(WIND_CATCH = ifelse(is.na(WIND_CATCH), 0, WIND_CATCH),
         WIND_BIO = ifelse(is.na(WIND_BIO), 0, WIND_BIO),
         WIND_TOW = ifelse(is.na(WIND_TOW), 0, WIND_TOW)) |>
  select(!TYPE) #|>
# rename(COMNAME = COMNAME.x)

saveRDS(compare_strat, here(init.analysis.dat, "compare_strat.rds"))


### By Species #####
### sum total catch and tow for each species and strata ###
# comp_sp_strat <- compare_sum |>
#   group_by(SVSPP, COMNAME, STRATUM) |>
#   summarise(BTS_CATCH = sum(BTS_CATCH),
#             OVERLAP_CATCH = sum(OVERLAP_CATCH),
#             WIND_CATCH = sum(WIND_CATCH),
#             BTS_BIO = sum(BTS_BIO),
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             WIND_BIO = sum(WIND_BIO),
#             BTS_TOW = sum(BTS_TOW),
#             OVERLAP_TOW = sum(OVERLAP_TOW),
#             WIND_TOW = sum(WIND_TOW)) |>
#   arrange(desc(BTS_CATCH))
#comp_sp_strat <- inner_join(comp_sp_strat, species2, by = "SVSPP") #comp_sp_strat <- comp_sp_strat[, c(1, 9, 10, 2, 3, 4, 5, 6, 7, 8)]

### count number of strata; sum tow and catch for each species ###
# comp_nstrat <- full_join(bts_sp_strat, wind_sp_strat, by = "CODE") |>
#   group_by(SVSPP.x) |> 
#   rename(SVSPP = SVSPP.x) |>
#   summarise(BTS_NSTRAT = length(STRATUM.x),                       # count number of strata each species occurs
#             WIND_NSTRAT = length(STRATUM.y[!is.na(STRATUM.y)]),   # count number of strata each species occurs
#             BTS_CATCH = sum(BTS_CATCH), 
#             WIND_CATCH = sum(ifelse(is.na(WIND_CATCH), 0, WIND_CATCH)), 
#             BTS_BIO = BTS_BIO, 
#             WIND_BIO = sum(ifelse(is.na(WIND_BIO), 0, WIND_BIO)),
#             BTS_TOW = sum(BTS_TOW), 
#             WIND_TOW = sum(ifelse(is.na(WIND_TOW), 0, WIND_TOW))) 
# 
# 
# comp_over_nstrat <- overlap_sp_strat |> 
#   group_by(SVSPP, COMNAME) |> 
#   summarise(OVERLAP_NSTRAT = length(STRATUM),            # count number of strata each species occurs
#             OVERLAP_CATCH = sum(OVERLAP_CATCH), 
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             OVERLAP_TOW = sum(OVERLAP_TOW))
# comp_nstrat <- inner_join(comp_nstrat, comp_over_nstrat, by = "SVSPP")
#comp_nstrat <- inner_join(comp_nstrat, species2, by = "SVSPP") 
#comp_nstrat <- comp_nstrat[, c(1, 10, 2, 11, 3, 4, 12, 5, 6, 13, 7, 8, 14, 9)] #|> 
#  arrange(desc(BTS_CATCH))

### compare total catch and tow count in each area###
# compare_species <-compare_sum |> 
#   group_by(SVSPP, COMNAME) |> 
#   summarise(BTS_CATCH = sum(BTS_CATCH), 
#             OVERLAP_CATCH = sum(OVERLAP_CATCH),
#             WIND_CATCH = sum(WIND_CATCH), 
#             BTS_BIO = sum(BTS_BIO), 
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             WIND_BIO = sum(WIND_BIO),
#             BTS_TOW = sum(BTS_TOW), 
#             OVERLAP_TOW = sum(OVERLAP_TOW),
#             WIND_TOW = sum(WIND_TOW)) |> 
#   arrange(SVSPP)

compare_species <- full_join(bts_sp, overlap_sp, by = c("SVSPP", "COMNAME")) |> 
  select(!c(TYPE.x, TYPE.y)) |> 
  mutate(OVERLAP_CATCH = ifelse(is.na(OVERLAP_CATCH), 0, OVERLAP_CATCH),
         OVERLAP_BIO = ifelse(is.na(OVERLAP_BIO), 0, OVERLAP_BIO),
         OVERLAP_TOW = ifelse(is.na(OVERLAP_TOW), 0, OVERLAP_TOW))

compare_species <- full_join(compare_species, wind_sp, by = c("SVSPP", "COMNAME")) |> 
  mutate(WIND_CATCH = ifelse(is.na(WIND_CATCH), 0, WIND_CATCH),
         WIND_BIO = ifelse(is.na(WIND_BIO), 0, WIND_BIO),
         WIND_TOW = ifelse(is.na(WIND_TOW), 0, WIND_TOW)) |>
  select(!TYPE)

saveRDS(compare_species, here(init.analysis.dat, "compare_species.rds"))


### By Season #####
### compare total catch and tow count between species ###
comp_sp_ssn <- compare_sum |> 
  group_by(SVSPP, COMNAME, SEASON) |> 
  summarise(BTS_CATCH = sum(BTS_CATCH), 
            OVERLAP_CATCH = sum(OVERLAP_CATCH),
            WIND_CATCH = sum(WIND_CATCH), 
            BTS_BIO = sum(BTS_BIO), 
            OVERLAP_BIO = sum(OVERLAP_BIO),
            WIND_BIO = sum(WIND_BIO), 
            BTS_TOW = sum(BTS_TOW), 
            OVERLAP_TOW = sum(OVERLAP_TOW),
            WIND_TOW = sum(WIND_TOW)) |> 
  mutate(CODE = str_c(SVSPP, SEASON)) |>
  arrange(SVSPP)

saveRDS(comp_sp_ssn, here(init.analysis.dat, "compare_species-season.rds"))

#comp_sp_ssn <- full_join(comp_sp_ssn, species2, by = "SVSPP") 
#comp_sp_ssn <- comp_sp_ssn[, c(1, 9, 10, 2, 3:8)] 
#comp_sp_ssn$CODE <- paste(comp_sp_ssn$SVSPP, comp_sp_ssn$SEASON)


### compare total catch and tow count ###
# compare_season <- comp_sp_ssn |>
#   group_by(SEASON) |>
#   summarise(BTS_CATCH = sum(BTS_CATCH), 
#             OVERLAP_CATCH = sum(OVERLAP_CATCH), 
#             WIND_CATCH = sum(WIND_CATCH), 
#             BTS_BIO = sum(BTS_BIO), 
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             WIND_BIO = sum(WIND_BIO), 
#             BTS_TOW = sum(BTS_TOW), 
#             OVERLAP_TOW = sum(OVERLAP_TOW), 
#             WIND_TOW = sum(WIND_TOW)) |> 
#   rename(GROUP = SEASON)

compare_season <- full_join(bts_ssn, overlap_ssn, by = "SEASON") |> 
  select(!c(TYPE.x, TYPE.y)) |> 
  mutate(OVERLAP_CATCH = ifelse(is.na(OVERLAP_CATCH), 0, OVERLAP_CATCH),
         OVERLAP_BIO = ifelse(is.na(OVERLAP_BIO), 0, OVERLAP_BIO),
         OVERLAP_TOW = ifelse(is.na(OVERLAP_TOW), 0, OVERLAP_TOW))

compare_season <- full_join(compare_season, wind_ssn, by = "SEASON") |> 
  mutate(WIND_CATCH = ifelse(is.na(WIND_CATCH), 0, WIND_CATCH),
         WIND_BIO = ifelse(is.na(WIND_BIO), 0, WIND_BIO),
         WIND_TOW = ifelse(is.na(WIND_TOW), 0, WIND_TOW)) |>
  select(!TYPE)

saveRDS(compare_season, here(init.analysis.dat, "compare_season.rds"))


### By Year #####
### compare total catch and tow count between species by year ###
# comp_sp_yr <- compare_sum |> 
#   group_by(SVSPP, COMNAME, YEAR) |> 
#   summarise(BTS_CATCH = sum(BTS_CATCH), 
#             OVERLAP_CATCH = sum(OVERLAP_CATCH),
#             WIND_CATCH = sum(WIND_CATCH), 
#             BTS_BIO = sum(BTS_BIO), 
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             WIND_BIO = sum(WIND_BIO), 
#             BTS_TOW = sum(BTS_TOW), 
#             OVERLAP_TOW = sum(OVERLAP_TOW),
#             WIND_TOW = sum(WIND_TOW)) |> 
#   mutate(CODE = str_c(SVSPP, YEAR)) |>
#   arrange(SVSPP)

compare_year <- full_join(bts_yr, overlap_yr, by = "EST_YEAR") |> 
  select(!c(TYPE.x, TYPE.y)) |> 
  mutate(OVERLAP_CATCH = ifelse(is.na(OVERLAP_CATCH), 0, OVERLAP_CATCH),
         OVERLAP_BIO = ifelse(is.na(OVERLAP_BIO), 0, OVERLAP_BIO),
         OVERLAP_TOW = ifelse(is.na(OVERLAP_TOW), 0, OVERLAP_TOW))

compare_year <- full_join(compare_year, wind_yr, by = "EST_YEAR") |> 
  mutate(WIND_CATCH = ifelse(is.na(WIND_CATCH), 0, WIND_CATCH),
         WIND_BIO = ifelse(is.na(WIND_BIO), 0, WIND_BIO),
         WIND_TOW = ifelse(is.na(WIND_TOW), 0, WIND_TOW)) |>
  select(!TYPE)

saveRDS(compare_year, here(init.analysis.dat, "compare_year.rds"))

#### Compare species by year ####
compare_sp_yr <- full_join(bts_sp_yr, overlap_sp_yr, by = c("SVSPP", "EST_YEAR")) |> 
  select(!c(TYPE.x, TYPE.y, COMNAME.y)) |> 
  mutate(OVERLAP_CATCH = ifelse(is.na(OVERLAP_CATCH), 0, OVERLAP_CATCH),
         OVERLAP_BIO = ifelse(is.na(OVERLAP_BIO), 0, OVERLAP_BIO),
         OVERLAP_TOW = ifelse(is.na(OVERLAP_TOW), 0, OVERLAP_TOW)) |> 
  rename(COMNAME = COMNAME.x)

compare_sp_yr <- full_join(compare_sp_yr, wind_sp_yr, by = c("SVSPP", "EST_YEAR")) |> 
  mutate(WIND_CATCH = ifelse(is.na(WIND_CATCH), 0, WIND_CATCH),
         WIND_BIO = ifelse(is.na(WIND_BIO), 0, WIND_BIO),
         WIND_TOW = ifelse(is.na(WIND_TOW), 0, WIND_TOW)) |> 
  select(!c(COMNAME.y, TYPE)) |> 
  rename(COMNAME = COMNAME.x)

saveRDS(compare_sp_yr, here(init.analysis.dat, "compare_species-year.rds"))

#### Compare species by year and season ####
compare_sp_yr_ssn <- full_join(bts_sp_ssn_yr, overlap_sp_ssn_yr, by = c("SVSPP", "EST_YEAR", "SEASON")) |> 
  select(!c(TYPE.x, TYPE.y, COMNAME.y)) |> 
  mutate(OVERLAP_CATCH = ifelse(is.na(OVERLAP_CATCH), 0, OVERLAP_CATCH),
         OVERLAP_BIO = ifelse(is.na(OVERLAP_BIO), 0, OVERLAP_BIO),
         OVERLAP_TOW = ifelse(is.na(OVERLAP_TOW), 0, OVERLAP_TOW)) |> 
  rename(COMNAME = COMNAME.x)

compare_sp_yr_ssn <- full_join(compare_sp_yr_ssn, wind_sp_ssn_yr, by = c("SVSPP", "EST_YEAR", "SEASON")) |> 
  mutate(WIND_CATCH = ifelse(is.na(WIND_CATCH), 0, WIND_CATCH),
         WIND_BIO = ifelse(is.na(WIND_BIO), 0, WIND_BIO),
         WIND_TOW = ifelse(is.na(WIND_TOW), 0, WIND_TOW)) |> 
  select(!c(COMNAME.y, TYPE)) |> 
  rename(COMNAME = COMNAME.x)

saveRDS(compare_sp_yr_ssn, here(init.analysis.dat, "compare_species-year-ssn.rds"))

# comp_sp_yr <- full_join(comp_sp_yr, species2, by = "SVSPP") 
# comp_sp_yr <- comp_sp_yr[, c(1, 9, 10, 2:8)] 
# comp_sp_yr$CODE <- paste(comp_sp_yr$SVSPP, comp_sp_yr$YEAR)



### compare total catch and tow count by year ###
# compare_year <- compare_sum |>
#   group_by(YEAR) |>
#   summarise(BTS_CATCH = sum(BTS_CATCH), 
#             OVERLAP_CATCH = sum(OVERLAP_CATCH), 
#             WIND_CATCH = sum(WIND_CATCH), 
#             BTS_BIO = sum(BTS_BIO), 
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             WIND_BIO = sum(WIND_BIO), 
#             BTS_TOW = sum(BTS_TOW), 
#             OVERLAP_TOW = sum(OVERLAP_TOW), 
#             WIND_TOW = sum(WIND_TOW)) |> 
#   rename(GROUP = YEAR) 


### By Time of Day #####
### compare total catch and tow count by time of day ###
# compare_time <- compare_sum |>
#   group_by(DAYTIME) |>
#   summarise(BTS_CATCH = sum(BTS_CATCH), 
#             OVERLAP_CATCH = sum(OVERLAP_CATCH), 
#             WIND_CATCH = sum(WIND_CATCH), 
#             BTS_BIO = sum(BTS_BIO), 
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             WIND_BIO = sum(WIND_BIO), 
#             BTS_TOW = sum(BTS_TOW), 
#             OVERLAP_TOW = sum(OVERLAP_TOW), 
#             WIND_TOW = sum(WIND_TOW)) |> 
#   rename(GROUP = DAYTIME) 

compare_time <- full_join(bts_time, overlap_time, by = "DAYTIME") |> 
  select(!c(TYPE.x, TYPE.y)) |> 
  mutate(OVERLAP_CATCH = ifelse(is.na(OVERLAP_CATCH), 0, OVERLAP_CATCH),
         OVERLAP_BIO = ifelse(is.na(OVERLAP_BIO), 0, OVERLAP_BIO),
         OVERLAP_TOW = ifelse(is.na(OVERLAP_TOW), 0, OVERLAP_TOW))

compare_time <- full_join(compare_time, wind_time, by = "DAYTIME") |> 
  mutate(WIND_CATCH = ifelse(is.na(WIND_CATCH), 0, WIND_CATCH),
         WIND_BIO = ifelse(is.na(WIND_BIO), 0, WIND_BIO),
         WIND_TOW = ifelse(is.na(WIND_TOW), 0, WIND_TOW)) |> 
  select(!TYPE)

saveRDS(compare_time, here(init.analysis.dat, "compare_time.rds"))

### compare total catch and tow count by time of day and species ###
# comp_sp_time <- compare_sum |>
#   group_by(SVSPP, COMNAME, DAYTIME) |>
#   summarise(BTS_CATCH = sum(BTS_CATCH), 
#             OVERLAP_CATCH = sum(OVERLAP_CATCH), 
#             WIND_CATCH = sum(WIND_CATCH), 
#             BTS_BIO = sum(BTS_BIO), 
#             OVERLAP_BIO = sum(OVERLAP_BIO),
#             WIND_BIO = sum(WIND_BIO), 
#             BTS_TOW = sum(BTS_TOW), 
#             OVERLAP_TOW = sum(OVERLAP_TOW), 
#             WIND_TOW = sum(WIND_TOW)) |> 
#   rename(GROUP = DAYTIME) |>
#   mutate(CODE = str_c(SVSPP, GROUP))
#comp_sp_time <- full_join(comp_sp_time, species2, by = "SVSPP") 
#comp_sp_time <- comp_sp_time[, c(1, 9, 10, 2:8)] 
#comp_sp_time$CODE <- paste(comp_sp_time$SVSPP, comp_sp_time$GROUP)


