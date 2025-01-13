### created:      02/15/2022
### last update:  11/11/2024
###

# 02b - CALCULATE PERCENT IMPACTS ####

## Objective ####
#  calculate the percent lost between the whole time series and the overlapped strata only when compared to the amount of tows and catch in wind areas only

# Script will calculate percent loss by: 
# year
# species
# season
# time of day 
# strata


### LOAD PACKAGES ###
library(stringr)
library(patchwork)
library(here)
library(tidyverse)

here()
init.analysis.dat <- here("data", "rds", "init-analysis")
summary.csv <- here("data", "csv", "summary-stats")

### LOAD DATA ###
# comparison data created here("init-analysis", "workshop-02_2022", "02a-compare-tows")
# total_tows <- readRDS(here(init.analysis.dat, "total_tows.rds"))

compare_strat <- readRDS(here(init.analysis.dat, "compare_strat.rds"))

compare_species <- readRDS(here(init.analysis.dat, "compare_species.rds"))

compare_season <- readRDS(here(init.analysis.dat, "compare_season.rds"))

compare_year <- readRDS(here(init.analysis.dat, "compare_year.rds"))

compare_time <- readRDS(here(init.analysis.dat, "compare_time.rds"))

compare_sp_yr <- readRDS(here(init.analysis.dat, "compare_species-year.rds"))

compare_sp_yr_ssn <- readRDS(here(init.analysis.dat, "compare_species-year-ssn.rds"))

# PERCENT SUMMARIES ####

## BY SPECIES ######
compare_species <- compare_species |>
  mutate(OVERLAP_CATCHPCT = round(((OVERLAP_CATCH/BTS_CATCH)*100), digits = 0), 
         WIND_CATCHPCT = round(((WIND_CATCH/BTS_CATCH)*100), digits = 0), 
         OVERLAP_BIOPCT = round(((OVERLAP_BIO/BTS_BIO)*100), digits = 0), 
         WIND_BIOPCT = round(((WIND_BIO/BTS_BIO)*100), digits = 0), 
         OVERLAP_TOWPCT = round(((OVERLAP_TOW/BTS_TOW)*100), digits = 0), 
         WIND_TOWPCT = round(((WIND_TOW/BTS_TOW)*100), digits = 0), 
         WIND_OVERLAP_CATCH = round(((WIND_CATCH/OVERLAP_CATCH)*100), digits = 0), 
         WIND_OVERLAP_BIO = round(((WIND_BIO/OVERLAP_BIO)*100), digits = 0), 
         WIND_OVERLAP_TOW = round(((WIND_TOW/OVERLAP_TOW)*100), digits = 0), 
         CLASS = "Species",#)
         
         #compare_species <- compare_species |> 
         # mutate(
         WIND_OVERLAP_CATCH = ifelse(is.nan(WIND_OVERLAP_CATCH), 0, WIND_OVERLAP_CATCH),
         WIND_OVERLAP_BIO = ifelse(is.nan(WIND_OVERLAP_BIO), 0, WIND_OVERLAP_BIO),
         WIND_OVERLAP_TOW = ifelse(is.nan(WIND_OVERLAP_TOW), 0, WIND_OVERLAP_TOW))



#write csv
write.csv(compare_species, here(summary.csv, "species-summaries.csv"), row.names=FALSE)
saveRDS(compare_species, here(init.analysis.dat, "species-impacts.rds"))

## BY SEASON ####
compare_season <- compare_season |>
  mutate(OVERLAP_CATCHPCT = round(((OVERLAP_CATCH/BTS_CATCH)*100), digits = 0), 
         WIND_CATCHPCT = round(((WIND_CATCH/BTS_CATCH)*100), digits = 0), 
         OVERLAP_BIOPCT = round(((OVERLAP_BIO/BTS_BIO)*100), digits = 0), 
         WIND_BIOPCT = round(((WIND_BIO/BTS_BIO)*100), digits = 0), 
         OVERLAP_TOWPCT = round(((OVERLAP_TOW/BTS_TOW)*100), digits = 0), 
         WIND_TOWPCT = round(((WIND_TOW/BTS_TOW)*100), digits = 0), 
         WIND_OVERLAP_CATCH = round(((WIND_CATCH/OVERLAP_CATCH)*100), digits = 0), 
         WIND_OVERLAP_BIO = round(((WIND_BIO/OVERLAP_BIO)*100), digits = 0), 
         WIND_OVERLAP_TOW = round(((WIND_TOW/OVERLAP_TOW)*100), digits = 0), 
         CLASS = "Season",#)
         
         #compare_season <- compare_season |> 
         #mutate(
         WIND_OVERLAP_CATCH = ifelse(is.nan(WIND_OVERLAP_CATCH), 0, WIND_OVERLAP_CATCH),
         WIND_OVERLAP_BIO = ifelse(is.nan(WIND_OVERLAP_BIO), 0, WIND_OVERLAP_BIO),
         WIND_OVERLAP_TOW = ifelse(is.nan(WIND_OVERLAP_TOW), 0, WIND_OVERLAP_TOW))

#write CSV
write.csv(compare_season, here(summary.csv, "seasonal-summaries.csv"), row.names=FALSE)
saveRDS(compare_season, here(init.analysis.dat, "season-impacts.rds"))

## BY YEAR ####
compare_year <- compare_year |>
  mutate(OVERLAP_CATCHPCT = round(((OVERLAP_CATCH/BTS_CATCH)*100), digits = 0), 
         WIND_CATCHPCT = round(((WIND_CATCH/BTS_CATCH)*100), digits = 0), 
         OVERLAP_BIOPCT = round(((OVERLAP_BIO/BTS_BIO)*100), digits = 0), 
         WIND_BIOPCT = round(((WIND_BIO/BTS_BIO)*100), digits = 0), 
         OVERLAP_TOWPCT = round(((OVERLAP_TOW/BTS_TOW)*100), digits = 0), 
         WIND_TOWPCT = round(((WIND_TOW/BTS_TOW)*100), digits = 0), 
         WIND_OVERLAP_CATCH = round(((WIND_CATCH/OVERLAP_CATCH)*100), digits = 0), 
         WIND_OVERLAP_BIO = round(((WIND_BIO/OVERLAP_BIO)*100), digits = 0), 
         WIND_OVERLAP_TOW = round(((WIND_TOW/OVERLAP_TOW)*100), digits = 0), 
         CLASS = "Year",#)
         
         # compare_year <- compare_year |> 
         # mutate(
         WIND_OVERLAP_CATCH = ifelse(is.nan(WIND_OVERLAP_CATCH), 0, WIND_OVERLAP_CATCH),
         WIND_OVERLAP_BIO = ifelse(is.nan(WIND_OVERLAP_BIO), 0, WIND_OVERLAP_BIO),
         WIND_OVERLAP_TOW = ifelse(is.nan(WIND_OVERLAP_TOW), 0, WIND_OVERLAP_TOW))

#write CSV
write.csv(compare_year, here(summary.csv, "yearly-summaries.csv"), row.names=FALSE)
saveRDS(compare_year, here(init.analysis.dat, "year-impacts.rds"))


#### BY SPECIES AND YEAR ####
compare_sp_yr <- compare_sp_yr |>
  mutate(OVERLAP_CATCHPCT = round(((OVERLAP_CATCH/BTS_CATCH)*100), digits = 0), 
         WIND_CATCHPCT = round(((WIND_CATCH/BTS_CATCH)*100), digits = 0), 
         OVERLAP_BIOPCT = round(((OVERLAP_BIO/BTS_BIO)*100), digits = 0), 
         WIND_BIOPCT = round(((WIND_BIO/BTS_BIO)*100), digits = 0), 
         OVERLAP_TOWPCT = round(((OVERLAP_TOW/BTS_TOW)*100), digits = 0), 
         WIND_TOWPCT = round(((WIND_TOW/BTS_TOW)*100), digits = 0), 
         WIND_OVERLAP_CATCH = round(((WIND_CATCH/OVERLAP_CATCH)*100), digits = 0), 
         WIND_OVERLAP_BIO = round(((WIND_BIO/OVERLAP_BIO)*100), digits = 0), 
         WIND_OVERLAP_TOW = round(((WIND_TOW/OVERLAP_TOW)*100), digits = 0),#)
         
         
         # compare_sp_yr <- compare_sp_yr |> 
         # mutate(
         OVERLAP_CATCHPCT = ifelse(is.nan(OVERLAP_CATCHPCT), 0, OVERLAP_CATCHPCT),
         WIND_CATCHPCT = ifelse(is.nan(WIND_CATCHPCT), 0, WIND_CATCHPCT),
         OVERLAP_BIOPCT = ifelse(is.nan(OVERLAP_BIOPCT), 0, OVERLAP_BIOPCT),
         WIND_BIOPCT = ifelse(is.nan(WIND_BIOPCT), 0, WIND_BIOPCT),
         WIND_OVERLAP_CATCH = ifelse(is.nan(WIND_OVERLAP_CATCH), 0, WIND_OVERLAP_CATCH),
         WIND_OVERLAP_BIO = ifelse(is.nan(WIND_OVERLAP_BIO), 0, WIND_OVERLAP_BIO),
         WIND_OVERLAP_TOW = ifelse(is.nan(WIND_OVERLAP_TOW), 0, WIND_OVERLAP_TOW))

write.csv(compare_sp_yr, here(summary.csv, "species-yearly-summaries.csv"), row.names=FALSE)
saveRDS(compare_sp_yr, here(init.analysis.dat, "species-year-impacts.rds"))


#### BY SPECIES, YEAR, AND SEASON ####
compare_sp_yr_ssn <- compare_sp_yr_ssn |>
  mutate(OVERLAP_CATCHPCT = round(((OVERLAP_CATCH/BTS_CATCH)*100), digits = 0), 
         WIND_CATCHPCT = round(((WIND_CATCH/BTS_CATCH)*100), digits = 0), 
         OVERLAP_BIOPCT = round(((OVERLAP_BIO/BTS_BIO)*100), digits = 0), 
         WIND_BIOPCT = round(((WIND_BIO/BTS_BIO)*100), digits = 0), 
         OVERLAP_TOWPCT = round(((OVERLAP_TOW/BTS_TOW)*100), digits = 0), 
         WIND_TOWPCT = round(((WIND_TOW/BTS_TOW)*100), digits = 0), 
         WIND_OVERLAP_CATCH = round(((WIND_CATCH/OVERLAP_CATCH)*100), digits = 0), 
         WIND_OVERLAP_BIO = round(((WIND_BIO/OVERLAP_BIO)*100), digits = 0), 
         WIND_OVERLAP_TOW = round(((WIND_TOW/OVERLAP_TOW)*100), digits = 0),      OVERLAP_CATCHPCT = ifelse(is.nan(OVERLAP_CATCHPCT), 0, OVERLAP_CATCHPCT),
         WIND_CATCHPCT = ifelse(is.nan(WIND_CATCHPCT), 0, WIND_CATCHPCT),
         OVERLAP_BIOPCT = ifelse(is.nan(OVERLAP_BIOPCT), 0, OVERLAP_BIOPCT),
         WIND_BIOPCT = ifelse(is.nan(WIND_BIOPCT), 0, WIND_BIOPCT),
         WIND_OVERLAP_CATCH = ifelse(is.nan(WIND_OVERLAP_CATCH), 0, WIND_OVERLAP_CATCH),
         WIND_OVERLAP_BIO = ifelse(is.nan(WIND_OVERLAP_BIO), 0, WIND_OVERLAP_BIO),
         WIND_OVERLAP_TOW = ifelse(is.nan(WIND_OVERLAP_TOW), 0, WIND_OVERLAP_TOW))

write.csv(compare_sp_yr_ssn, here(summary.csv, "annual-seasonal-species-summaries.csv"), row.names=FALSE)
saveRDS(compare_sp_yr_ssn, here(init.analysis.dat, "species-season-year-impacts.rds"))



## BY TIME OF DAY ####
compare_time <- compare_time |>
  mutate(OVERLAP_CATCHPCT = round(((OVERLAP_CATCH/BTS_CATCH)*100), digits = 0), 
         WIND_CATCHPCT = round(((WIND_CATCH/BTS_CATCH)*100), digits = 0), 
         OVERLAP_BIOPCT = round(((OVERLAP_BIO/BTS_BIO)*100), digits = 0), 
         WIND_BIOPCT = round(((WIND_BIO/BTS_BIO)*100), digits = 0), 
         OVERLAP_TOWPCT = round(((OVERLAP_TOW/BTS_TOW)*100), digits = 0), 
         WIND_TOWPCT = round(((WIND_TOW/BTS_TOW)*100), digits = 0), 
         WIND_OVERLAP_CATCH = round(((WIND_CATCH/OVERLAP_CATCH)*100), digits = 0), 
         WIND_OVERLAP_BIO = round(((WIND_BIO/OVERLAP_BIO)*100), digits = 0), 
         WIND_OVERLAP_TOW = round(((WIND_TOW/OVERLAP_TOW)*100), digits = 0), 
         CLASS = "Time of Day", #)

# compare_time <- compare_time |> 
  # mutate(
    WIND_OVERLAP_CATCH = ifelse(is.nan(WIND_OVERLAP_CATCH), 0, WIND_OVERLAP_CATCH),
         WIND_OVERLAP_BIO = ifelse(is.nan(WIND_OVERLAP_BIO), 0, WIND_OVERLAP_BIO),
         WIND_OVERLAP_TOW = ifelse(is.nan(WIND_OVERLAP_TOW), 0, WIND_OVERLAP_TOW))

#write CSV
write.csv(compare_time, here(summary.csv, "time-of-day-summaries.csv"), row.names=FALSE)
saveRDS(compare_time, here(init.analysis.dat, "time-of-day-impacts.rds"))

## BY STRATUM #####
compare_strat <- compare_strat |>
  mutate(OVERLAP_CATCHPCT = round(((OVERLAP_CATCH/BTS_CATCH)*100), digits = 0), 
         WIND_CATCHPCT = round(((WIND_CATCH/BTS_CATCH)*100), digits = 0), 
         OVERLAP_BIOPCT = round(((OVERLAP_BIO/BTS_BIO)*100), digits = 0), 
         WIND_BIOPCT = round(((WIND_BIO/BTS_BIO)*100), digits = 0), 
         OVERLAP_TOWPCT = round(((OVERLAP_TOW/BTS_TOW)*100), digits = 0), 
         WIND_TOWPCT = round(((WIND_TOW/BTS_TOW)*100), digits = 0), 
         WIND_OVERLAP_CATCH = round(((WIND_CATCH/OVERLAP_CATCH)*100), digits = 0), 
         WIND_OVERLAP_BIO = round(((WIND_BIO/OVERLAP_BIO)*100), digits = 0), 
         WIND_OVERLAP_TOW = round(((WIND_TOW/OVERLAP_TOW)*100), digits = 0), 
         CLASS = "Strata", #)

# compare_strat <- compare_strat |> 
  # mutate(
    WIND_OVERLAP_CATCH = ifelse(is.nan(WIND_OVERLAP_CATCH), 0, WIND_OVERLAP_CATCH),
         WIND_OVERLAP_BIO = ifelse(is.nan(WIND_OVERLAP_BIO), 0, WIND_OVERLAP_BIO),
         WIND_OVERLAP_TOW = ifelse(is.nan(WIND_OVERLAP_TOW), 0, WIND_OVERLAP_TOW))

#write CSV
write.csv(compare_strat, here(summary.csv, "strata-summaries.csv"), row.names=FALSE)
saveRDS(compare_strat, here(init.analysis.dat, "strata-impacts.rds"))

# #write csv
# write.csv(comp_sp_time, here("data", "summary-stats", "day-species-percents.csv"), row.names=FALSE)
# 
# ## By Year ###
# compare_year$OVERLAP_CATCHPCT <- round((compare_year$OVERLAP_CATCH * 100)/compare_year$BTS_CATCH, digits = 0)
# compare_year$WIND_CATCHPCT <- round((compare_year$WIND_CATCH * 100)/compare_year$BTS_CATCH, digits = 0)
# compare_year$OVERLAP_TOWPCT <- round((compare_year$OVERLAP_TOW * 100)/compare_year$BTS_TOW, digits = 0)
# compare_year$WIND_TOWPCT <- round((compare_year$WIND_TOW * 100)/compare_year$BTS_TOW, digits = 0)
# compare_year$WIND_OVERLAP_CATCH <- round((compare_year$WIND_CATCH * 100)/compare_year$OVERLAP_CATCH, digits = 0) 
# compare_year$WIND_OVERLAP_TOW <- round((compare_year$WIND_TOW * 100)/compare_year$OVERLAP_TOW, digits = 0)
# compare_year$CLASS <- paste("year")
# 
# #write CSV
# write.csv(compare_year, here("data", "summary-stats", "yearly-summaries.csv"), row.names=FALSE)
# 
# ## By Time of Day ###
# compare_time$OVERLAP_CATCHPCT <- round((compare_time$OVERLAP_CATCH * 100)/compare_time$BTS_CATCH, digits = 0)
# compare_time$WIND_CATCHPCT <- round((compare_time$WIND_CATCH * 100)/compare_time$BTS_CATCH, digits = 0)
# compare_time$OVERLAP_TOWPCT <- round((compare_time$OVERLAP_TOW * 100)/compare_time$BTS_TOW, digits = 0)
# compare_time$WIND_TOWPCT <- round((compare_time$WIND_TOW * 100)/compare_time$BTS_TOW, digits = 0)
# compare_time$WIND_OVERLAP_CATCH <- round((compare_time$WIND_CATCH * 100)/compare_time$OVERLAP_CATCH, digits = 0) 
# compare_time$WIND_OVERLAP_TOW <- round((compare_time$WIND_TOW * 100)/compare_time$OVERLAP_TOW, digits = 0)
# compare_time$GROUP <- toupper(compare_time$GROUP)
# compare_time$CLASS <- paste("time of day")
# 
# #write CSV
# write.csv(compare_time, here("data", "summary-stats", "daytime-summaries.csv"), row.names=FALSE)
# 
# 
# ## By Season ###
# compare_season$OVERLAP_CATCHPCT <- round((compare_season$OVERLAP_CATCH * 100)/compare_season$BTS_CATCH, digits = 0)
# compare_season$WIND_CATCHPCT <- round((compare_season$WIND_CATCH * 100)/compare_season$BTS_CATCH, digits = 0)
# compare_season$OVERLAP_TOWPCT <- round((compare_season$OVERLAP_TOW * 100)/compare_season$BTS_TOW, digits = 0)
# compare_season$WIND_TOWPCT <- round((compare_season$WIND_TOW * 100)/compare_season$BTS_TOW, digits = 0)
# compare_season$WIND_OVERLAP_CATCH <- round((compare_season$WIND_CATCH * 100)/compare_season$OVERLAP_CATCH, digits = 0) 
# compare_season$WIND_OVERLAP_TOW <- round((compare_season$WIND_TOW * 100)/compare_season$OVERLAP_TOW, digits = 0)
# compare_season$CLASS <- paste("season")
# 
# #write CSV
# write.csv(compare_season, here("data", "summary-stats", "season-summary.csv"), row.names=FALSE)

## By Strata
# compare_strat$OVERLAP_CATCHPCT <- round((compare_strat$OVERLAP_CATCH * 100)/compare_strat$BTS_CATCH, digits = 0)
# compare_strat$WIND_CATCHPCT <- round((compare_strat$WIND_CATCH * 100)/compare_strat$BTS_CATCH, digits = 0)
# compare_strat$OVERLAP_TOWPCT <- round((compare_strat$OVERLAP_TOW * 100)/compare_strat$BTS_TOW, digits = 0)
# compare_strat$WIND_TOWPCT <- round((compare_strat$WIND_TOW * 100)/compare_strat$BTS_TOW, digits = 0)
# compare_strat$WIND_OVERLAP_CATCH <- round(ifelse((compare_strat$OVERLAP_CATCH == 0), 0, ((compare_strat$WIND_CATCH * 100)/compare_strat$OVERLAP_CATCH)), digits = 0) 
# compare_strat$WIND_OVERLAP_TOW <- round(ifelse((compare_strat$OVERLAP_TOW == 0), 0, ((compare_strat$WIND_TOW * 100)/compare_strat$OVERLAP_TOW)), digits = 0)
# compare_strat$CLASS <- paste("stratum")
# compare_strat <- rename(compare_strat, GROUP = STRATUM)
# 
# #write CSV
# write.csv(compare_strat, here("data", "summary-stats", "strata-summary.csv"), row.names=FALSE)

# # species and stratum
# comp_sp_strat$OVERLAP_CATCHPCT <- round((comp_sp_strat$OVERLAP_CATCH * 100)/comp_sp_strat$BTS_CATCH, digits = 0)
# comp_sp_strat$WIND_CATCHPCT <- round((comp_sp_strat$WIND_CATCH * 100)/comp_sp_strat$BTS_CATCH, digits = 0)
# comp_sp_strat$OVERLAP_TOWPCT <- round((comp_sp_strat$OVERLAP_TOW * 100)/comp_sp_strat$BTS_TOW, digits = 0)
# comp_sp_strat$WIND_TOWPCT <- round((comp_sp_strat$WIND_TOW * 100)/comp_sp_strat$BTS_TOW, digits = 0)
# comp_sp_strat$WIND_OVERLAP_CATCH <- round(ifelse((comp_sp_strat$OVERLAP_CATCH == 0), 0, ((comp_sp_strat$WIND_CATCH * 100)/comp_sp_strat$OVERLAP_CATCH)), digits = 0) 
# comp_sp_strat$WIND_OVERLAP_TOW <- round(ifelse((comp_sp_strat$OVERLAP_TOW == 0), 0, ((comp_sp_strat$WIND_TOW * 100)/comp_sp_strat$OVERLAP_TOW)), digits = 0)
# comp_sp_strat$CLASS <- paste("stratum")
# comp_sp_strat <- rename(comp_sp_strat, GROUP = STRATUM)


## Total Tows ###
# total_tows$OVERLAP_CATCHPCT <- round((total_tows$OVERLAP_CATCH * 100)/total_tows$BTS_CATCH, digits = 0)
# total_tows$WIND_CATCHPCT <- round((total_tows$WIND_CATCH * 100)/total_tows$BTS_CATCH, digits = 0)
# total_tows$OVERLAP_TOWPCT <- round((total_tows$OVERLAP_TOW * 100)/total_tows$BTS_TOW, digits = 0)
# total_tows$WIND_TOWPCT <- round((total_tows$WIND_TOW * 100)/total_tows$BTS_TOW, digits = 0)
# total_tows$WIND_OVERLAP_CATCH <- round((total_tows$WIND_CATCH * 100)/total_tows$OVERLAP_CATCH, digits = 0) 
# total_tows$WIND_OVERLAP_TOW <- round((total_tows$WIND_TOW * 100)/total_tows$OVERLAP_TOW, digits = 0)

# total_tows <- total_tows |>
#   mutate(OVERLAP_CATCHPCT = round(((OVERLAP_CATCH/BTS_CATCH)*100), digits = 0), 
#          WIND_CATCHPCT = round(((WIND_CATCH/BTS_CATCH)*100), digits = 0), 
#          OVERLAP_BIOPCT = round(((OVERLAP_BIO/BTS_BIO)*100), digits = 0), 
#          WIND_BIOPCT = round(((WIND_BIO/BTS_BIO)*100), digits = 0), 
#          OVERLAP_TOWPCT = round(((OVERLAP_TOW/BTS_TOW)*100), digits = 0), 
#          WIND_TOWPCT = round(((WIND_TOW/BTS_TOW)*100), digits = 0), 
#          WIND_OVERLAP_CATCH = round(((WIND_CATCH/OVERLAP_CATCH)*100), digits = 0), 
#          WIND_OVERLAP_BIO = round(((WIND_BIO/OVERLAP_BIO)*100), digits = 0), 
#          WIND_OVERLAP_TOW = round(((WIND_TOW/OVERLAP_TOW)*100), digits = 0), 
#          CLASS = "Strata")
# 
# total_tows <- total_tows |> 
#   mutate(WIND_OVERLAP_CATCH = ifelse(is.nan(WIND_OVERLAP_CATCH), 0, WIND_OVERLAP_CATCH),
#          WIND_OVERLAP_BIO = ifelse(is.nan(WIND_OVERLAP_BIO), 0, WIND_OVERLAP_BIO),
#          WIND_OVERLAP_TOW = ifelse(is.nan(WIND_OVERLAP_TOW), 0, WIND_OVERLAP_TOW))
# 
# #write CSV
# write.csv(total_tows, here("init-analysis", "outputs", "summary_stats", "total-summaries.csv"), row.names=FALSE)
# saveRDS(total_tows, here(init.analysis.dat, "total-impacts.rds"))

### Total Strata Count and Catch ###
# comp_nstrat$OVERLAP_NSTRAT_PCT <- round((comp_nstrat$OVERLAP_NSTRAT * 100)/comp_nstrat$BTS_NSTRAT, digits = 0)
# comp_nstrat$WIND_NSTRAT_PCT <- round((comp_nstrat$WIND_NSTRAT * 100)/comp_nstrat$BTS_NSTRAT, digits = 0)
# # comp_nstrat$OVERLAP_TOWPCT <- round((comp_nstrat$OVERLAP_TOW* 100)/comp_nstrat$BTS_TOW, digits = 0)
# # comp_nstrat$WIND_TOWPCT <- round((comp_nstrat$WIND_TOW * 100)/comp_nstrat$BTS_TOW, digits = 0)
# comp_nstrat$WIND_OVERLAP_NSTRAT_PCT <- round((comp_nstrat$WIND_NSTRAT * 100)/comp_nstrat$OVERLAP_NSTRAT, digits = 0) 
# # comp_nstrat$WIND_OVERLAP_TOW <- round((comp_nstrat$WIND_TOW * 100)/comp_nstrat$OVERLAP_TOW, digits = 0)
# 
# #write CSV
# write.csv(comp_nstrat, here("data", "summary-stats", "stata-species-summaries.csv"), row.names=FALSE)


# SUBSETS ######

## Season Catch by Area ###
# Split season summary columns by type and recombine as rows ### 
# overlap_ssn <- comp_sp_ssn %>%
#   select(SVSPP, SCINAME, COMNAME, SEASON, OVERLAP_CATCH, OVERLAP_TOW) %>% 
#   rename(EXPCATCHNUM = OVERLAP_CATCH, 
#          TOW = OVERLAP_TOW)
# overlap_ssn$TYPE <- paste("overlap")
# wind_ssn <- comp_sp_ssn %>%
#   select(SVSPP, SCINAME, COMNAME, SEASON, WIND_CATCH, WIND_TOW) %>% 
#   rename(EXPCATCHNUM = WIND_CATCH, 
#          TOW = WIND_TOW)
# wind_ssn$TYPE <- paste("wind")
# bts_ssn <- comp_sp_ssn %>%
#   select(SVSPP, SCINAME, COMNAME, SEASON, BTS_CATCH, BTS_TOW) %>% 
#   rename(EXPCATCHNUM = BTS_CATCH, 
#          TOW = BTS_TOW)
# bts_ssn$TYPE <- paste("whole")
# group_ssn <- rbind(bts_ssn, overlap_ssn, wind_ssn)
# #write.csv
# write.csv(group_ssn, here("data", "summary-stats", "season-catch-by-area.csv"), row.names=FALSE)

## Season Percent Catch by Area ###
### Split season percent columns by type and recombine as rows ### 
# overlap_ssn_pct <- comp_sp_ssn %>%
#   select(SVSPP, SCINAME, COMNAME, SEASON, OVERLAP_CATCHPCT, OVERLAP_TOWPCT) %>% 
#   rename(EXPCATCHPCT= OVERLAP_CATCHPCT, 
#          TOWPCT = OVERLAP_TOWPCT)
# overlap_ssn_pct$TYPE <- paste("overlap")
# wind_ssn_pct <- comp_sp_ssn %>%
#   select(SVSPP, SCINAME, COMNAME, SEASON, WIND_CATCHPCT, WIND_TOWPCT) %>% 
#   rename(EXPCATCHPCT = WIND_CATCHPCT, 
#          TOWPCT = WIND_TOWPCT)
# wind_ssn_pct$TYPE <- paste("wind")
# group_ssn_pct <- rbind(overlap_ssn_pct, wind_ssn_pct)

# # TOTAL SUMMARIES #####
# total_summaries <- rbind(compare_year, compare_strat, compare_species, compare_season, compare_time)#, total_tows)
# 
# #write.csv
# write.csv(total_summaries, here("data", "summary-stats", "total-summaries.csv"), row.names=FALSE)


## Black Sea Bass ###
# filter strata for BSB and for highest impacted strata by percent. Change STRATUM field to character for row binding later 
# bsb_compare_strat <- comp_sp_strat %>% 
#   filter(SVSPP==141, 
#          GROUP %in% c(1050, 1090, 1380, 1730, 3200, 3230))
# bsb_compare_strat$GROUP <- as.character(bsb_compare_strat$GROUP)
# 
# # filter yearly species impacts by BSB, and for years 2015-2021. Change YEAR field to character for row binding later.    
# bsb_comp_yr <- comp_sp_yr %>%
#   select(-c(CODE)) %>%
#   filter(SVSPP==141, YEAR %in% c(2015:2021)) %>%
#   rename(GROUP = YEAR) 
# bsb_comp_yr$GROUP <- as.character(bsb_comp_yr$GROUP)
# 
# # filter Time of Day species impacts for BSB. Change GROUP field to uppercase for aesthetic consistency.
# bsb_comp_time <- comp_sp_time %>%
#   select(-c(CODE)) %>%
#   filter(SVSPP==141)
# 
# # filter seasonal species impacts for BSB. 
# bsb_comp_ssn <- comp_sp_ssn %>%
#   select(-c(CODE)) %>%
#   filter(SVSPP==141) %>%
#   rename( GROUP = SEASON)
# 
# bsb <- rbind(bsb_comp_yr, bsb_compare_strat, bsb_comp_ssn, bsb_comp_time)
# 
# write.csv(bsb, here("data", "summary-stats", "bsb-summaries.csv"), row.names=FALSE)