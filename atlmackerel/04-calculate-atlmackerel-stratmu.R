### created: 01/26/2024
### last updated: 04/27/2025

# 04 - CALCULATE STRATIFIED MEAN: ATLANTIC MACKEREL  ####


## OBJECTIVE ####
# calculate stratified mean index using the Atlantic mackerel footprint under the status quo and preclusion scenarios 
# compare the indices using a mean percent relative difference for the most recent 5 years 
# calculate the change in indices over time using a linear regression
# plot the indices over time 

### LOAD PACKAGES ####
library(here)
library(tidyverse)
library(nationalparkcolors)
source(here("R", "StratMeanFXs_v2.R"))
source(here("R", "plot_fns.R"))

pal <- park_palette("Badlands")

### LOAD DATA ####
# strata and year combinations that will be removed during the stratified mean calculations because only one tow was conducted. created from `05b-spatial-filter-data.R` here("tidy-data").
one_tow_strata <- readRDS(here("data", "rds", "one_tow_strata.rds"))

# spring Atlantic mackerel data consisting of observations filtered here("atlmackerel", "02-filter-atlantic-mackerel.R"). Remove occurances of year and strata where only one tow occurred in a given season.
atlmackerel <- readRDS(here("data", "atlmackerel", "atlmackerel_spring.rds")) #|> 
  # anti_join(one_tow_strata, by = c("SEASON", "EST_YEAR", "STRATUM"))

#active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

## DATA WRANGLE ####
# create data frames of year and strata combinations where only one tow occurred
### Status quo effort 
remove_incl_strata <- one_tow_strata |> 
  select(!precl_strata) |> unnest(cols = incl_strata)
### Wind-precluded effort
remove_precl_strata <- one_tow_strata |> 
  select(!incl_strata) |> unnest(cols = precl_strata)

## CALCULATE STRATIFIED MEAN ####
### With Wind Included ####
stratmu_incl <- atlmackerel |> 
  anti_join(remove_incl_strata, by = c("SEASON", "EST_YEAR", "STRATUM")) |>
  group_by(SEASON, EST_YEAR) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  mutate(effort = "Status quo survey effort") |> 
  select(!data) |> 
  unnest(cols = stratmu)

### With Wind Precluded ####
# remove observations of year and strata where only one tow occurred due to reduced effort
# mack_precl_data <- atlmackerel |> 
#   filter(AREA == "OUTSIDE") |> 
#   group_by(SEASON) |> 
#   nest() |> 
#   mutate(tow_sum = map(data, ~group_by(., EST_YEAR, STRATUM) |> 
#                          summarise(tow_ct = length(unique(TOWID))) |> 
#                          filter(tow_ct == 1)), 
#          filtered_data = map2(data, tow_sum, ~anti_join(.x, .y, by = c("EST_YEAR", "STRATUM")))) |> 
#   select(!c(data, tow_sum)) |> 
#   unnest(cols = filtered_data)

# calculate the abundance index
stratmu_precl <- atlmackerel |> 
  filter(AREA == "OUTSIDE") |> 
  anti_join(remove_precl_strata, by = c("SEASON", "EST_YEAR", "STRATUM")) |>
  group_by(SEASON, EST_YEAR) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  mutate(effort = "Wind-precluded survey effort") |> 
  select(!data) |> 
  unnest(cols = stratmu)


stratmu_rows <- bind_rows(stratmu_incl, stratmu_precl) 

stratmu_cols <- left_join(stratmu_incl, stratmu_precl, by = c("SEASON", "EST_YEAR")) 

## FIT LINEAR REGRESSIONS ####
stratmu_lms <- stratmu_rows |>
  group_by(SEASON, effort) |> 
  nest() |>
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)),  
         coef = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(coef) |>
  select(SEASON, effort, term, estimate, conf.low, conf.high) 

stratmu_slopes <- stratmu_lms |>
  filter(term == "EST_YEAR")

precl_slopes <- stratmu_slopes |> 
  filter(effort == "Wind-precluded survey effort")

incl_slopes <- stratmu_slopes |> 
  filter(effort == "Status quo survey effort")

slopes_cols <- left_join(incl_slopes, precl_slopes, by = "SEASON")

# fall_lms <- stratmu_lms |> 
#   filter(SEASON == "FALL")

spring_lms <- stratmu_lms |> 
  filter(SEASON == "SPRING")


## MEAN PERCENT RELATIVE DIFFERENCE ####
mudiff_dat <- stratmu_cols |> 
  group_by(SEASON) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = stratmu.x, observed = stratmu.y)), # calculate the relative errors 
         year_sel = map(errors, ~arrange(., EST_YEAR) |> tail(5)),
         mudiff = map(year_sel, ~mean.diff(., group_by = NULL))) |> # calculate the mean percent difference
  dplyr::select(!c(data, errors, year_sel)) |> 
  unnest(cols = mudiff)

cvdiff_dat <- stratmu_cols |> 
  group_by(SEASON) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = cv.x, observed = cv.y)), # calculate the relative errors 
         year_sel = map(errors, ~arrange(., EST_YEAR) |> tail(5)),
         cv.diff = map(year_sel, ~mean.diff(., group_by = NULL))) |> # calculate the mean percent difference; NULL drops any groups remaining on the df
  dplyr::select(!c(data, errors, year_sel)) |> 
  unnest(cols = cv.diff)

slope_diff <- slopes_cols |> 
  calc.errors(observed = estimate.y, expected = estimate.x) |> 
  group_by(SEASON) |> 
  mean.diff(group_by = "SEASON") 


## PLOT ####
stratmu_rows |> 
  plot.stratmu(EST_YEAR, color = effort, shape = effort) + 
  facet_wrap(~str_to_title(SEASON)) #+ 

ggsave("am_stratmeans.png", last_plot(), device = "png", here("outputs", "init-analysis-plots", "atlmackerel"), width = 9, height = 5)

## TABLE #### 
diff.tbl <- left_join(mudiff_dat, cvdiff_dat, by = "SEASON") |> 
  left_join(slope_diff, by = "SEASON") |> 
  select(SEASON, MARE.x, MARE.y, MAE) |> 
  rename(stratmu_mare = MARE.x, 
         cv_mare = MARE.y, 
         slope_mae = MAE) |> 
  mutate(species = "Atlantic mackerel")


# save data 
save.data <- list("mudiff_dat.rds" = mudiff_dat, 
                  "cvdiff_dat.rds" = cvdiff_dat,
                  "slope_diff.rds" = slope_diff,
                  "stratmu_included.rds" = stratmu_incl, 
                  # "mackerel_precluded_dat.rds" = mack_precl_data,
                  "stratmu_precluded.rds" = stratmu_precl, 
                  "stratmu_rows.rds" = stratmu_rows, 
                  "stratmu_cols.rds" = stratmu_cols, 
                  "stratmu_linregs.rds" = stratmu_lms, 
                  "obs_slopes.rds" = stratmu_slopes, 
                  "obs_slopes_cols.rds" = slopes_cols,
                  "diff_summary_tbl.rds" = diff.tbl)

pmap(list(save.data, names(save.data)), ~saveRDS(.x, here("data", "atlmackerel", .y)))


