### created: 12/05/2023
### last updated: 11/10/2024

# 05 - CALCULATE STRATIFIED MEAN: SUMMER FLOUNDER ####


## OBJECTIVE ####
# calculate stratified mean index using the summer flounder footprint under the status quo and preclusion scenarios 
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
# summer flounder data consisting of observations filtered based on seasonal footprints created here("sumflounder", "04-filter-summer-flounder.R")
sumflounder <- readRDS(here("data", "sumflounder", "sumflounder.rds"))

#active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))


## CALCULATE STRATIFIED MEAN ####
# with wind included
sf_stratmu_incl <- sumflounder |> 
  group_by(SEASON, EST_YEAR) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  mutate(effort = "With Wind Included") |> 
  select(!data) |> 
  unnest(cols = stratmu)

# with wind precluded
sf_stratmu_precl <- sumflounder |> 
  filter(AREA == "OUTSIDE") |> 
  group_by(SEASON, EST_YEAR) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  mutate(effort = "With Wind Precluded") |> 
  select(!data) |> 
  unnest(cols = stratmu)

sf_stratmu_rows <- bind_rows(sf_stratmu_incl, sf_stratmu_precl) #|> # bind the data sets 

sf_stratmu_cols <- left_join(sf_stratmu_incl, sf_stratmu_precl, by = c("SEASON", "EST_YEAR"))

## FIT LINEAR REGRESSIONS ####
sf_stratmu_lms <- sf_stratmu_rows |>
  group_by(SEASON, effort) |> 
  nest() |>
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)),  
         coef = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(coef) |>
  select(SEASON, effort, term, estimate, conf.low, conf.high) 

sf_stratmu_slopes <- sf_stratmu_lms |>
  filter(term == "EST_YEAR")

precl_slopes <- sf_stratmu_slopes |> 
  filter(effort == "With Wind Precluded")

incl_slopes <- sf_stratmu_slopes |> 
  filter(effort == "With Wind Included")

slopes_cols <- left_join(incl_slopes, precl_slopes, by = "SEASON")

fall_lms <- sf_stratmu_lms |> 
  filter(SEASON == "FALL")

spring_lms <- sf_stratmu_lms |> 
  filter(SEASON == "SPRING")



## MEAN PERCENT RELATIVE DIFFERENCE ####
mudiff_dat <- sf_stratmu_cols |> 
  calc.errors(observed = stratmu.y, expected = stratmu.x) |> 
  group_by(SEASON) |> 
  mean.diff()

cvdiff_dat <- sf_stratmu_cols |> 
  calc.errors(observed = cv.y, expected = cv.x) |> 
  group_by(SEASON) |> 
  mean.diff()

slope_diff <- slopes_cols |> 
  calc.errors(observed = estimate.y, expected = estimate.x) |> 
  group_by(SEASON) |> 
  mean.diff()



## PLOT ####
sf_stratmu_rows |> 
  plot.stratmu(EST_YEAR, color = effort, shape = effort) + 
  facet_wrap(~str_to_title(SEASON)) #+ 


ggsave("sf_stratmeans.png", last_plot(), device = "png", here("outputs", "sumflounder"), width = 9, height = 5)

## TABLE #### 
diff.tbl <- left_join(mudiff_dat, cvdiff_dat, by = "SEASON") |> 
  left_join(slope_diff, by = "SEASON") |> 
  select(SEASON, MARE.x, MARE.y, MARE) |> 
  rename(stratmu_mare = MARE.x, 
         cv_mare = MARE.y, 
         slope_mare = MARE) |> 
  mutate(species = "Summer flounder")


# save data 
save.data <- list("mudiff_dat.rds" = mudiff_dat, 
                  "cvdiff_dat.rds" = cvdiff_dat,
                  "slope_diff.rds" = slope_diff,
                  "stratmu_included.rds" = sf_stratmu_incl, 
                  "stratmu_precluded.rds" = sf_stratmu_precl, 
                  "stratmu_rows.rds" = sf_stratmu_rows, 
                  "stratmu_cols.rds" = sf_stratmu_cols, 
                  "stratmu_linregs.rds" = sf_stratmu_lms, 
                  "obs_slopes.rds" = sf_stratmu_slopes, 
                  "diff_summary_tbl.rds" = diff.tbl)

pmap(list(save.data, names(save.data)), ~saveRDS(.x, here("data", "sumflounder", .y)))

