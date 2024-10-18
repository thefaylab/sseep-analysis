### created: 02/21/2024
### updated: 

# CALCULATE SIMULATION STATISTICS  ####


## OBJECTIVE ####


### LOAD PACKAGES #### 
library(here)
# library(infer)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
source(here("R", "StratMeanFXs_v2.R"))


### ENVIRONMENT SET UP ####
set.seed(72346)
# set season and species to be simulated
season <- "spring"

species <- "sumflounder"

# number of simulations 
nsims <- 1:1000

# set file locations of where to read in data and save outputs
#dat.file <- here("data", species)
dat.files <- here("sdmtmb", species, "data")
sim.dat <- here("sdmtmb", species, "data", "simulations", season)
plot.files <- here("sdmtmb", species, "plots", "simulations")

### LOAD DATA ####
# 
future_locs <- readRDS(here(sim.dat, str_c(season, species, length(nsims), "future_locs.rds", sep = "_")))

#
sim.NoEff.tows <- readRDS(here(sim.dat, str_c(season, species, "no-effect", length(nsims), "surv-sims.rds", sep = "_")))

#
sim.base.tows <- readRDS(here(sim.dat, str_c(season, species, "baseline", length(nsims), "surv-sims.rds", sep = "_")))

#
sim.inc.tows <- readRDS(here(sim.dat, str_c(season, species, "enhanced", length(nsims), "surv-sims.rds", sep = "_")))

#
sim.dec.tows <- readRDS(here(sim.dat, str_c(season, species, "reduced", length(nsims), "surv-sims.rds", sep = "_")))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

### DATA WRANGLE ####
noeff.tows.df <- map(sim.NoEff.tows, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~bind_cols(.x, .y)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "No Wind Effect", 
         season = season)
# saveRDS(noeff.tows.df, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "five-yr-tows.rds", sep = "_")))

base.tows.df <- map(sim.base.tows, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~bind_cols(.x, .y)) |> 
         map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
         mutate(scenario = "Baseline", 
                season = season)
# saveRDS(base.tows.df, here(sim.dat, str_c(season, species, "baseline", length(nsims), "five-yr-tows.rds", sep = "_")))

inc.tows.df <- map(sim.inc.tows, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~bind_cols(.x, .y)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "Enhanced", 
         season = season)
# saveRDS(inc.tows.df, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "five-yr-tows.rds", sep = "_")))

dec.tows.df <- map(sim.dec.tows, ~janitor::clean_names(.) |> dplyr::select(est_year, x, y, observed, as_factor_area_wind)) |>
  map2(future_locs, ~bind_cols(.x, .y)) |> 
  map2_dfr(nsims, ~mutate(.x, sim = .y)) |> 
  mutate(scenario = "Reduced", 
         season = season)
# saveRDS(dec.tows.df, here(sim.dat, str_c(season, species, "reduced", length(nsims), "five-yr-tows.rds", sep = "_")))


## CALCULATE STRATIFIED MEANS ####
### No Wind Effect ####
noeff_strat <-  noeff.tows.df |> 
  rename(EXPCATCHWT = observed) |> 
  group_by(sim, future_year, scenario) |> 
  nest() |> 
  mutate(incl_stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Included")), 
         precl_stratmu = map(data, ~filter(., AREA == "OUTSIDE") |> stratified.mean(strata) |> mutate(effort = "With Wind Precluded"))) 
# saveRDS(noeff_strat, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "all-strats.rds", sep = "_")))

noeff_precl <- noeff_strat |> 
  select(!c(data, incl_stratmu)) |> 
  unnest(cols = precl_stratmu)
# saveRDS(noeff_precl, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "precl-strat.rds", sep = "_")))


noeff_incl <- noeff_strat |> 
  select(!c(data, precl_stratmu)) |> 
  unnest(cols = incl_stratmu) 
# saveRDS(noeff_incl, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "incl-strat.rds", sep = "_")))

noeff_strat_rows <- bind_rows(noeff_incl, noeff_precl) |> mutate(scenario = "No Wind Effect")
# saveRDS(noeff_strat_rows, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "all-strat-rows.rds", sep = "_")))


noeff_strat_cols <- left_join(noeff_incl, noeff_precl, by = c("sim", "future_year")) |> janitor::clean_names()
# saveRDS(noeff_strat_cols, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "all-strat-cols.rds", sep = "_")))


### Baseline ####
# base_inc.strat  <- map(base.tows.df, ~rename(., EXPCATCHWT = observed) |> 
#                          group_by(future_year, scenario) |>
#                          nest() |> 
#                          mutate(stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Included"))) |>
#                          dplyr::select(!data) |> 
#                          unnest(cols = stratmu))
# # saveRDS(base_inc.strat, here(sim.dat, str_c(season, species, "baseline", length(nsims), "incl-strat.rds", sep = "_")))
# 
# base_precl.strat  <- map(base.tows.df, ~rename(., EXPCATCHWT = observed) |> 
#                            filter(AREA == "OUTSIDE") |> 
#                            group_by(future_year, scenario) |>
#                            nest() |> 
#                            mutate(stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Precluded"))) |>
#                            dplyr::select(!data) |> 
#                            unnest(cols = stratmu))
# saveRDS(base_precl.strat, here(sim.dat, str_c(season, species, "baseline", length(nsims), "precl-strat.rds", sep = "_")))

base_strat <-  base.tows.df |> 
  rename(EXPCATCHWT = observed) |> 
  group_by(sim, future_year, scenario) |> 
  nest() |> 
  mutate(incl_stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Included")), 
         precl_stratmu = map(data, ~filter(., AREA == "OUTSIDE") |> stratified.mean(strata) |> mutate(effort = "With Wind Precluded"))) 
# saveRDS(base_strat, here(sim.dat, str_c(season, species, "baseline", length(nsims), "all-strats.rds", sep = "_")))

base_precl <- base_strat |> 
  select(!c(data, incl_stratmu)) |> 
  unnest(cols = precl_stratmu)
# saveRDS(base_precl, here(sim.dat, str_c(season, species, "baseline", length(nsims), "precl-strat.rds", sep = "_")))


base_incl <- base_strat |> 
  select(!c(data, precl_stratmu)) |> 
  unnest(cols = incl_stratmu) 
# saveRDS(base_incl, here(sim.dat, str_c(season, species, "baseline", length(nsims), "incl-strat.rds", sep = "_")))

base_strat_rows <- bind_rows(base_incl, base_precl) |> mutate(scenario = "Baseline")
# saveRDS(base_strat_rows, here(sim.dat, str_c(season, species, "baseline", length(nsims), "all-strat-rows.rds", sep = "_")))


base_strat_cols <- left_join(base_incl, base_precl, by = c("sim", "future_year")) |> janitor::clean_names()
# saveRDS(base_strat_cols, here(sim.dat, str_c(season, species, "baseline", length(nsims), "all-strat-cols.rds", sep = "_")))


# for calculating mudiff
# base_strat <- map2(base_inc.strat, base_precl.strat, ~left_join(.x, .y, by = "future_year") |> janitor::clean_names())
# # saveRDS(base_strat, here(sim.dat, str_c(season, species, "baseline", length(nsims), "all-strat-cols.rds", sep = "_")))
# 
# # for plotting
# base_strat_rows <- map2_dfr(base_inc.strat, base_precl.strat, ~bind_rows(.x, .y), .id = "sim")
# # saveRDS(base_strat_rows, here(sim.dat, str_c(season, species, "baseline", length(nsims), "all-strat-rows.rds", sep = "_")))


### Enhanced ####
# inc_inc.strat  <- map(inc.tows.df, ~rename(., EXPCATCHWT = observed) |> 
#                         group_by(future_year, scenario) |>
#                         nest() |> 
#                         mutate(stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Included"))) |>
#                         dplyr::select(!data) |> 
#                         unnest(cols = stratmu))
# # saveRDS(inc_inc.strat, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "incl-strat.rds", sep = "_")))
# 
# inc_precl.strat  <- map(inc.tows.df, ~rename(., EXPCATCHWT = observed) |> filter(AREA == "OUTSIDE") |> 
#                           group_by(future_year, scenario) |>
#                           nest() |> 
#                           mutate(stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Precluded"))) |>
#                           dplyr::select(!data) |> 
#                           unnest(cols = stratmu))
# # saveRDS(inc_precl.strat, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "precl-strat.rds", sep = "_")))
# 
# # for calculating mudiff
# inc_strat <- map2(inc_inc.strat, inc_precl.strat, ~left_join(.x, .y, by = "future_year") |> janitor::clean_names())
# # saveRDS(inc_strat, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "all-strat-cols.rds", sep = "_")))
# 
# # for plotting
# inc_strat_rows <- map2_dfr(inc_inc.strat, inc_precl.strat, ~bind_rows(.x, .y), .id = "sim")
# # saveRDS(inc_strat_rows, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "all-strat-rows.rds", sep = "_")))

inc_strat <-  inc.tows.df |> 
  rename(EXPCATCHWT = observed) |> 
  group_by(sim, future_year, scenario) |> 
  nest() |> 
  mutate(incl_stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Included")), 
         precl_stratmu = map(data, ~filter(., AREA == "OUTSIDE") |> stratified.mean(strata) |> mutate(effort = "With Wind Precluded")))
# saveRDS(inc_strat, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "all-strats.rds", sep = "_")))


inc_precl <- inc_strat |> 
  select(!c(data, incl_stratmu)) |> 
  unnest(cols = precl_stratmu) 
# saveRDS(inc_precl, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "precl-strat.rds", sep = "_")))


inc_incl <- inc_strat |> 
  select(!c(data, precl_stratmu)) |> 
  unnest(cols = incl_stratmu)
# saveRDS(inc_incl, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "incl-strat.rds", sep = "_")))


inc_strat_rows <- bind_rows(inc_incl, inc_precl) |> mutate(scenario = "Enhanced")
# saveRDS(inc_strat_rows, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "all-strat-rows.rds", sep = "_")))

inc_strat_cols <- left_join(inc_incl, inc_precl, by = c("sim", "future_year")) |> janitor::clean_names()
# saveRDS(inc_strat_cols, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "all-strat-cols.rds", sep = "_")))


### Reduced ####
dec_strat <-  dec.tows.df |> 
  rename(EXPCATCHWT = observed) |> 
  group_by(sim, future_year, scenario) |> 
  nest() |> 
  mutate(incl_stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Included")), 
         precl_stratmu = map(data, ~filter(., AREA == "OUTSIDE") |> stratified.mean(strata) |> mutate(effort = "With Wind Precluded"))) 
# saveRDS(dec_strat, here(sim.dat, str_c(season, species, "reduced", length(nsims), "all-strats.rds", sep = "_")))


dec_precl <- dec_strat |> 
  select(!c(data, incl_stratmu)) |> 
  unnest(cols = precl_stratmu) 
# saveRDS(dec_precl, here(sim.dat, str_c(season, species, "reduced", length(nsims), "precl-strat.rds", sep = "_")))


dec_incl <- dec_strat |> 
  select(!c(data, precl_stratmu)) |> 
  unnest(cols = incl_stratmu) 
# saveRDS(dec_incl, here(sim.dat, str_c(season, species, "reduced", length(nsims), "incl-strat.rds", sep = "_")))


dec_strat_rows <- bind_rows(dec_incl, dec_precl) |> mutate(scenario = "Reduced")
# saveRDS(dec_strat_rows, here(sim.dat, str_c(season, species, "reduced", length(nsims), "all-strat-rows.rds", sep = "_")))


dec_strat_cols <- left_join(dec_incl, dec_precl, by = c("sim", "future_year")) |> janitor::clean_names()
# saveRDS(dec_strat_cols, here(sim.dat, str_c(season, species, "reduced", length(nsims), "all-strat-cols.rds", sep = "_")))


# dec_inc.strat  <- map(dec.tows.df, ~rename(., EXPCATCHWT = observed) |> 
#                         group_by(future_year, scenario) |>
#                         nest() |> 
#                         mutate(stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Included"))) |>
#                         dplyr::select(!data) |> 
#                         unnest(cols = stratmu))
# # saveRDS(dec_inc.strat, here(sim.dat, str_c(season, species, "reduced", length(nsims), "incl-strat.rds", sep = "_")))
# 
# dec_precl.strat  <- map(dec.tows.df, ~rename(., EXPCATCHWT = observed) |> filter(AREA == "OUTSIDE") |> 
#                           group_by(future_year, scenario) |>
#                           nest() |> 
#                           mutate(stratmu = map(data, ~stratified.mean(., strata) |> mutate(effort = "With Wind Precluded"))) |>
#                           dplyr::select(!data) |> 
#                           unnest(cols = stratmu))
# # saveRDS(dec_precl.strat, here(sim.dat, str_c(season, species, "reduced", length(nsims), "precl-strat.rds", sep = "_")))
# 
# # for calculating mudiff
# dec_strat <- map2(dec_inc.strat, dec_precl.strat, ~left_join(.x, .y, by = "future_year") |> janitor::clean_names())
# # saveRDS(dec_strat, here(sim.dat, str_c(season, species, "reduced", length(nsims), "all-strat-cols.rds", sep = "_")))
# 
# # for plotting 
# dec_strat_rows <- map2_dfr(dec_inc.strat, dec_precl.strat, ~bind_rows(.x, .y), .id = "sim")
# # saveRDS(dec_strat_rows, here(sim.dat, str_c(season, species, "reduced", length(nsims), "all-strat-rows.rds", sep = "_")))

## CALCULATE ERROR AND PERCENT DIFFERENCES ####
# base_mudiff <- map2_dfr(base_strat, ~group_by(., future_year) |>
#                          calc.errors(stratmu_x, stratmu_y) |> ungroup() |> mean.diff(), .id = "sim") |> mutate(scenario = "Baseline")
# # saveRDS(base_mudiff, here(sim.dat, str_c(season, species, "baseline", length(nsims), "mudiff.rds", sep = "_")))
# 
# 
# inc_mudiff <- map_dfr(inc_strat, ~group_by(., future_year) |> calc.errors(stratmu_x, stratmu_y) |> ungroup() |> mean.diff(), .id = "sim") |> mutate(scenario = "Enhanced")
# # saveRDS(inc_mudiff, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "mudiff", sep = "_")))
# 
# 
# dec_mudiff <- map_dfr(dec_strat, ~group_by(., future_year) |> calc.errors(stratmu_x, stratmu_y) |> ungroup() |> mean.diff(), .id = "sim") |> mutate(scenario = "Reduced")
# saveRDS(dec_mudiff, here(sim.dat, str_c(season, species, "reduced", length(nsims), "mudiff.rds", sep = "_")))

### No Wind Effect ####
noeff_errors <- noeff_strat |> 
  mutate(errors = map2(incl_stratmu, precl_stratmu, ~calc.errors(., observed = .y$stratmu, expected = .x$stratmu))) |> 
  select(!c(data, incl_stratmu, precl_stratmu)) |> 
  unnest(cols = errors)
# saveRDS(noeff_errors, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "errors.rds", sep = "_")))


noeff_mudiff <- noeff_errors |> 
  group_by(sim, scenario) |> 
  mean.diff()
# saveRDS(noeff_mudiff, here(sim.dat, str_c(season, species, "no-effect", length(nsims), "mudiff.rds", sep = "_")))



### Baseline ####
base_errors <- base_strat |> 
  mutate(errors = map2(incl_stratmu, precl_stratmu, ~calc.errors(., observed = .y$stratmu, expected = .x$stratmu))) |> 
  select(!c(data, incl_stratmu, precl_stratmu)) |> 
  unnest(cols = errors)
# saveRDS(base_errors, here(sim.dat, str_c(season, species, "baseline", length(nsims), "errors.rds", sep = "_")))


base_mudiff <- base_errors |> 
  group_by(sim, scenario) |> 
  mean.diff()
# saveRDS(base_mudiff, here(sim.dat, str_c(season, species, "baseline", length(nsims), "mudiff.rds", sep = "_")))


### Enhanced ####
inc_errors <- inc_strat |> 
  mutate(errors = map2(incl_stratmu, precl_stratmu, ~calc.errors(.,  observed = .y$stratmu, expected = .x$stratmu))) |> 
  select(!c(data, incl_stratmu, precl_stratmu)) |> 
  unnest(cols = errors)
# saveRDS(inc_errors, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "errors.rds", sep = "_")))


inc_mudiff <- inc_errors |> 
  group_by(sim, scenario) |> 
  mean.diff()
# saveRDS(inc_mudiff, here(sim.dat, str_c(season, species, "enhanced", length(nsims), "mudiff.rds", sep = "_")))


### Reduced ####
dec_errors <- dec_strat |> 
  mutate(errors = map2(incl_stratmu, precl_stratmu, ~calc.errors(.,  observed = .y$stratmu, expected = .x$stratmu))) |> 
  select(!c(data, incl_stratmu, precl_stratmu)) |> 
  unnest(cols = errors)
# saveRDS(dec_errors, here(sim.dat, str_c(season, species, "reduced", length(nsims), "errors.rds", sep = "_")))


dec_mudiff <- dec_errors |> 
  group_by(sim, scenario) |> 
  mean.diff()
# saveRDS(dec_mudiff, here(sim.dat, str_c(season, species, "reduced", length(nsims), "mudiff.rds", sep = "_")))


