### created: 05/03/2024
### last updated: 05/29/2024

# 07 - POSTERIOR PREDICTIVE CHECK ####

## OBJECTIVE ####
# simulate new data with fitted model 
# calculate abundance index and change in abundance index over time for simulated data and model predicted data
# plot distribution of simulated trend estimates compared to the model-predicted trend when survey effort is precluded by wind
# calculate p-value for differences between simulated data and predicted data

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(infer)
library(nationalparkcolors)
library(patchwork)
set.seed(123)
theme_set(theme_bw())

### Environment Set Up ####
# season 
season <- "spring"

# species
species <- "sumflounder"

species_name <- "summer flounder"

### File locations ####
dat.files <- here("data", "rds", "sdmtmb",  species) ## FIXME when repo is reorganized
post.check <- here(dat.files, "post-check") ## FIXME when repo is reorganized
plot.files <- here("outputs", "sdmtmb", species, "plots", "post-check")#, "07152024") ## FIXME when repo is reorganized

### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, "mods", season, str_c("m10_", season, ".rds", sep = ""))) ## FIXME as needed
# mod <- readRDS(here(dat.files, "mods", "dpg", "no-dep-out", str_c("m8_", season, ".rds", sep = "")))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))

### Data wrangle ####
# extract model fitting data and convert factors 
moddat <- mod$data |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR)), 
         AREA = as.character(AREA))

# extract model predictions
preds <- predict(mod) |> 
  mutate(EST_YEAR = as.integer(as.character(EST_YEAR))) |> 
  select(!EXPCATCHWT) |> 
  mutate(EXPCATCHWT = exp(est))

## SIMULATE DATASETS ####
simdat <- simulate(mod, nsim = 1000, re_form = NA, type = "mle-mvn") #simulate new random affects

# create unique column names 
colnames(simdat) <- paste0("sim", seq(1:1000))

### Bind datasets for calculations ####
simdat <- moddat |> 
  select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) |>
  bind_cols(simdat) |>
  pivot_longer(cols = c(sim1:sim1000), 
               names_to = "nsim", 
               values_to = "EXPCATCHWT") 


## CALCULATE STRATIFIED MEANS
source(here("R", "StratMeanFXs_v2.R")) # stratified mean function

### Model predicted estimates ####
preds_stratmus <- preds |> 
  group_by(EST_YEAR) |> 
  nest() |>
  mutate(stratmu_sq = map(data, ~stratified.mean(., strata) |> # calculate status quo indices
                            mutate(effort = "With Wind Included",
                                   season = str_to_title(season), 
                                   EST_YEAR = EST_YEAR)), 
         stratmu_precl = map(data , ~filter(., AREA == "OUTSIDE") |> # calculate precluded indices
                               stratified.mean(strata) |> 
                               mutate(effort = "With Wind Precluded",
                                      season = str_to_title(season),
                                      EST_YEAR = EST_YEAR))) |> 
  arrange(EST_YEAR)

### Simulated estimates ####
# calculate the stratified mean abundance index for each scenario's replicate and year 
sim_stratmus <- simdat |> 
  group_by(EST_YEAR, nsim) |> 
  nest() |>
  mutate(stratmu_sq = map(data, ~stratified.mean(., strata) |> # calculate status quo indices
                            mutate(effort = "With Wind Included",
                                   season = str_to_title(season), 
                                   EST_YEAR = EST_YEAR, 
                                   nsim = nsim)), 
         stratmu_precl = map(data , ~filter(., AREA == "OUTSIDE") |> # calculate precluded indices
                               stratified.mean(strata) |> 
                               mutate(effort = "With Wind Precluded",
                                      season = str_to_title(season), 
                                      EST_YEAR = EST_YEAR, 
                                      nsim = nsim))) 



## BIND STRATIFIED MEANS ####
### Model predicted means #### 
pred_strat_rows <- bind_rows(preds_stratmus$stratmu_sq, preds_stratmus$stratmu_precl)

### Simulated means ####
sim_strat_rows <- bind_rows(sim_stratmus$stratmu_sq, sim_stratmus$stratmu_precl)


## FIT LINEAR REGRESSIONS ####
### Model predicted trends ####
# estimate the change in model predicted abundance index over time for each scenario
pred_slopes <- pred_strat_rows |> 
  group_by(effort) |> 
  nest() |> 
  mutate(mods = map(data, ~lm(stratmu~EST_YEAR, data = .)), # fit models
         coefs = map(mods, broom::tidy, conf.int = TRUE), # extract coefficients 
         season = str_to_title(season)) |> 
  unnest(cols = coefs) |> 
  select(!c(data, mods)) |> 
  filter(term == "EST_YEAR") # extract the slopes


### Simulated trends ####
# estimate the simulated change in abundance index over time for each scenario
sim_slopes <- sim_strat_rows |>
  group_by(nsim, effort) |> 
  nest() |> 
  #select(nsim, stratmu, SEASON, TYPE) |> 
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)), # fit models
         coef = map(model, broom::tidy, conf.int = TRUE),  # extract coefficients
         season = str_to_title(season)) |> 
  unnest(cols = coef) |>
  select(nsim, term, estimate, conf.low, conf.high, season, effort) |>
  filter(term == "EST_YEAR") # extract the slopes


## CALCULATE CONFIDENCE INTERVALS OF SIMULATED ESTIMATES ####
sim_cis <- sim_slopes |> 
  group_by(effort, season) |> 
  nest() |> 
  mutate(lower = map(data, ~quantile(.$estimate, 0.025)),
         upper = map(data, ~quantile(.$estimate, 0.975))) |> 
  unnest(cols = c(lower, upper))


## PLOT POSTERIOR PREDICTIVE CHECK ####
source(here("R", "plot_fns.R"))
pal <- park_palette("SmokyMountains")

# Simulated status quo vs predicted preclusion estimates
sim.v.pred_sq.plot <- plot_distribution(sim_slopes, ci_dat = c(sim_cis$lower[1], sim_cis$upper[1])) + 
  geom_vline(aes(xintercept = as.numeric(pred_slopes[2,3]), linetype = effort, color = effort), linewidth = 1.5) +
  scale_color_manual(values = c(pal[6]), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(season)) #+ xlim(-12, 6)
  
# Simulated preclusion vs predicted preclusion estimates
sim.v.pred_precl.plot <- plot_distribution(sim_slopes, ci_dat = c(sim_cis$lower[2], sim_cis$upper[2]), scenario = "With Wind Precluded") +
  geom_vline(aes(xintercept = as.numeric(pred_slopes[2,3]), linetype = effort, color = effort), linewidth = 1.5) +
  scale_color_manual(values = c(pal[6]), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(season)) #+ xlim(-12,6)

# patchwork 
patchwork <- sim.v.pred_sq.plot + sim.v.pred_precl.plot + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

### Save the data ####
plots <- list("sim-pred_sq-plot.png" = sim.v.pred_sq.plot,
             "sim-pred_precl-plot.png" = sim.v.pred_precl.plot,
             "sim-pred_plots.png" = patchwork)

pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = str_c(season, .y, sep = "_"), device = "png", path = here(plot.files), width = 9, height = 5))



## CALCULATE P-VALUE ####
# extract precluded slope only
pred_precl_slope <- filter(pred_slopes, effort == "With Wind Precluded") |> 
  ungroup() |> 
  select(estimate)

pval <- sim_slopes |>
  group_by(effort) |>
  nest() |>
  bind_cols(pred_precl_slope) |>
  mutate(test_ct = map2(data, estimate, ~filter(., .x$estimate >= .y) |> nrow()),
         pval = map(test_ct, ~ .x / 1000)) |>
  select(effort, pval) |>
  unnest(cols = pval)


## SAVE THE DATA ####
data <- list("post-check_simdat.rds" = simdat,
             "preds_stratmus.rds" = preds_stratmus,
             "sim_stratmus.rds" = sim_stratmus,
             "preds_strat_rows.rds" = pred_strat_rows, 
             "sim_strat_rows.rds" = sim_strat_rows, 
             "pred_slopes.rds"= pred_slopes, 
             "post-check_sim-slopes.rds" = sim_slopes, 
             "post-check_conf-int.rds" = sim_cis,
             "post-pred_pvalue.rds" = pval) 

pmap(list(data, names(data)), ~saveRDS(.x, here(post.check, str_c(season, .y, sep = "_"))))
