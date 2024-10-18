### created: 04/29/2024
### last updated: 

# 05 - MARGINAL EFFECTS ####

## OBJECTIVE ####
# plot marginal effects of given predictor variables
# calculates marginal effects with the effects package using the parameter covariance matrix.
# does not work with smoothers
# effects are “marginalized” or “averaged” over the other fixed effects

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)
library(ggeffects)
set.seed(123)
theme_set(theme_bw())

### Environment Set Up ####
# season 
season <- "fall"

# species
species <- "sumflounder"

species_name <- "summer flounder"

### File locations ####
dat.files <- here("sdmtmb",  species, "data") ## FIXME when repo is reorganized
plot.files <- here("sdmtmb", species, "plots", "marg-effects")

### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, "mods", season, str_c("m12_", season, ".rds", sep = ""))) ## FIXME as needed 



## PREDICT MARGINAL EFFECTS ####
### Depth ####
dep.eff <- ggpredict(mod, "AVGDEPTH [all]")

### Year ####
year.eff <- ggpredict(mod, "EST_YEAR [all]")

### Depth & Year ####
dep_yr.eff <- ggpredict(mod, terms = c("AVGDEPTH [all]", "EST_YEAR [all]"))

### Area ###
# area.eff <- ggpredict(mod, "AREA [all]")
# 
# ### Depth & Area ###
# dep_area.eff <- ggpredict(mod, terms = c("AVGDEPTH [all]", "AREA [all]"))
# 
# ### Year & Area ###
# year_area.eff <- ggpredict(mod, terms = c("EST_YEAR [all]", "AREA [all]"))

### Save the data ####
## FIXME when the repo is organized 
data <- list(depth_marg-eff = dep.eff, 
             year_marg-eff = year.eff, 
             dep.year_marg-eff = dep_yr.eff) |> 
  map(~list(.))

pmap(list(data, names(data)), ~saveRDS(.x, file = here(dat.files, "marg-effects", str_c(season,"_", .y, ".rds", sep = ""))))

# saveRDS(dep.eff, here(dat.files, "marg-effects", str_c(season, "depth_marg-eff.rds", sep = "_")))
# saveRDS(year.eff, here(dat.files, "marg-effects", str_c(season, "year_marg-eff.rds", sep = "_")))
# saveRDS(dep_yr.eff, here(dat.files, "marg-effects", str_c(season, "depth-year_marg-eff.rds", sep = "_")))
# saveRDS(area.eff, here(dat.files, "marg-effects", str_c(season, "area_marg-eff.rds", sep = "_")))
# saveRDS(dep_area.eff, here(dat.files, "marg-effects", str_c(season, "depth-area_marg-eff.rds", sep = "_")))
# saveRDS(year_area.eff, here(dat.files, "marg-effects", str_c(season, "year-area_marg-eff.rds", sep = "_")))

## PLOT MARGINAL EFFECTS ####
### Depth ####
plot(dep.eff) + labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))

# ggsave(str_c(season, "depth-effects.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)

### Year ####
plot(year.eff) + labs(x = "Year", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))

# ggsave(str_c(season, "year-effects.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)

### Depth & Year ####
ggplot(dep_yr.eff, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", color = "Year", title=str_c("Predicted values of", season, species_name, "biomass", sep = " ")) + xlim(0, NA)

# ggsave(str_c(season, "depth-year-effects.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)

ggplot(dep_yr.eff, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", color = "Year", title=str_c("Predicted values of", season, species_name, "biomass", sep = " ")) + xlim(0, NA) +
  facet_wrap(~group)

# ggsave(str_c(season, "depth-year-effects_facet.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)

# ### Area ###
# plot(area.eff) + labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))
# 
# # ggsave(str_c(season, "area-effects.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)
# 
# 
# ### Depth & Area ###
# plot(dep_area.eff, facet = TRUE) + labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))
# 
# # ggsave(str_c(season, "depth-area-effects_facet.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)
# 
# ### Year & Area ###
# plot(year_area.eff, facet = TRUE) + labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))
# 
# # ggsave(str_c(season, "year-area-effects_facet.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)

## FOR DELTA MODELS ####
# extract the model fitting data 
moddat <- mod$data

# create a new dataframe with the same variables as the linear predictor across the range of each variable used in model fitting
nd <- expand.grid(
  AVGDEPTH = seq(min(moddat$AVGDEPTH), max(moddat$AVGDEPTH), length.out = 200),
  EST_YEAR = unique(moddat$EST_YEAR))
saveRDS(nd, here(dat.files, "marg-effects", str_c(species, "new-df_marg-eff.rds", sep = "_")))

# generate model expectations at each new predictor value, 
p <- predict(mod, newdata = nd, re_form = NA) # ~0 or NA for population-level predictions
head(p)
saveRDS(p, here(dat.files, "marg-effects", str_c(species, "preds_marg-effs.rds", sep = "_")))

# plot 
ggplot(p, aes(AVGDEPTH, exp(est1) + exp(est2), colour = EST_YEAR)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))

# ggsave(str_c(season, "depth-year-effects.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)

ggplot(p, aes(AVGDEPTH, exp(est1) + exp(est2), colour = EST_YEAR)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " ")) + 
  facet_wrap(~EST_YEAR)

# ggsave(str_c(season, "depth-year-effects.png", sep = "_"), device = "png", last_plot(), here(plot.files), width = 7, height = 5)
