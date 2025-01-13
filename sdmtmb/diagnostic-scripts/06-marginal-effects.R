### created: 04/29/2024
### last updated: 11/10/2024

# 06 - DIAGNOSTICS: MARGINAL EFFECTS ####

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
library(sdmTMB)
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
dat.files <- here("data", "rds", "sdmtmb",  species)
plot.files <- here("outputs", "sdmtmb", species, "plots", "marg-effects")

### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, "mods", season, str_c("m10_", season, ".rds", sep = ""))) ## FIXME as needed 



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

data <- list("depth_marg-eff.rds" = dep.eff, 
             "year_marg-eff.rds" = year.eff, 
             "dep.year_marg-eff.rds" = dep_yr.eff)

pmap(list(data, names(data)), ~saveRDS(.x, file = here(dat.files, "marg-effects", str_c(season, .y, sep = "_"))))


## PLOT MARGINAL EFFECTS ####
### Depth ####
dep_eff_plot <- plot(dep.eff) + labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))

### Year ####
year_eff_plot <- plot(year.eff) + labs(x = "Year", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))

### Depth & Year ####
dep_yr.eff_plot <-  ggplot(dep_yr.eff, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", color = "Year", title=str_c("Predicted values of", season, species_name, "biomass", sep = " ")) + xlim(0, NA)

dep_yr.eff_facet <- ggplot(dep_yr.eff, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", color = "Year", title=str_c("Predicted values of", season, species_name, "biomass", sep = " ")) + xlim(0, NA) +
  facet_wrap(~group)

### Save the plots ####
plots <- list(dep_eff_plot,
              year_eff_plot,
              dep_yr.eff_plot,
              dep_yr.eff_facet)
names(plots) <- c(str_c(season, "depth-effects.png", sep = "_"), 
                  str_c(season, "year-effects.png", sep = "_"), 
                  str_c(season, "depth-year-effects.png", sep = "_"),
                  str_c(season, "depth-year-effects_facet.png", sep = "_"))

pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = .y, device = "png", path = here(plot.files), width = 8, height = 6))



# ### Area ###
# plot(area.eff) + labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))
# 
# 
# ### Depth & Area ###
# plot(dep_area.eff, facet = TRUE) + labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))
# 
# 
# ### Year & Area ###
# plot(year_area.eff, facet = TRUE) + labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))
# 


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
depth_marg_plot1 <- ggplot(p, aes(AVGDEPTH, exp(est1 + est2), colour = EST_YEAR)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " "))

depth_marg_plot2 <- ggplot(p, aes(AVGDEPTH, exp(est1 + est2), colour = EST_YEAR)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", title=str_c("Predicted values of", season, species_name, "biomass", sep = " ")) + 
  facet_wrap(~EST_YEAR)

### Save the plots ####
plots <- list("dpg-mod_depth-marg_plot1.png" = depth_marg_plot1,
              "dpg-mod_depth-marg_plot2.png" = depth_marg_plot2)

pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = .y, device = "png", path = here(plot.files, "marg-effects"), width = 8, height = 6))
