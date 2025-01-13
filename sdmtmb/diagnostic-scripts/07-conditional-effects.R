### created: 04/29/2024
### last updated: 11/10/2024

# 07 - DIAGNOSTICS: CONDITIONAL EFFECTS ####

## OBJECTIVE ####
# visualize the fit of the model with visreg
# conditional on the other predictors being set to certain values.

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
# library(sf) 
library(sdmTMB)
# library(kableExtra)
library(ggeffects)
library(visreg)
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
plot.files <- here("outputs" ,"sdmtmb", species, "plots", "cond-effects") 

### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, "mods", season, str_c("m10_", season, ".rds", sep = ""))) ## FIXME as needed
# mod <- readRDS(here(dat.files, "mods", "dpg", "no-dep-out", str_c("m4_", season, ".rds", sep = "")))

## PREDICT CONDITIONAL EFFECTS ####
### Depth ####
depth_reg <- visreg(mod, xvar = "AVGDEPTH", plot = FALSE)

#### In response space ####
depth_resp <- visreg(mod, xvar = "AVGDEPTH", scale = "response", gg = TRUE)

### Year #####
year_reg <- visreg(mod, xvar = "EST_YEAR", gg = TRUE)

### Depth and year ####
depth_yr_reg <- visreg(mod, xvar = "AVGDEPTH", by = "EST_YEAR", plot = FALSE)

#### In response space ####
dep.yr_resp <- visreg(mod, xvar = "AVGDEPTH", by = "EST_YEAR", scale = "response", gg = TRUE)

### Area #### 
# area_reg <- visreg(mod, xvar = "AREA", gg = TRUE)
# # 
#### Depth and area ####
# depth_area_reg <- visreg(mod, xvar = "AVGDEPTH", by = "AREA", plot = FALSE)
# # 
#### In response space ####
# depth_area_resp <- visreg(mod, xvar = "AVGDEPTH", by = "AREA", scale = "response", gg = TRUE)
# # 
#### Year and area ####
# year_area <- visreg(mod, xvar = "EST_YEAR", by = "AREA", gg = TRUE)

## Save the Data ####
data <- list("depth_visreg_dat.rds" = depth_reg, 
             "depth_visreg.resp_dat.rds" = depth_resp,
             "year_visreg_dat.rds" = year_reg, 
             "dep.year_visreg_dat.rds" = depth_yr_reg, 
             "dep.year_visreg.resp_dat.rds" = dep.yr_resp#, 
             # area_visreg_dat = area_reg, 
             # dep.area_visreg_dat = depth_area_reg, 
             # dep.area_visreg.resp_dat = depth_area_resp, 
             # year.area_visreg_dat = year_area
)

pmap(list(data, names(data)), ~saveRDS(.x, file = here(dat.files, "cond-effects", str_c(season, .y, sep = "_"))))

## For delta models ####
### Depth ####
# depth_reg1 <- visreg_delta(mod, xvar = "AVGDEPTH", model = 1, gg = TRUE)
# depth_reg2 <- visreg_delta(mod, xvar = "AVGDEPTH", model = 2, gg = TRUE)
# 
# depth_reg_plot1 <- depth_reg1 + labs(x = "Average Depth", y = "Change in biomass in link space")
# depth_reg_plot2 <- depth_reg2 + labs(x = "Average Depth", y = "Change in biomass in link space")

#### In response space ####
# depth_resp_plot1 <- visreg_delta(mod, xvar = "AVGDEPTH", scale = "response", model = 1, gg = TRUE)
# depth_resp_plot2 <- visreg_delta(mod, xvar = "AVGDEPTH", scale = "response", model = 2, gg = TRUE)

### Year #####
# year_reg1 <- visreg_delta(mod, xvar = "EST_YEAR", model = 1, gg = TRUE)
# year_reg2 <- visreg_delta(mod, xvar = "EST_YEAR", model = 2, gg = TRUE)
# 
# year_reg_plot1 <- year_reg1 + labs(x = "Year", y = "Change in biomass in link space")
# year_reg_plot2 <- year_reg2 + labs(x = "Year", y = "Change in biomass in link space")

### Depth and year ####
# depth_yr_reg1 <- visreg_delta(mod, xvar = "AVGDEPTH", by = "EST_YEAR", model = 1, gg = TRUE)
# depth_yr_reg2 <- visreg_delta(mod, xvar = "AVGDEPTH", by = "EST_YEAR", model = 2, gg = TRUE)
# 
# depth_yr_reg_plot1 <- depth_yr_reg1 + labs(x = "Average Depth", y = "Change in biomass in link space")
# depth_yr_reg_plot2 <- depth_yr_reg2 + labs(x = "Average Depth", y = "Change in biomass in link space")

#### In response space ####
# dep.yr_resp_plot1 <- visreg_delta(mod, xvar = "AVGDEPTH", by = "EST_YEAR", scale = "response", model = 1, gg = TRUE)
# dep.yr_resp_plot2 <- visreg_delta(mod, xvar = "AVGDEPTH", by = "EST_YEAR", scale = "response", model = 2, gg = TRUE)
# 
# visreg2d_delta(mod,
#                xvar = "AVGDEPTH", yvar = "EST_YEAR",
#                model = 1, scale = "response", plot.type = "persp"
# )

### Save the data ####
# data <- list("dpg-mod1_depth_visreg_dat.rds" = depth_reg1,
#              "dpg-mod2_depth_visreg_dat.rds" = depth_reg2,
#              "dpg-mod1_year_visreg_dat.rds" = year_reg_plot1,
#              "dpg-mod2_year_visreg_dat.rds" = year_reg_plot2,
#              "dpg-mod1_dep-yr_visreg_dat.rds" = depth_yr_reg1,
#              "dpg-mod2_dep-yr_visreg_dat.rds" = depth_yr_reg2)
# 
# 
# pmap(list(data, names(data)), ~saveRDS(.x, file = here(dat.files, "cond-effects", .y)))
# 
# plots <- list("dpg-mod1_depth-visreg_plot.png" = depth_reg_plot1,
#               "dpg-mod2_depth-visreg_plot.png" = depth_reg_plot2,
#               "dpg-mod1_year-visreg_plot.png" = year_reg_plot1.1, 
#               "dpg-mod2_year-visreg_plot.png" = year_reg_plot2.1, 
#               "dpg-mod1_dep-yr-visreg_plot.png" = depth_yr_reg_plot1, 
#               "dpg-mod2_dep-yr-visreg_plot.png" = depth_yr_reg_plot2)
# 
# pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = .y, device = "png", path = here(plot.files, "cond-effects"), width = 10, height = 6))


## PLOT CONDITIONAL EFFECTS ####
### Depth ####
depth_plot <- ggplot(depth_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) +
  labs(x = "Average Depth", y = "Change in biomass", subtitle = str_c("Conditional effect of average depth on", season, species_name, "biomass", sep = " "))

depth_plot1 <- depth_plot + geom_point(aes(y = visregRes), data = depth_reg$res, size = 1, alpha = 0.4)


#### In response space ####
depth_resp_plot <- depth_resp + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = str_c("Conditional effect of average depth on", season, species_name, "biomass", sep = " "))


### Year ####
year_reg_plot <- year_reg + labs(x= "Year", y = "Change in biomass in link space", subtitle = str_c("Conditional effect of year on changes in", season, species_name,  "biomass", sep = " "))


#### In response space ####
# year_resp_plot + labs(x = "Year", y = "Biomass (kg)", subtitle = str_c("Conditional effect of year on", season, species_name, "biomass", sep = " "))
# 

### Depth & Year ####
depth_year_plot <- ggplot(depth_yr_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) +
  labs(x = "Average Depth", y = "Change in biomass", subtitle = str_c("Conditional effect of average depth on annual", season, species_name, "biomass", sep = " ")) +
  facet_wrap(~EST_YEAR)

depth_year_plot1 <- depth_year_plot + geom_point(aes(y = visregRes), data = depth_yr_reg$res, size = 1, alpha = 0.4)

#### In response space ####
dep.yr_resp_plot <- dep.yr_resp + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = str_c("Conditional effect of average depth on annual", season, species_name, "biomass", sep = " "))


### Area #### 
# area_reg + labs(x= "Year", y = "Change in biomass in link space", subtitle = str_c("Conditional effect of area on", season, species_name, "biomass", sep = " "))
# 
# 
### Depth and area ####
# depth_area_plot <- ggplot(depth_area_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) +
#   labs(x = "Average Depth", y = "Change in biomass", subtitle = str_c("Conditional effect of average depth on", season, species_name, "biomass by area", sep = " ")) +
#   facet_wrap(~AREA)
# depth_area_plot + geom_point(aes(y = visregRes), data = depth_area_reg$res, size = 1, alpha = 0.4)
# 
# 
#### In response space ####
# depth_area_resp + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = str_c("Conditional effect of average depth on", season, species_name, "biomass by area", sep = " "))
# 
# 
### Year and area ####
# year_area + labs(x= "Year", y = "Change in biomass in link space", subtitle = str_c("Conditional effect of year on", season, species_name, "biomass by area", sep = " "))
# 

## Save the Plots ####
plots <- list("depth_cond-eff.png" = depth_plot1,
              "depth-response_cond-eff.png" = depth_resp_plot,
              "year_cond-eff.png" = year_reg_plot,
              "depth-year_cond-eff.png" = depth_year_plot1,
              "depth-year-response_cond-eff.png" = dep.yr_resp_plot)


pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = str_c(season, .y, sep = "_"), device = "png", path = here(plot.files), width = 10, height = 6))
