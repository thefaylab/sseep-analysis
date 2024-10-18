### created: 04/29/2024
### last updated: 

# 06 - CONDITIONAL EFFECTS ####

## OBJECTIVE ####
# visualize the fit of the model with visreg
# conditional on the other predictors being set to certain values.

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
library(visreg)
set.seed(123)
theme_set(theme_bw())

### Environment Set Up ####
# season 
season <- "spring"

# species
species <- "atlmackerel"

species_name <- "atlantic mackerel"

### File locations ####
dat.files <- here("sdmtmb",  species, "data") ## FIXME when repo is reorganized
plot.files <- here("sdmtmb", species, "plots", "cond-effects") ## FIXME when repo is reorganized

### Read in data ####
# best fit model according to AIC or other model selection
# mod <- readRDS(here(dat.files, "mods", season, str_c("m12_", season, ".rds", sep = ""))) ## FIXME as needed
mod <- readRDS(here(dat.files, "mods", "dpg", "no-dep-out", str_c("m4_", season, ".rds", sep = "")))

## PREDICT CONDITIONAL EFFECTS ####
### Depth ####
depth_reg <- visreg(mod, xvar = "AVGDEPTH", plot = FALSE)

#### In response space ####
depth_resp_plot <- visreg(mod, xvar = "AVGDEPTH", scale = "response", gg = TRUE)

### Year #####
year_reg_plot <- visreg(mod, xvar = "EST_YEAR", gg = TRUE)

### Depth and year ####
depth_yr_reg <- visreg(mod, xvar = "AVGDEPTH", by = "EST_YEAR", plot = FALSE)

#### In response space ####
dep.yr_resp_plot <- visreg(mod, xvar = "AVGDEPTH", by = "EST_YEAR", scale = "response", gg = TRUE)

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

### For delta models ####
#### Depth ####
# depth_reg1 <- visreg_delta(mod, xvar = "AVGDEPTH", model = 1, gg = TRUE)
# depth_reg2 <- visreg_delta(mod, xvar = "AVGDEPTH", model = 2, gg = TRUE)
# 
##### In response space ####
# depth_resp_plot1 <- visreg_delta(mod, xvar = "AVGDEPTH", scale = "response", model = 1, gg = TRUE)
# depth_resp_plot2 <- visreg_delta(mod, xvar = "AVGDEPTH", scale = "response", model = 2, gg = TRUE)
# 
#### Year #####
# year_reg_plot1 <- visreg_delta(mod, xvar = "EST_YEAR", model = 1, gg = TRUE)
# year_reg_plot2 <- visreg_delta(mod, xvar = "EST_YEAR", model = 2, gg = TRUE)
# 
#### Depth and year ####
# depth_yr_reg1 <- visreg_delta(mod, xvar = "AVGDEPTH", by = "EST_YEAR", model = 1, gg = TRUE)
# depth_yr_reg2 <- visreg_delta(mod, xvar = "AVGDEPTH", by = "EST_YEAR", model = 2, gg = TRUE)
# 
##### In response space ####
# dep.yr_resp_plot1 <- visreg_delta(mod, xvar = "AVGDEPTH", by = "EST_YEAR", scale = "response", model = 1, gg = TRUE)
# dep.yr_resp_plot2 <- visreg_delta(mod, xvar = "AVGDEPTH", by = "EST_YEAR", scale = "response", model = 2, gg = TRUE)
# 
# visreg2d_delta(mod,
#                xvar = "AVGDEPTH", yvar = "EST_YEAR",
#                model = 1, scale = "response", plot.type = "persp"
# )

## Save the data ####
data <- list("depth_visreg_dat.rds" = depth_reg, 
             "depth_visreg.resp_plot.rds" = depth_resp_plot,
             "year_visreg_plot.rds" = year_reg_plot, 
             "dep.year_visreg_dat.rds" = depth_yr_reg, 
             "dep.year_visreg.resp_plot.rds" = dep.yr_resp_plot#, 
             # area_visreg_dat = area_reg, 
             # dep.area_visreg_dat = depth_area_reg, 
             # dep.area_visreg.resp_plot = depth_area_resp, 
             # year.area_visreg_plot = year_area
             ) #|> 
  # map(~list(.))

## FIXME when the repo is organized 
pmap(list(data, names(data)), ~saveRDS(.x, file = here(dat.files, "cond-effects", str_c(season, .y, sep = "_"))))


## PLOT CONDITIONAL EFFECTS ####
### Depth ####
depth_plot <- ggplot(depth_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) +
  labs(x = "Average Depth", y = "Change in biomass", subtitle = str_c("Conditional effect of average depth on", season, species_name, "biomass", sep = " "))

depth_plot + geom_point(aes(y = visregRes), data = depth_reg$res, size = 1, alpha = 0.4)

ggsave(str_c(season, "depth", "cond-eff.png", sep = "_"), last_plot(), device = "png", here(plot.files), width = 8, height = 6)

#### In response space ####
depth_resp_plot + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = str_c("Conditional effect of average depth on", season, species_name, "biomass", sep = " "))

ggsave(str_c(season, "depth-response", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 8, height = 4)

### Year ####
year_reg_plot + labs(x= "Year", y = "Change in biomass in link space", subtitle = str_c("Conditional effect of year on changes in", season, species_name,  "biomass", sep = " "))

ggsave(str_c(season, "year", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 8, height = 4)

#### In response space ####
# year_resp_plot + labs(x = "Year", y = "Biomass (kg)", subtitle = str_c("Conditional effect of year on", season, species_name, "biomass", sep = " "))
# 
# ggsave(str_c(season, "year-response", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 8, height = 4)

### Depth & Year ####
depth_year_plot <- ggplot(depth_yr_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) +
  labs(x = "Average Depth", y = "Change in biomass", subtitle = str_c("Conditional effect of average depth on annual", season, species_name, "biomass", sep = " ")) +
  facet_wrap(~EST_YEAR)

depth_year_plot + geom_point(aes(y = visregRes), data = depth_yr_reg$res, size = 1, alpha = 0.4)
 
ggsave(str_c(season, "depth-year", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 8, height = 6)

#### In response space ####
dep.yr_resp_plot + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = str_c("Conditional effect of average depth on annual", season, species_name, "biomass", sep = " "))

ggsave(str_c(season, "depth-year-response", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 10, height = 5)

### Area #### 
# area_reg + labs(x= "Year", y = "Change in biomass in link space", subtitle = str_c("Conditional effect of area on", season, species_name, "biomass", sep = " "))
# 
# ggsave(str_c(season, "area", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 8, height = 4)
# 
### Depth and area ####
# depth_area_plot <- ggplot(depth_area_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) +
#   labs(x = "Average Depth", y = "Change in biomass", subtitle = str_c("Conditional effect of average depth on", season, species_name, "biomass by area", sep = " ")) +
#   facet_wrap(~AREA)
# depth_area_plot + geom_point(aes(y = visregRes), data = depth_area_reg$res, size = 1, alpha = 0.4)
# # 
# ggsave(str_c(season, "depth-area", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 8, height = 4)
# 
#### In response space ####
# depth_area_resp + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = str_c("Conditional effect of average depth on", season, species_name, "biomass by area", sep = " "))
# # 
# ggsave(str_c(season, "depth-area-resp", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 8, height = 4)
# 
### Year and area ####
# year_area + labs(x= "Year", y = "Change in biomass in link space", subtitle = str_c("Conditional effect of year on", season, species_name, "biomass by area", sep = " "))
# 
# ggsave(str_c(season, "year-area", "cond-eff.png", sep = "_"), plot = last_plot(), device = "png", here(plot.files), width = 8, height = 4)
