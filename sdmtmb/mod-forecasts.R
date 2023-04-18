### created: 04/01/2023
### last updated: 04/17/2023

#### PREDICT AND FORECAST ####

###################
#### OBJECTIVE ####
###################
# run best fit models for fall and spring
# include extra years in the model fit for future distribution forecasting
# make predictions and forecasts over the impacted grid from model fits
# plot predictions over the impacted grid

####################


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
library(raster)
library(sdmTMB)
library(marmap)
library(oce)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
#source()

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

#### LOAD DATA ####
strata <- sf::st_read(dsn = here("gis", "NEFSC_BTS_AllStrata_Jun2022.shp")) %>% 
  rename(STRATUM = "Strata_Num")

# read in fall data for model fitting
sf_fall <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds"))
# read in the fall mesh for model fitting 
fall_mesh <- readRDS(here(sdmtmb.dir, "data", "fall_mesh.rds"))
# data wrangle
#sf_fall$scaled_depth <- (sf_fall$AVGDEPTH - mean(sf_fall$AVGDEPTH)) / sd(sf_fall$AVGDEPTH)
#sf_fall$scaled_year <- (sf_fall$EST_YEAR - mean(sf_fall$EST_YEAR)) / sd(sf_fall$EST_YEAR)
#sf_fall$scaled_area <- (sf_fall$AREA_CODE - mean(sf_fall$AREA_CODE)) / sd(sf_fall$AREA_CODE)

# read in spring data for model fitting
sf_spring <- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds"))
# read in the spring mesh for model fitting  
spring_mesh <- readRDS(here(sdmtmb.dir, "data", "spring_mesh.rds"))
#data wrangle
#sf_spring$scaled_depth <- (sf_spring$AVGDEPTH - mean(sf_spring$AVGDEPTH)) / sd(sf_spring$AVGDEPTH)
#sf_spring$scaled_year <- (sf_spring$EST_YEAR - mean(sf_spring$EST_YEAR)) / sd(sf_spring$EST_YEAR)
#sf_spring$scaled_area <- (sf_spring$AREA_CODE - mean(sf_spring$AREA_CODE)) / sd(sf_spring$AREA_CODE)

# read in the grid with the area predictor created here(sdmtmb.dir, "grids")
grid <- readRDS(here(sdmtmb.dir, "data", "impacted_grid.rds")) |> 
  rename("AVGDEPTH" = depth) |>
  mutate(X = X/1000, 
         Y = Y/1000)
         #scaled_depth = (depth - mean(depth)) / sd(depth))

# create a fall grid based on differences in scaled_area         
# scaled_area <- sf_fall |> 
#   select(AREA_CODE, scaled_area) |> 
#   unique()
# fall.grid <- left_join(grid, scaled_area, by = "AREA_CODE") 

# create a spring grid based on differences in scaled_area         
# scaled_area <- sf_spring |> 
#   select(AREA_CODE, scaled_area) |> 
#   unique()
# spr.grid <- left_join(grid, scaled_area, by = "AREA_CODE") 

# define extra years to forecast 
fall_extra_years <- c(2020, 2022:2026)
spring_extra_years <- c(2022:2026)

#### MODEL FITS ####
# re run model 11 from here(sdmtmb.dir, "02a-fit-fall-mods.R") with forecasted years
fall_mod <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + 
                      #as.factor(EST_YEAR) + 
                      as.factor(AREA),#-1,
                    data = sf_fall,
                    mesh = fall_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "AR1", 
                    extra_time = fall_extra_years, 
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE) 
saveRDS(fall_mod, here(sdmtmb.dir, "mar-536-project", "data", "fall_mod.rds"))

# re run model 8 from here(sdmtmb.dir, "02b-fit-spr-mods.R") with forecasted years
spring_mod <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + #as.factor(EST_YEAR) + 
                       as.factor(AREA),#-1,
                     data = sf_spring,
                     mesh = spring_mesh,
                     family = tweedie(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID",
                     extra_time = spring_extra_years,  
                     control = sdmTMBcontrol(newton_loops = 1), 
                     silent = FALSE) 
saveRDS(spring_mod, here("sdmtmb", "mar-536-project", "data", "spring_mod.rds"))

#### MAKE PREDICTIONS ####
# replicate our grid across all necessary years
grid <- sdmTMB::replicate_df(grid, "EST_YEAR", c(2009:2026)) 

# 
# spr.scaled_years <-  unique(spring_mod$data$scaled_year)
# spr.mod.years <- unique(spring_mod$data$EST_YEAR)
# spr.scaledyrs <- data.frame(EST_YEAR = spr.mod.years, 
#                             scaled_year = spr.scaled_years)

#spr.grid <- left_join(spr.grid, mutate(scaled_year = round(((EST_YEAR - mean(EST_YEAR)) / sd(EST_YEAR)), 7))

# predictions
fall_preds <- predict(fall_mod, newdata = grid)# |> as.data.frame()
spring_preds <- predict(spring_mod, newdata = grid) #|> as.data.frame()

### save the predictions
saveRDS(fall_preds, file = here(sdmtmb.dir, "data", "fall_projects.rds"))
saveRDS(spring_preds, file = here(sdmtmb.dir, "data", "spring_projects.rds"))

#### PLOT THE PREDICTIONS ####
# a ggplot function 
plot_map <- function(dat, column) {
  ggplot(dat, aes(X, Y, fill = {{ column }})) +
    geom_tile(width = 10, height = 10) +
    facet_wrap(~EST_YEAR) +
    coord_equal()
}


# biomass estimates (main + random effects)
plot_map(fall_preds |> filter(EST_YEAR %in% c(2018:2026)), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

plot_map(spring_preds |> filter(EST_YEAR %in% c(2018:2026)), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

# biomass estimates (main + random effects) in wind areas only
plot_map(fall_preds |> filter(EST_YEAR %in% c(2018:2026), AREA == "WIND"), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

plot_map(spring_preds |> filter(EST_YEAR %in% c(2018:2026), AREA == "WIND"), exp(est)) + scale_fill_viridis_c(trans = "sqrt", option = "H") +
  labs(fill = "Biomass estimates") +
  theme(legend.position = "bottom")

