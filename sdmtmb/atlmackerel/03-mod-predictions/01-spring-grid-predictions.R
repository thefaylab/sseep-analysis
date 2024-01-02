### created: 12/28/2023
### last updated: 

# 01 - GRID PREDICTIONS ####

## OBJECTIVE ####
# make predictions from the best fitting spatiotemporal model over the spring spatial footprint for Atlantic mackerel


### LOAD PACKAGES ####
library(sf)
library(patchwork)
library(here)
library(sdmTMB)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

### LOAD DATA ####
# load active strata shapefile 
strata_utm <- readRDS(here("data", "rds", "active_strata_utm.rds"))

# spring model for Atlantic mackerel decided here("sdmtmb", "atlmackerel", "02-mod-diagnostics", "04-compare-model-resids.Rmd")
spring_mod <- readRDS(here("sdmtmb", "atlmackerel", "data", "spring_mod.rds"))

# read in the grid with the area predictor created here("sdmtmb", "R", "make-survey-grid.R")
grid <- readRDS(here("sdmtmb", "survey_grid_Jun2022.rds")) 

ggplot(grid, aes(X, Y, fill = AVGDEPTH)) +
  geom_tile(width = 10000, height = 10000) +
  scale_fill_viridis_c() +
  coord_equal()


## MAKE PREDICTIONS ####
# replicate the grid across all necessary years
grid <- sdmTMB::replicate_df(grid, "EST_YEAR", c(2009:2019, 2021)) |> # spring survey was incomplete in 2020 so there cannot be a grid for 2020 to predict across
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA), 
         X = X/1000, # match X, Y coordinates used to fit model 
         Y = Y/1000)

# predictions
spring_preds <- predict(spring_mod, newdata = grid)#, return_tmb_object = TRUE) #|> as.data.frame()

### save the predictions
saveRDS(spring_preds, file = here("sdmtmb", "atlmackerel", "data", "atlmackerel_grid_preds.rds"))


