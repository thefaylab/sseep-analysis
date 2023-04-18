### created: 12/10/2022
### last updated: 03/03/2023

#### 02a - FIT FALL MODELS ####

###################
#### OBJECTIVE ####
###################
# fit various forms of models using prepared summer flounder data  

####################


#### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)

here()


#### LOAD DATA ####
# summer flounder data 
# sumflounder <- readRDS(here("sdmtmb", "data", "sumflounder.rds"))
sumflounder <- readRDS(here("sdmtmb", "data", "sumflounder.rds"))

# mesh 
fall_mesh <- readRDS(here("sdmtmb", "data", "fall_mesh.rds"))
spring_mesh <- readRDS(here("sdmtmb", "data", "spring_mesh.rds"))

#### DATA WRANGLING #### 
fall <- sumflounder |> 
  group_by(SEASON) |> 
  nest() |> 
  filter(SEASON == "FALL")
spring <- sumflounder |> 
  group_by(SEASON) |> 
  nest() |> 
  filter(SEASON == "SPRING")

#### CREATE TABLE FOR LOOP ####
formulae <- c("EXPCATCHWT ~ AVGDEPTH", 
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", 
              "EXPCATCHWT ~ AVGDEPTH", 
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", 
              "EXPCATCHWT ~ AVGDEPTH", 
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", 
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", 
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) + as.factor(AREA)", 
              "EXPCATCHWT ~ AVGDEPTH",
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", 
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) + as.factor(AREA)", 
              "EXPCATCHWT ~ AVGDEPTH", 
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", 
              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) + local trendas.factor(AREA)")
spatial.fields <- c(rep("off", 2), 
                    rep("on", 4), 
                    "off", 
                    rep("on", 7))
time.field <- c(rep("-", 4), 
                rep("EST_YEAR", 10))
spatiotemporal <- c(rep("-", 4), 
                    rep("IID", 4), 
                    rep("AR1", 3), 
                    rep("RW", 3))
mods <- data.frame(model = paste0("m", seq(1,14)), 
                   formula = formulae, 
                   spatial = spatial.fields, 
                   time = time.field, 
                   spatiotemporal = spatiotemporal) |> 
  group_by(model) |>
  nest()

pmap(list(fall, mods), ~sdmTMB(formula = .y$formula, 
                               data = .x$data, 
                               mesh = fall_mesh, 
                               time = ifelse()))
