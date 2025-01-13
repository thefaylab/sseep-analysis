### created: 2024-11-02
### last updated: 

# 03 - DIAGNOSTICS: CALCULATE DEVIANCE ####

## OBJECTIVE ####
# this script will:
## extract log likelihood values from each seasonal model fit
## fit a null (intercept-only) model for a given season and species 
## calculate the deviance based on the log likelihood values 
## calcultate the percent deviance explained by each model run compared to the null model

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
set.seed(123)

### Environment Set Up ####
# season 
season <- "spring"

# species
species <- "atlmackerel"

species_name <- "atlantic mackerel"

### File locations ####
dat.files <- here("data", "rds", "sdmtmb",  species) 

# model locations
mod.locs <- here(dat.files, "mods", season)


### Read in data ####
# number of models to read in 
mod.num <- length(list.files(mod.locs, pattern = "[a-z]\\d+")) # find files in mod.locs that contain a pattern related to the model file names only; excludes folders. 
# this is used here rather than assigning list.files(mod.locs) into mod.files (below), because models numbered in the teens are listed between model 1 and 2 due to folder organization. 

# file names that will be read in 
mod.files <- str_c("m",seq(mod.num), "_", season, ".rds", sep = "")

# extract only the m# string
mod.names <- str_c("m",seq(mod.num)) 

# read in the data 
mods.list <- mod.files |>
  map(~list(.)) |>
  map(~readRDS(here(mod.locs, .)))

## RUN A NULL MODEL FOR COMPARISON ####
# dataset containing complete observations for species based on their generated seasonal spatial footprint. 
data <- readRDS(here(dat.files, str_c(species, "_", season, ".rds", sep = "")))

# mesh
mesh <- readRDS(here(dat.files, str_c(season, "mesh.rds", sep = "_")))

# null model
null_mod <- sdmTMB(formula = EXPCATCHWT ~ 1, data = data, mesh = mesh, family = tweedie(), spatial = "off")

## EXTRACT LOG LIKELIHOOD ####
log_lik <- map(mods.list, ~logLik(.)) |> 
  as.numeric() |>
  as.data.frame() |>
  rename(log_likelihood = `as.numeric(map(mods.list, ~logLik(.)))`)
rownames(log_lik) <- mod.names

null_logLik <- logLik(null_mod)

## CALCULATE DEVIANCE ####
# multiply log likelihood to calculate the deviance explained by the mode
deviance <- log_lik |> 
  mutate(deviance = log_likelihood*(-2),
         dev_exp = deviance/(null_logLik*(-2)), 
         perc_dev_exp = round((1-dev_exp)*100, 2)) |> 
  rownames_to_column(var = "models")
        

## SAVE THE DATA 
save.data <- list("log_likelihoods.rds"= log_lik, 
                  "deviance.rds" = deviance) 

pmap(list(save.data, names(save.data)), ~saveRDS(.x, here(dat.files, str_c(season, .y, sep = "_"))))

