### created: 12/10/2022
### last updated: 03/05/2023

#### 05a - FALL MODEL SIMULATIONS ####

###################
#### OBJECTIVE ####
###################
# simulate new data with fitted model and bind to summer flounder survey data 

####################


#### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
# library(marmap)
# library(raster)

here()

#### LOAD DATA ####
# best fit model created in `02a-fit-models.R` here("sdmtmb")
m6_fall <- readRDS(file = here("sdmtmb", "model-outputs", "m6_fall.rds"))

# worst fit model created in `02a-fit-models.R` here("sdmtmb")
#m1 <- readRDS(file = here("sdmtmb", "model-outputs", "m1.rds"))

# summer flounder data created in `01-prepare-data.R` here("sdmtmb").
sf_fall <- readRDS(here("sdmtmb", "data", "sumflounder_fall.rds"))



#### SIMULATE DATASETS ####
# run two simulations on each model
sim_m6fall <- simulate(m6_fall, nsim = 1000)
colnames(sim_m6fall) <- paste0("sim", seq(1:1000))
saveRDS(sim_m6fall, here("sdmtmb", "data", "m6fall-sims.rds"))


#sim1_m1 <- simulate(m1, nsim = 2)
# dim(s)
# s[1:3,1:4]


#### BIND DATASETS FOR CALCULATIONS ####
# bind summer flounder data with simulated data from m6 model
m6fall_simdat <- sf_fall %>% 
  select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) %>%
  bind_cols(sim_m6fall) 
  # rename(sim1 = "...10",
  #        sim2 = "...11") %>% 
  # mutate(model = "m6")

# save the data
saveRDS(m6fall_simdat, here("sdmtmb", "data", "sf-fall-with-m6sims.rds"))


# bind summer flounder data with simulated data from m1 model  
# sf_m1sim <- sumflounder %>% 
#   select(STRATUM, CRUISE6, STATION, depth, year, GEO_AREA, AREA, X, Y) %>%
#   bind_cols(sim1_m1) %>% 
#   rename(sim1 = "...10",
#          sim2 = "...11") %>% 
#   mutate(model = "m1")
# 
# # save the data
# saveRDS(sf_m1sim, here("sdmtmb", "data", "sumflounder-m1sims.rds"))
