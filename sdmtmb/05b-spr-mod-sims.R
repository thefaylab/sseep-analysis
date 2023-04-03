### created: 12/10/2022
### last updated: 03/05/2023

#### 05b - SPRING MODEL SIMULATIONS ####

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
library(janitor)
# library(marmap)
# library(raster)

here()

#### LOAD DATA ####
# best fit model created in `02a-fit-models.R` here("sdmtmb")
m6_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m6_spring.rds"))

# worst fit model created in `02a-fit-models.R` here("sdmtmb")
#m1 <- readRDS(file = here("sdmtmb", "model-outputs", "m1.rds"))

# summer flounder data created in `01-prepare-data.R` here("sdmtmb").
sf_spring <- readRDS(here("sdmtmb", "data", "sumflounder_spring.rds"))



#### SIMULATE DATASETS ####
# run two simulations on each model
sim_m6spr <- simulate(m6_spring, nsim = 1000) 
colnames(sim_m6spr) <- paste0("sim", seq(1:1000))
saveRDS(sim_m6spr, here("sdmtmb", "data", "m6spr-sims.rds"))

#sim1_m1 <- simulate(m1, nsim = 2)
# dim(s)
# s[1:3,1:4]



#### BIND DATASETS FOR CALCULATIONS ####
# bind summer flounder data with simulated data from m6 model
m6spr_simdat <- sf_spring %>% 
  select(SVSPP, STRATUM, CRUISE6, STATION, AVGDEPTH, EST_YEAR, AREA, X, Y, SEASON) %>%
  bind_cols(sim_m6spr) #%>% 
  # rename(sim1 = "...10",
  #        sim2 = "...11") %>% 
  # mutate(model = "m6")

# save the data
saveRDS(m6spr_simdat, here("sdmtmb", "data", "sf-spr-with-m6sims.rds"))


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
