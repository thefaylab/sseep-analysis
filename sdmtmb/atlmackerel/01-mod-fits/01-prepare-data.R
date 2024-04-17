### created: 11/02/2023
### last updated: 04/08/2024

# 01 - PREPARE ATLANTIC MACKEREL DATA ####

## OBJECTIVE ####
# prepare Atlantic mackerel data for sdmTMB model fits and predicting


## LOAD PACKAGES ####
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
# library(sf) 
library(sdmTMB)


here()
atlmack.dat <-  here("sdmtmb", "atlmackerel", "data")

## LOAD DATA ####
# dataset created from `02-filter-atlantic-mackerel.R` here("atlmackerel"). Contains complete observations for Atlantic mackerel. 
data <- readRDS(here("data", "atlmackerel", "atlmackerel_spring.rds"))

# dataset created from `02-filter-atlantic-mackerel.R` here("atlmackerel"). Contains complete observations for Atlantic mackerel and removes outliers greater than quantile(EXPCATCHWT, 0.99)
data_no.out <- readRDS(here("data", "atlmackerel", "atlmackerel_spring_no-out.rds"))


## PREPARE ATLANTIC MACKEREL DATA ####
# add utm columns for model fitting
data <- data |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT"), utm_crs = 32618) #|> # convert lat long; default units are km 
 
# add utm columns for model fitting
data_no.out <- data_no.out |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT"), utm_crs = 32618)

# remove the NA values in depth
na <- filter(data, is.na(AVGDEPTH)) # check for any na values
data <- data |> 
 filter(!is.na(AVGDEPTH)) # keep everything that is not NA

na_no.out <- filter(data_no.out, is.na(AVGDEPTH)) # check for any na values
data_no.out <- data_no.out |> 
  filter(!is.na(AVGDEPTH)) # keep everything that is not NA

# remove observations at depths deeper than 200m for model fitting to avoid variable outliers in the data
data_200m <- data |> 
  filter(AVGDEPTH <= 200)


# save data 
saveRDS(data, here(atlmack.dat, "atlmackerel_spring.rds"))
saveRDS(data_200m, here(atlmack.dat, "atlmackerel_no-200-obs.rds"))
saveRDS(data_no.out, here(atlmack.dat, "atlmackerel_spring_no-outliers.rds"))

## CONSTRUCT MESH #### 
#mesh <- make_mesh(data, xy_cols = c("X", "Y"), cutoff = 10) 

spring_mesh <- make_mesh(data, xy_cols = c("X", "Y"), cutoff = 20) #469 knots
spring_mesh_200m <- make_mesh(data_200m, xy_cols = c("X", "Y"), cutoff = 20) #429 knots
spring_mesh_no.out <- make_mesh(data_no.out, xy_cols = c("X", "Y"), cutoff = 20) #470 knots 
#cutoff defines the minimum allowed distance between points in the units of X and Y (km)

plot(spring_mesh)
plot(spring_mesh_200m)
plot(spring_mesh_no.out)

# save mesh
saveRDS(spring_mesh, here(atlmack.dat, "spring_mesh.rds"))
saveRDS(spring_mesh_200m, here(atlmack.dat, "spring_mesh_no-200-obs.rds"))
saveRDS(spring_mesh_no.out, here(atlmack.dat, "spring_mesh_no-outliers.rds"))
