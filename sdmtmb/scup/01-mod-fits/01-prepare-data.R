### created: 12/10/2022
### last updated: 11/27/2023

# 01 - PREPARE SCUP DATA ####

## OBJECTIVE ####
# prepare data for sdmTMB model fits and predicting


## LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(patchwork)
library(ggplot2)
library(gridExtra)
# library(marmap)
# library(raster)

here()

## LOAD DATA ####
# raw bottom trawl data; contains present only observations and will be used to pull depth, bottom temperature, and area swept values for each station below.  

scup_complete <- readRDS(here("data", "rds", "completed_bts_data.rds")) |> 
  filter(SVSPP == 143, 
         EST_YEAR %in% c(2009:2016,2018,2019,2021,2022)) |> 
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))


mab_strata <- c(3450, 1050, 1610, 1090, 3410, 3380, 3020, 3460 ,3050, 3440, 3260, 3350, 8510, 1010,
1060, 3080, 3230, 3320, 3290, 8500, 1650, 1690, 7520, 1100, 3110, 3140, 3170, 1020,1740,1700,1730,3200, 
1660,1620,1110,1070,1030,1750,1710,1670,1630,8520,1120,1080,1040,1760,1720,1680,1640,8530)
length(mab_strata)

### filter specific strata ####
### 
scup_fall <- scup_complete |> filter(SEASON == "FALL",
                                      STRATUM %in% mab_strata,
                                      AVGDEPTH <= 75)



scup_spring <- scup_complete |> filter(SEASON == "SPRING",
                                        STRATUM %in% mab_strata,
                                        AVGDEPTH %in% (20:150)) #based on data explore


#Catch 
ggplot(scup_fall) +
  geom_point(aes(x = AVGDEPTH, y = EXPCATCHWT)) + ylim(0,400) +
 # facet_wrap(~YEAR) +
  labs(x = "Depth (m)", y = "Catch (weight) per tow", subtitle = "Fall")

ggplot(scup_spring) +
  geom_point(aes(x = AVGDEPTH, y = EXPCATCHWT)) + ylim(0,800) +
  #facet_wrap(~YEAR) +
  labs(x = "Depth (m)", y = "Catch (weight) per tow", subtitle = "Spring")


## PREPARE SCUP DATA ####
scup_complete <- scup_complete |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR) 

scup_fall <- scup_fall |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR) 

scup_spring <- scup_spring |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR) 


# save data 
saveRDS(scup_fall, here("sdmtmb", "scup", "data", "scup_fall.rds"))
saveRDS(scup_spring, here("sdmtmb", "scup", "data", "scup_spring.rds"))


## CONSTRUCT MESH #### 
mesh <- make_mesh(scup_complete, xy_cols = c("X", "Y"), cutoff = 10) 
fall_mesh <- make_mesh(scup_fall, xy_cols = c("X", "Y"), cutoff = 10)
spring_mesh <- make_mesh(scup_spring, xy_cols = c("X", "Y"), cutoff = 10)
#cutoff defines the minimum allowed distance between points in the units of X and Y (km)

#mesh$mesh$n 
plot(mesh)
plot(fall_mesh)
plot(spring_mesh)

# save mesh
saveRDS(mesh, here("sdmtmb", "scup", "data", "mesh.rds"))
saveRDS(fall_mesh, here("sdmtmb", "scup", "data", "fall_mesh.rds"))
saveRDS(spring_mesh, here("sdmtmb", "scup", "data", "spring_mesh.rds"))
