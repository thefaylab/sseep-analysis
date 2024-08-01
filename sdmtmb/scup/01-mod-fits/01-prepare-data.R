### created: 12/10/2022
### last updated: GF on 2024-08-01

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
         #EST_YEAR %in% c(2009:2016,2018,2019,2021,2022)) |> 
         EST_YEAR %in% c(2009:2019,2021,2022),
         !(EST_YEAR == 2017 & SEASON == "FALL")) |> 
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT),
         date = make_date(EST_YEAR, EST_MONTH, EST_DAY),
         day = yday(date))

# all <- read.csv(here("data","raw-data","NEFSC_BTS_ALLCATCHES.csv"))
# temp_data <- read.csv(here("data","raw-data","NEFSC_BTS_ALLCATCHES.csv")) |> 
#   dplyr::select(CRUISE6, STATION, STRATUM, BOTTEMP, EST_YEAR) |>
#   filter(EST_YEAR %in% c(2009:2016,2018,2019,2021,2022)) |>
#   group_by(STRATUM, CRUISE6, STATION) |>
#   distinct() |>
#   mutate(code = str_c(STRATUM, CRUISE6, STATION))
#   
# 
# scup_complete <- scup_complete0 |>
#   mutate(code = str_c(STRATUM, CRUISE6, STATION)) |>
#   left_join(temp_data, by="code") |>
#   select(!c(STRATUM.y,CRUISE6.y, STATION.y, EST_YEAR.y)) |>
#   rename(STRATUM = STRATUM.x, CRUISE6 = CRUISE6.x, STATION = STATION.x, EST_YEAR = EST_YEAR.x)

strata <- readRDS(here("data", "rds", "active_strata.rds")) %>% 
  select(STRATUM, Region)
scup_complete <- scup_complete %>% 
  left_join(strata)


mab_strata <- c(3450, 1050, 1610, 1090, 3410, 3380, 3020, 3460 ,3050, 3440, 3260, 3350, 8510, 1010,
1060, 3080, 3230, 3320, 3290, 8500, 1650, 1690, 7520, 1100, 3110, 3140, 3170, 1020,1740,1700,1730,3200, 1660,1620,1110,1070,1030,1750,1710,1670,1630,8520,1120,1080,1040,1760,1720,1680,1640,8530)
length(mab_strata)

### filter specific strata ####
### 
scup_fall <- scup_complete |> filter(SEASON == "FALL",
                                      STRATUM %in% mab_strata,
                                      AVGDEPTH <= 75) # GF 2024-08-01 - this seems like it needs revisiting



scup_spring <- scup_complete |> filter(SEASON == "SPRING",
                                        STRATUM %in% mab_strata,
                                        #AVGDEPTH %in% (20:150)) #based on data explore
                                        Region %in% c(1,8),
                                        day < 120,
                                        AVGDEPTH > 50,
                                        AVGDEPTH <= 140)


# #Catch 
# ggplot(scup_fall) +
#   geom_point(aes(x = AVGDEPTH, y = EXPCATCHWT)) + ylim(0,400) +
#  # facet_wrap(~YEAR) +
#   labs(x = "Depth (m)", y = "Catch (weight) per tow", subtitle = "Fall")

# ggplot(scup_fall) +
#   geom_point(aes(x = BOTTEMP, y = EXPCATCHWT)) +
#   # facet_wrap(~YEAR) +
#   labs(x = "Temp (C)", y = "Catch (weight) per tow", subtitle = "")

# 
# ggplot(scup_spring) +
#   geom_point(aes(x = AVGDEPTH, y = EXPCATCHWT)) + ylim(0,800) +
#   #facet_wrap(~YEAR) +
#   labs(x = "Depth (m)", y = "Catch (weight) per tow", subtitle = "Spring")

# 
# ggplot(scup_spring) +
#   geom_point(aes(x = BOTTEMP, y = EXPCATCHWT)) +
#   #facet_wrap(~YEAR) +
#   labs(x = "Temp (C)", y = "Catch (weight) per tow", subtitle = "")



#remove catch ouliers
# scup_fall <- scup_fall1 |> filter(EXPCATCHWT < 900,
#                                  !is.na(AVGDEPTH))
# scup_spring <- scup_spring1 |> filter(EXPCATCHWT < 900,
#                                      !is.na(AVGDEPTH))
# 
# 






## PREPARE SCUP DATA ####
scup_complete <- scup_complete |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR) 

scup_fall <- scup_fall |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) |> # convert lat long; default units are km 
  group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR) 

scup_spring <- scup_spring |>
  sdmTMB::add_utm_columns(c("DECDEG_BEGLON", "DECDEG_BEGLAT")) #|> # convert lat long; default units are km 
  #group_by(STRATUM, CRUISE6, STATION, SEASON, EST_YEAR)   #GF 2024-08-01 don't understand what the grouping is for/


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




#strata <- readRDS(here("data", "rds", "active_strata.rds")) 

dist_wt_fall <- ggplot(strata) + 
  geom_sf() +  
  geom_point(data=scup_fall, aes(x = DECDEG_BEGLON, y = DECDEG_BEGLAT, size=EXPCATCHWT), 
             color = "firebrick3", shape = 20, alpha=0.3) +   
  scale_size(range = c(.01,10)) +
  labs(title = "Fall distribution of catches (wt)", x = "Long", y = "Lat") +
  facet_wrap(~YEAR)


dist_num_spring <- ggplot(strata) + 
  geom_sf() +  
  geom_point(data=scup_spring, aes(x = DECDEG_BEGLON, y = DECDEG_BEGLAT, size=EXPCATCHWT), 
             color = "firebrick3", shape = 20, alpha=0.5) +   
  scale_size(range = c(.01,10)) +
  labs(title = "Spring distribution of catches (wt)", x = "Long", y = "Lat")+
  facet_wrap(~YEAR)







