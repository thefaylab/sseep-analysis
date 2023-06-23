
# Load packages
library(VAST)
library(splines)
library(effects)
library(rio)
library(tidyverse)
library(truncnorm)
library(R.utils)
library(data.table)
library(here)
library(mgcv)
library(patchwork)



dat <- readRDS("data/data_full.rds")
geounits <- read.csv(here("data/mergedpresence3.csv"), sep=";")


geounits <- geounits %>% 
  select(STRATUM, CRUISE6, STATION, GEO_AREA) %>% 
  group_by(STRATUM, CRUISE6, STATION) %>% 
  distinct() %>% 
  mutate(code = str_c(STRATUM, CRUISE6, STATION))

#Cross data with Ecological Production Units (MAB, GOM, GB, SS)
dat2 <- dat %>% 
  mutate(code = str_c(STRATUM.x, CRUISE6.x, STATION.x)) %>%
  left_join(geounits, by="code")


# filter 95% of the distribution 
dat2.95 <- dat2 %>% 
  group_by(SVSPP, COMNAME, SEASON) %>% 
  mutate(csum_prop = round((cumsum(EXPCATCHWT)/max(cumsum(EXPCATCHWT))),2)) %>% # calculate the cumulative sum of biomass; divide each value of cumulative sum by the maximum sum for each COMNAME and SEASON combination to find the proportion each sum makes up of the total distribution of sums
  filter(csum_prop <= 0.95) # filter the data where the proportion is less than or equal to 95% of the total distribution of biomass for each COMNAME and SEASON group to identify which strata make up the majority of the biomass.


#Filtering for sp of interes
#species <- c(141, 105, 106, 121,15,131,32,72,503,22,23,24,25,26,27,28, 103, 143) 
#sp.names <- c("BLACK SEA BASS", "YELLOWTAIL FLOUNDER", "WINTER FLOUNDER", "ATLANTIC MACKEREL", "SPINY DOGFISH",
#              "BUTTERFISH", "ATLANTIC HERRING", "SILVER HAKE", "LONGFIN SQUID", "BARNDOOR SKATE", "WINTER SKATE",
#              "CLEARNOSE SKATE", "ROSETTE SKATE", "LITTLE SKATE", "SMOOTH SKATE", "THORNY SKATE","SUMMER FLOUNDER", "SCUP")


spp_depth_1 <- dat2.95 |>
  dplyr::select(SEASON, EST_YEAR, EST_MONTH, DAYTIME, DECDEG_BEGLAT, DECDEG_BEGLON, EXPCATCHWT,
                EXPCATCHNUM, MAXDEPTH, AVGDEPTH, BOTTEMP, SURFTEMP,SVSPP,COMNAME,AREA, GEO_AREA) |>
  filter(SVSPP %in% c(141, 105, 106, 121,15,131,32,72,503,22,23,24,25,26,27,28, 103, 143), EXPCATCHWT > 0.1) |>
  group_by(COMNAME) |>
  drop_na() |>
  summarise(mean.depth = round(mean(AVGDEPTH), digits = 2),
            sd.depth = round(sd(AVGDEPTH), digits = 2))

spp_depth_2 <- dat2 |>
  dplyr::select(SEASON, EST_YEAR, EST_MONTH, DAYTIME, DECDEG_BEGLAT, DECDEG_BEGLON, EXPCATCHWT,
                EXPCATCHNUM, MAXDEPTH, AVGDEPTH, BOTTEMP, SURFTEMP,SVSPP,COMNAME,AREA, GEO_AREA) |>
  filter(SVSPP %in% c(141, 105, 106, 121,15,131,32,72,503,22,23,24,25,26,27,28, 103, 143), EXPCATCHWT > 0.1) |>
  group_by(COMNAME,GEO_AREA) |>
  drop_na() |>
  summarise(mean.depth = round(mean(AVGDEPTH), digits = 2),
            sd.depth = round(sd(AVGDEPTH), digits = 2))


