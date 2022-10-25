### Load Packages ###
library(ggplot2)
library(tidyverse)
library(here)
library(stringr)
#remotes::install_github("NOAA-EDAB/survdat",build_vignettes = TRUE)
#install.packages("lwgeom")
library(survdat)
library(data.table)

here()

### Load Data ###
retro_data <- read_csv(here("data", "clean-data", "merged_data_clean.csv")) %>% 
  rename(YEAR = EST_YEAR,               # rename EST_YEAR; strat_prep() needs year column
         BIOMASS = EXPCATCHWT,          # rename EXPCATCHWT; strat_mean() needs Biomass column
         ABUNDANCE = EXPCATCHNUM) %>%   # rename EXPCATCHNUM; strat_mean() needs Abundance column 
  as.data.table()                       # dataframe needs to be a data table in order to be read into strat_prep


### Prepare the Data ###
prepped_data <- strat_prep(retro_data) 


### Strat Mean ### 

#Remove length data if present
data.table::setkey(prepped_data, CRUISE6, STRATUM, STATION, SVSPP)
stratmeanData <- unique(prepped_data, by = key(prepped_data))
#stratmeanData[, c('LENGTH', 'NUMLEN') := NULL]

#Merge sex or keep separate
#if(mergesexFlag == F) stratmeanData[, group := paste(group, CATCHSEX, sep = '')]

data.table::setkey(stratmeanData, CRUISE6, STRATUM, STATION)
stratmeanData[, BIOMASS   := sum(BIOMASS),   by = key(stratmeanData)]
stratmeanData[, ABUNDANCE := sum(ABUNDANCE), by = key(stratmeanData)]
stratmeanData <- unique(stratmeanData, by = key(stratmeanData))

#Calculate total number of stations per year
data.table::setkey(stratmeanData, STRATUM, YEAR)
N <- unique(stratmeanData, by = key(stratmeanData))
N <- N[, sum(ntows), by = 'YEAR']
data.table::setnames(N, 'V1', 'N')




full <- calc_stratified_mean(stratmeanData, filterBySeason = "all")
