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


# Prepare the Data #####
prepped_data <- strat_prep(retro_data) 


# Strat Mean ##### 
stratmeanData <- strat_mean(prepped_data)


## Survdat Strat Mean #####
data.table::setkey(prepped_data, CRUISE6, STRATUM, STATION, SVSPP)
stratmeanData <- unique(prepped_data, by = key(prepped_data))
stratmeanData <- stratmeanData %>% rename(strat = STRATA)

# adjust_stratmean <- function (prepData, groupDescription = "SVSPP", filterByGroup = "all") {

####Remove length data if present
# data.table::setkey(prepped_data, CRUISE6, STRATUM, STATION, SVSPP)
# stratmeanData <- unique(prepped_data, by = key(prepped_data))
#stratmeanData[, c('LENGTH', 'NUMLEN') := NULL]

###Merge sex or keep separate
#if(mergesexFlag == F) stratmeanData[, group := paste(group, CATCHSEX, sep = '')]
# data.table::setkey(stratmeanData, CRUISE6, STRATUM, STATION)
stratmeanData[, BIOMASS   := sum(BIOMASS),   by = key(stratmeanData)]
stratmeanData[, ABUNDANCE := sum(ABUNDANCE), by = key(stratmeanData)]
stratmeanData <- unique(stratmeanData, by = key(stratmeanData))

#Calculate total number of stations per year
data.table::setkey(stratmeanData, STRATUM, YEAR) #group by
Nsurvdat <- unique(stratmeanData, by = key(stratmeanData)) #determine and remove duplicate rows 
Nsurvdat <- Nsurvdat[, sum(ntows), by = 'YEAR']
data.table::setnames(Nsurvdat, 'V1', 'N')

### Calculate weight per tow and number per tow #####
data.table::setkeyv(stratmeanData, c('group', 
                                     'strat'#, 
                                     #keyoff
))

stratmeanData[, biomass.tow   := sum(BIOMASS)   / ntows, by = key(stratmeanData)]
stratmeanData[, abundance.tow := sum(ABUNDANCE) / ntows, by = key(stratmeanData)]

### Calculated stratified means #####
stratmeanData[, weighted.biomass   := biomass.tow   * W.h]
stratmeanData[, weighted.abundance := abundance.tow * W.h]

### Variance - need to account for zero catch #####
#stratmeanData[, n.zero := ntows - length(BIOMASS), by = key(stratmeanData)]

stratmeanData[, zero.var.b := n.zero * (0 - biomass.tow)^2]
stratmeanData[, vari.b := (BIOMASS - biomass.tow)^2]
stratmeanData[, Sh.2.b := (zero.var.b + sum(vari.b)) / (ntows - 1), by = key(stratmeanData)]
stratmeanData[is.nan(Sh.2.b), Sh.2.b := 0]

stratmeanData[, zero.var.a := n.zero * (0 - abundance.tow)^2]
stratmeanData[, vari.a := (ABUNDANCE - abundance.tow)^2]
stratmeanData[, Sh.2.a := (zero.var.a + sum(vari.a)) / (ntows - 1), by = key(stratmeanData)]
stratmeanData[is.nan(Sh.2.a), Sh.2.a := 0]

stratmeanData <- unique(stratmeanData, by = key(stratmeanData))

stratmeanData <- merge(stratmeanData, N, by = keyoff)

#Stratified mean
data.table::setkeyv(stratmeanData, c('group', keyoff))

stratmeanData[, strat.biomass := sum(weighted.biomass),   by = key(stratmeanData)]
stratmeanData[, strat.abund   := sum(weighted.abundance), by = key(stratmeanData)]

#Stratified variance
if(poststratFlag == F){
  stratmeanData[, biomass.var := sum(((W.h^2) * Sh.2.b) / ntows), by = key(stratmeanData)]
  stratmeanData[, abund.var   := sum(((W.h^2) * Sh.2.a) / ntows), by = key(stratmeanData)]
}

if(poststratFlag == T){
  stratmeanData[, biomass.var := sum(Sh.2.b * W.h) / N + sum((1 - W.h) * Sh.2.b) / N^2, by = key(stratmeanData)]
  stratmeanData[, abund.var   := sum(Sh.2.a * W.h) / N + sum((1 - W.h) * Sh.2.a) / N^2, by = key(stratmeanData)]
  
}

#standard error of the means
stratmeanData[, biomass.SE := sqrt(biomass.var), by = key(stratmeanData)]
stratmeanData[, abund.SE   := sqrt(abund.var),   by = key(stratmeanData)]

#Delete extra rows/columns
stratmeanData <- unique(stratmeanData, by = key(stratmeanData))
stratmeanData <- stratmeanData[, list(YEAR, SEASON, group, CATCHSEX, N, strat.biomass,
                                      biomass.var, biomass.SE, strat.abund,
                                      abund.var, abund.SE)]

data.table::setnames(stratmeanData, 'group', groupDescription)

return(stratmeanData[])
}

trial <- adjust_stratmean(stratmeanData)

N <- stratmeanData %>% 
  group_by(YEAR) %>% 
  summarise(N = sum(ntows))
 #comparing data.table and tidy, data.table has far less because it removed duplicates which could include the 0s from our presence/absence tidying. Do we want these included 



## Tidy Strat Mean #####

### Calculate weight per tow and number per tow #####
stratmean_tidy <- prepped_data %>% 
  group_by(CRUISE6, STRATUM, STATION, SVSPP) %>% 
  summarise(BIOMASS = sum(BIOMASS), 
            ABUNDANCE = sum(ABUNDANCE), 
            ntows = sum(ntows), 
            W.h = W.h)



stratmean_tidy <- stratmean_tidy %>% 
  group_by(CRUISE6, STRATUM, STATION, SVSPP) %>% 
  mutate(biomass.tow   = sum(BIOMASS)   / ntows, 
         abundance.tow = sum(ABUNDANCE) / ntows)



### Calculated stratified means #####
stratmean_tidy <- stratmean_tidy %>% 
  group_by(CRUISE6, STRATUM, STATION, SVSPP) %>% 
  mutate(weighted.biomass   = biomass.tow   * W.h, 
         weighted.abundance = abundance.tow   * W.h)


### Variance - need to account for zero catch #####
# stratmean_tidy <- stratmean_tidy %>% 
#   group_by(CRUISE6, STRATUM, STATION, SVSPP) %>% 
#   mutate(n.zero   = ntows - length(BIOMASS), 
#          zero.var.b = n.zero * (0 - biomass.tow)^2,
#          vari.b = (BIOMASS - biomass.tow)^2, 
#          Sh.2.b = (zero.var.b + sum(vari.b)) / (ntows - 1))



# stratmeanData[, zero.var.a := n.zero * (0 - abundance.tow)^2]
# stratmeanData[, vari.a := (ABUNDANCE - abundance.tow)^2]
# stratmeanData[, Sh.2.a := (zero.var.a + sum(vari.a)) / (ntows - 1), by = key(stratmeanData)]
# stratmeanData[is.nan(Sh.2.a), Sh.2.a := 0]

stratmeanData <- unique(stratmeanData, by = key(stratmeanData))

stratmeanData <- merge(stratmeanData, N, by = keyoff)

#Stratified mean
data.table::setkeyv(stratmeanData, c('group', keyoff))

stratmeanData[, strat.biomass := sum(weighted.biomass),   by = key(stratmeanData)]
stratmeanData[, strat.abund   := sum(weighted.abundance), by = key(stratmeanData)]

stratmean_tidy <- stratmean_tidy %>% 
  group_by(CRUISE6, STRATUM, STATION, SVSPP) %>% 
  mutate(strat.biomass = sum(weighted.biomass), 
         strat.abund   = sum(weighted.abundance))

#Stratified variance
# if(poststratFlag == F){
#   stratmeanData[, biomass.var := sum(((W.h^2) * Sh.2.b) / ntows), by = key(stratmeanData)]
#   stratmeanData[, abund.var   := sum(((W.h^2) * Sh.2.a) / ntows), by = key(stratmeanData)]
# }
# 
# if(poststratFlag == T){
#   stratmeanData[, biomass.var := sum(Sh.2.b * W.h) / N + sum((1 - W.h) * Sh.2.b) / N^2, by = key(stratmeanData)]
#   stratmeanData[, abund.var   := sum(Sh.2.a * W.h) / N + sum((1 - W.h) * Sh.2.a) / N^2, by = key(stratmeanData)]
#   
# }

stratmean_tidy <- stratmean_tidy %>% 
  group_by(CRUISE6, STRATUM, STATION, SVSPP) %>% 
  mutate(biomass.var = sum(((W.h^2) * Sh.2.b) / ntows), 
         abund.var = sum(((W.h^2) * Sh.2.a) / ntows), 
         biomass.SE = sqrt(biomass.var), 
         abund.SE   = sqrt(abund.var))

#standard error of the means
# stratmeanData[, biomass.SE := sqrt(biomass.var), by = key(stratmeanData)]
# stratmeanData[, abund.SE   := sqrt(abund.var),   by = key(stratmeanData)]



