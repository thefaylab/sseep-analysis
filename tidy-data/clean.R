### created: 11/09/2022
### last updated: 

#### 01-TIDY-DATA ####

###################
#### OBJECTIVE ####
###################
# prepare and tidy data for further analyses; add areal codes (wind area vs outside area); add day vs night code; add a skate complex to the overall observations; expands the dataset to a complete dataset with present and absent values for each species and tow combination.

# awaiting new full raw dataset from Catherine; when received run and check code. Derived data to be used with retro analysis script here("retro-analysis") ####################

#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(sf)
library(here)
suppressPackageStartupMessages(library(tidyverse))

#### LOAD DATA ####
raw <- st_read(here("data", "gis", "[raw tow data]")) #update with raw tow data shapefile when received from Catherine Foley 
strata_shp <- st_read(here("data", "gis", "[strata data]")) #update
wind_shp <- st_read(here("data", "gis", "[wind shapefile]")) #update

###########################################
#### CALCULATE AREAS AND INTERSECTIONS ####
###########################################
# calculates the area of each strata polygon 
# area will be used later in stratified mean calculations
bstrat_area <- st_area(strata_shp)

# calculates the area of each strata polygon 
# area will be used later in stratified mean calculations
wind_area <- st_area(wind_shp)

# determines the area of the bottom trawl strata minus the wind areas
shp_intersect <- sf::st_disjoint(strata_shp, wind_shp)

# calculates the area of the remaining bottom trawl strata when wind areas are removed
intersect_area <- st_area(shp_intersect)

# join the strata area values as columns to the originial strata shapefile by unique strata
strata_shp <- dplyr::left_join(strata_shp, bstrat_area, by = STRATUM)

wind_shp <- dplyr::left_join(wind_shp, wind_area, by = STRATUM)

# save the new shapefiles and rds objects 
saveRDS(strata_shp, here(""))
saveRDS(wind_shp, here(""))
sf_save(strata_shp, here("gis", ))
sf_save(wind_shp, here("gis", ))

################################
#### CALCULATE OUTSIDE TOWS ####
################################

windtows <- intersect(raw, wind area) #find the fx that identifies points within a polygon and returns point data 

outsidetows <- remove(raw, windtows, by = ) #find function that removes obs by a unique id or otherwise 

# save the new shapefiles and rds objects
saveRDS(outsidetows, here(""))
saveRDS(windtows, here(""))
sf_save(windtows, here("gis", ))
sf_save(outsidetows, here("gis", ))


##############################
#### CREATE SKATE COMPLEX ####
##############################
# create a skate complex since skates are federally managed as a group

windskates <- windtows %>% 
  filter(SVSPP %in% c(22, 23, 24, 26))
  group_by(CRUISE6, STATION, SEASON,# replace list with unique cruise id value
           YEAR, STRATUM, SHG, TOGA, 
           EST_YEAR, EST_MONTH, EST_DAY, 
           EST_TIME, DECDEG_BEGLON, DECDEG_BEGLAT, 
           AVGDEPTH,) %>% 
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), EXPCATCHWT = sum(EXPCATCHWT)) %>% 
  mutate(TOWCT = as.integer(1),  # both sexes can be caught in the same tow
         PURPOSE_CODE = as.integer(10), #bottom trawl code
         STATUS_CODE = as.integer(10),
         SVVESSEL = "HB", #Henry Bigelow
         COMNAME = "SKATE COMPLEX", 
         SVSPP = as.integer(999))
  
outsideskates <- outsidetows %>% 
    filter(SVSPP %in% c(22, 23, 24, 26))
    group_by(CRUISE6, STATION, SEASON,# replace list with unique cruise id value
           YEAR, STRATUM, SHG, TOGA, 
           EST_YEAR, EST_MONTH, EST_DAY, 
           EST_TIME, DECDEG_BEGLON, DECDEG_BEGLAT, 
           AVGDEPTH,) %>% 
    summarise(EXPCATCHNUM = sum(EXPCATCHNUM), EXPCATCHWT = sum(EXPCATCHWT)) %>% 
    mutate(TOWCT = as.integer(1),  # both sexes can be caught in the same tow
           PURPOSE_CODE = as.integer(10), #bottom trawl code
           STATUS_CODE = as.integer(10),
           SVVESSEL = "HB", #Henry Bigelow
           COMNAME = "SKATE COMPLEX", 
           SVSPP = as.integer(999))
    
# save the new shapefiles and rds objects 
saveRDS(outsideskates, here(""))
saveRDS(windskates, here(""))
sf_save(windskates, here("gis", ))
sf_save(outsideskates, here("gis", ))



###########################
#### ADD SKATE COMPLEX ####
###########################
# add skate complex as an observation to the full datasets to be able to filter by individual skate species or the full managed skate complex. 
windtows <- bind_rows(windtows, windskates)

outsidetows <- bind_rows(outsidetows, outsideskates)

# save the new shapefiles and rds objects 
saveRDS(outsideskates, here(""))
saveRDS(windskates, here(""))
sf_save(windskates, here("gis", ))
sf_save(outsideskates, here("gis", ))


#########################################
#### ADD ADDITIONAL IDENTIFYING INFO ####
#########################################

# split estimated time into hours, minutes, and seconds data to characterize a tow as either a night or day tow based on hour values. 

### Wind ###
windtows[c('HOUR', 'MINUTE', 'SECONDS')] <- str_split_fixed(windtows$EST_TIME, ':', 3)
windtows$HOUR <- as.integer(windtows$HOUR)
windtows$DAYTIME <- paste(ifelse(windtows$HOUR <= 5 | windtows$HOUR >= 20, "NIGHT", "DAY"))


### Outside ###
outsidetows[c('HOUR', 'MINUTE', 'SECONDS')] <- str_split_fixed(outsidetows$EST_TIME, ':', 3)
outsidetows$HOUR <- as.integer(outsidetows$HOUR)
outsidetows$DAYTIME <- paste(ifelse(outsidetows$HOUR <= 5 | outsidetows$HOUR >= 20, "NIGHT", "DAY"))


# add area type (outside area vs wind area) and an associated code (wind ==1, outside == 2). 

### Outside ###
outsidetows$AREA <- paste("OUTSIDE")  
outsidetows$AREA_CODE <- paste(as.integer(2))

### Wind ###
windtows$AREA <- paste("WIND")
windtows$AREA_CODE <- paste(as.integer(1))

# save the new shapefiles and rds objects 
saveRDS(outsidetows, here(""))
saveRDS(windtows, here(""))
sf_save(windtows, here("gis", ))
sf_save(outsidetows, here("gis", ))


###############################
#### COMPLETE THE DATASETS ####
###############################
# expands the dataset to find missing values based on every combination of provided values. In this case, we will find every missing species in a given tow based on the full suite of species caught and create an observation of that tow and species combination where the "observed" numbers and weights are equal to 0. This will allow for a presence/absence analysis. 

### Wind ###
windtows <- windtows %>% 
  complete(ID, 
           SVSPP, 
           fill=list(EXPCATCHNUM = as.integer(0),   #fill with 0
                     EXPCATCHWT = as.integer(0),    #fill with 0
                     STATUS_CODE = as.integer(10),  #fill with 10
                     PURPOSE_CODE = as.integer(10), #fill with 10
                     TOWCT = as.integer(1)          #fill with 1 for summarising
           ),
           explicit = FALSE)

# save the new shapefiles and rds objects 
saveRDS(windtows, here(""))
sf_save(windtows, here("gis", ))


### Outside ###
outsidetows <- outsidetows %>% 
  complete(ID, 
           SVSPP, 
           fill=list(EXPCATCHNUM = as.integer(0),   #fill with 0
                     EXPCATCHWT = as.integer(0),    #fill with 0
                     STATUS_CODE = as.integer(10),  #fill with 10
                     PURPOSE_CODE = as.integer(10), #fill with 10
                     TOWCT = as.integer(1)          #fill with 1 for summarising
           ),
           explicit = FALSE)

# save the new shapefiles and rds objects 
saveRDS(outsidetows, here(""))
sf_save(outsidetows, here("gis", ))


#################################
#### BIND DATA SETS TOGETHER ####
#################################

merged_data <- bind_rows(outsidetows, windtows)


# save the new shapefiles and rds objects 
saveRDS(merged_data, here(""))
sf_save(merged_data, here("gis", ))


#########################################
#### CREATE PRESENCE/ABSENCE COLUMNS ####
#########################################
# Create a column for assigning an integer to presence/absence of a species based on EXPCATCHNUM
# 00 = ABSENT; 01 = PRESENT
merged_data$PRESENCE <- ifelse(merged_data$EXPCATCHNUM == 0, as.integer(0), as.integer(1))

# save the new shapefiles and rds objects 
saveRDS(merged_data, here(""))
sf_save(merged_data, here("gis", ))
