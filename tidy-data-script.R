### created:      2/15/2022
### last update:  7/20/2022
###

library(stringr)
library(patchwork)
library(here)
library(tidyverse)

##### DATA SET UP #####

###### General ######

### read and prepare datasets ###

# A new full catch dataset provided by Catherine Foley on Feb 10, 2022. 
# The dataset was loaded into GIS in order to identify tows that were within
# the wind farm areas, thus creating a new wind dataset. 
here()
wind <- read.csv(here("data", "raw-data", "windareatows_07112022.csv"))
bts <- read.csv(here("data", "raw-data", "SSEEP_EXPORT_10FEB2022.csv"))
outside <- read.csv(here("data", "raw-data", "Outside_WindTows07122022.csv"))
species <- read.csv(here("data", "raw-data", "species.csv"))

# join full dataset with common and scientific names
species <- species %>% 
  select(-c(X)) %>% 
  rename(SVSPP = species, 
         SCINAME = scientific_name, 
         COMNAME = common_name)
bts <- full_join(bts, species, by = "SVSPP")

# create species reference dataframe #
species <- species %>%
  group_by(SVSPP, SCINAME, COMNAME) %>%
  filter(SVSPP %in% c(121, 131, 141, 32, 22, 23, 24, 26, 15, 72, 503, 105, 106)) %>%
  select(SVSPP, SCINAME, COMNAME) %>% distinct()



# create SEX reference dataframe #
# n.sex <- enframe(c(0, 1, 2))
# n.sex$value <- as.integer(n.sex$value)
# sex <- enframe(c(" ", "MALE", "FEMALE"))
# sex <- merge(x = n.sex, y = sex, by = "name")
# sex <- sex %>% select(-c(name)) %>% 
#   rename(CATCHSEX = value.x, SEX = value.y)

##### TIDY DATA BY AREA #####

###### Wind #####
# filter for species of interest
wind <- wind %>%
  group_by(SVSPP, COMNAME) %>%
  filter(SVSPP %in% c(121, 131, 141, 32, 22, 23, 24, 26, 15, 72, 503, 105, 106)) %>%
  select(-c(ï..OBJECTID))

#add tow count to track number of tows through aggregation
wind$TOWCT <- as.integer(paste(1))

# pull out unique strata in wind area dataset indicating potentially impacted strata ###
strata <- unique(wind$STRATUM)

#split the time into three parts to make easy query and convert character vector to integer 
wind[c('HOUR', 'MINUTE', 'SECONDS')] <- str_split_fixed(wind$EST_TIME, ':', 3)
wind$HOUR <- as.integer(wind$HOUR)
# classify time of day based on HOUR values 
wind$DAYTIME <- paste(ifelse(wind$HOUR <= 5 | wind$HOUR >= 20, "Night", "Day"))
# add unique code 
wind$CODE <- paste(wind$YEAR, wind$SVSPP, wind$SEASON, wind$STRATUM, wind$DAYTIME)
# add SEX column
# wind <- full_join(x = wind, y = sex, by = "CATCHSEX", all.x = TRUE)


###### All Strata ######
# filter for species of interest 
bts <- bts %>%
  group_by(SVSPP, COMNAME) %>%
  filter(SVSPP %in% c(121, 131, 141, 32, 22, 23, 24, 26, 15, 72, 503, 105, 106))

#add tow count to track number of tows through aggregation
bts$TOWCT <- as.integer(paste(1))

#split the time into three parts to make easy query and convert character vector to integer 
bts[c('HOUR', 'MINUTE', 'SECONDS')] <- str_split_fixed(bts$EST_TIME, ':', 3)
bts$HOUR <- as.integer(bts$HOUR)
# classify time of day based on HOUR values 
bts$DAYTIME <- paste(ifelse(bts$HOUR <= 5 | bts$HOUR >= 20, "Night", "Day"))
# paste in unique codes
bts$CODE <- paste(bts$YEAR, bts$SVSPP, bts$SEASON, bts$STRATUM, bts$DAYTIME)
# add SEX column
# bts <- full_join(x = bts, y = sex, by = "CATCHSEX", all.x = TRUE)

###### Overlapped Strata  ######
# filter for species of interest and tows overlap strata overlapped by wind
overlap <- bts %>%
  group_by(SVSPP, COMNAME) %>%
  filter(SVSPP %in% c(121, 131, 141, 32, 22, 23, 24, 26, 15, 72, 503, 105, 106), 
         STRATUM %in% strata)



##### AGGREGATE SPECIES ######

###### Spiny Dogfish (By Sex) #####
### WIND ###

# female spiny dogfish
sd_wind <- wind %>% group_by(SVSPP, COMNAME) %>%
  filter(SVSPP == 15)
sd_wind <- sd_wind %>% 
  group_by(CRUISE6, STATION, SEASON, 
           YEAR, STRATUM, SHG, TOGA, 
           EST_YEAR, EST_MONTH, EST_DAY, 
           EST_TIME, DECDEG_BEGLON, DECDEG_BEGLAT, 
           AVGDEPTH, HOUR, MINUTE, SECONDS, DAYTIME, 
           SVSPP, SCINAME, COMNAME) %>% 
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), EXPCATCHWT = sum(EXPCATCHWT), TOWCT = sum(TOWCT))
sd_wind$CATCHSEX <- as.integer(paste(3))
sd_wind$TOWCT <- as.integer(paste(1))
sd_wind$PURPOSE_CODE <- as.integer(paste(10))
sd_wind$STATUS_CODE <- as.integer(paste(10))
sd_wind$SVVESSEL <- paste("HB")
sd_wind$CODE <- paste(sd_wind$YEAR, sd_wind$SVSPP, sd_wind$SEASON, sd_wind$STRATUM, sd_wind$DAYTIME)

# Remove observations of Spiny Dogfish by Sex and Replace with aggregated Spiny Dogfish dataframe

wind <- wind %>%
  subset(!(SVSPP == 15))
wind <- rbind(wind, sd_wind)

### ALL OVERLAPPED STRATA ### 
sd_bts <- bts %>% group_by(SVSPP, COMNAME) %>%
  filter(SVSPP == 15)
sd_bts <- sd_bts %>% 
  group_by(CRUISE6, STATION, SEASON, 
           YEAR, STRATUM, SHG, TOGA, 
           EST_YEAR, EST_MONTH, EST_DAY, 
           EST_TIME, DECDEG_BEGLON, DECDEG_BEGLAT, 
           AVGDEPTH, HOUR, MINUTE, SECONDS, DAYTIME, 
           SVSPP, SCINAME, COMNAME) %>% 
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), EXPCATCHWT = sum(EXPCATCHWT), TOWCT = sum(TOWCT))
#sd_bts$SEX <- paste("BOTH")
sd_bts$CATCHSEX <- as.integer(paste(3))
sd_bts$TOWCT <- as.integer(paste(1))
sd_bts$PURPOSE_CODE <- as.integer(paste(10))
sd_bts$STATUS_CODE <- as.integer(paste(10))
sd_bts$SVVESSEL <- paste("HB")
sd_bts$CODE <- paste(sd_bts$YEAR, sd_bts$SVSPP, sd_bts$SEASON, sd_bts$STRATUM, sd_bts$DAYTIME)

# Remove observations of Spiny Dogfish by Sex and Replace with aggregated Spiny Dogfish dataframe

# bts <- full_join(x = bts, y = sex, by = "CATCHSEX", all.x = TRUE)

bts <- bts %>%
  subset(!(SVSPP == 15))

bts <- rbind(bts, sd_bts)

# count strata
count_all_strat <-  bts %>%
  group_by(STRATUM) %>%
  select(STRATUM) %>% 
  distinct() 
#19 obs. in bts 
#19 obs in wind 
#19 obs in overlap

### overlap STRATA ### 
sd_overlap <- overlap %>% 
  group_by(SVSPP, COMNAME) %>%
  filter(SVSPP == 15)
sd_overlap <- sd_overlap %>% 
  group_by(CRUISE6, STATION, SEASON, 
           YEAR, STRATUM, SHG, TOGA, 
           EST_YEAR, EST_MONTH, EST_DAY, 
           EST_TIME, DECDEG_BEGLON, DECDEG_BEGLAT, 
           AVGDEPTH, HOUR, MINUTE, SECONDS, DAYTIME, 
           SVSPP, SCINAME, COMNAME) %>% 
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), EXPCATCHWT = sum(EXPCATCHWT), TOWCT = sum(TOWCT))

sd_overlap$CATCHSEX <- as.integer(paste(3))
sd_overlap$TOWCT <- as.integer(paste(1))
sd_overlap$PURPOSE_CODE <- as.integer(paste(10))
sd_overlap$STATUS_CODE <- as.integer(paste(10))
sd_overlap$SVVESSEL <- paste("HB")
sd_overlap$CODE <- paste(sd_overlap$YEAR, sd_overlap$SVSPP, sd_overlap$SEASON, sd_overlap$STRATUM, sd_overlap$DAYTIME)

# Remove observations of Spiny Dogfish by Sex and Replace with aggregated Spiny Dogfish dataframe
# bts_overlap <- full_join(x = bts_overlap, y = sex, by = "CATCHSEX", all.x = TRUE)

overlap <- overlap %>%
  subset(!(SVSPP == 15))

overlap <- rbind(overlap, sd_overlap)

###### Skates ######

### WIND ###
sk_wind <- wind %>% 
  filter(SVSPP %in% c(22, 23, 24, 26)) 
sk_wind <- sk_wind %>% 
  group_by(CRUISE6, STATION, SEASON, 
           YEAR, STRATUM, SHG, TOGA, 
           EST_YEAR, EST_MONTH, EST_DAY, 
           EST_TIME, DECDEG_BEGLON, DECDEG_BEGLAT, 
           AVGDEPTH, HOUR, MINUTE, SECONDS, DAYTIME) %>%
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), EXPCATCHWT = sum(EXPCATCHWT), TOWCT = sum(TOWCT))

#revisit to make dataframe of lines 209-218 and join to sk_wind, repeat for remaingin areas 
sk_wind$CATCHSEX <- as.integer(paste(0))
sk_wind$TOWCT <- as.integer(paste(1))
sk_wind$PURPOSE_CODE <- as.integer(paste(10))
sk_wind$STATUS_CODE <- as.integer(paste(10))
sk_wind$SVVESSEL <- paste("HB")
sk_wind$SVSPP <- as.integer(paste(999))
sk_wind$COMNAME <- paste("ALL SKATES")
sk_wind$SCINAME <- paste(" ")
sk_wind$LOGGED_SPECIES_NAME <- paste(" ")
sk_wind$CODE <- paste(sk_wind$YEAR, sk_wind$SVSPP, sk_wind$SEASON, sk_wind$STRATUM, sk_wind$DAYTIME)

# Remove observations of Skates by SVSPPP and Replace with aggregated Skate dataframe
wind <- wind %>%
  subset(!(SVSPP %in% c(22, 23, 24, 26)))
wind <- rbind(wind, sk_wind)

### BTS ###
sk_bts <- bts %>% 
  filter(SVSPP %in% c(22, 23, 24, 26)) 
sk_bts <- sk_bts %>% 
  group_by(CRUISE6, STATION, SEASON, 
           YEAR, STRATUM, SHG, TOGA, 
           EST_YEAR, EST_MONTH, EST_DAY, 
           EST_TIME, DECDEG_BEGLON, DECDEG_BEGLAT, 
           AVGDEPTH, HOUR, MINUTE, SECONDS, DAYTIME) %>%
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), EXPCATCHWT = sum(EXPCATCHWT), TOWCT = sum(TOWCT))
# sk_bts$SEX <- paste(" ")
sk_bts$CATCHSEX <- as.integer(paste(0))
sk_bts$TOWCT <- as.integer(paste(1))
sk_bts$PURPOSE_CODE <- as.integer(paste(10))
sk_bts$STATUS_CODE <- as.integer(paste(10))
sk_bts$SVVESSEL <- paste("HB")
sk_bts$SVSPP <- as.integer(paste(999))
sk_bts$COMNAME <- paste("ALL SKATES")
sk_bts$SCINAME <- paste(" ")
sk_bts$LOGGED_SPECIES_NAME <- paste(" ")
sk_bts$CODE <- paste(sk_bts$YEAR, sk_bts$SVSPP, sk_bts$SEASON, sk_bts$STRATUM, sk_bts$DAYTIME)

# Remove observations of Skates by SVSPPP and Replace with aggregated Skate dataframe
bts <- bts %>%
  subset(!(SVSPP %in% c(22, 23, 24, 26)))
bts <- rbind(bts, sk_bts)

### OVERLAP ###
sk_overlap <- overlap %>% 
  filter(SVSPP %in% c(22, 23, 24, 26)) 
sk_overlap <- sk_overlap %>% 
  group_by(CRUISE6, STATION, SEASON, 
           YEAR, STRATUM, SHG, TOGA, 
           EST_YEAR, EST_MONTH, EST_DAY, 
           EST_TIME, DECDEG_BEGLON, DECDEG_BEGLAT, 
           AVGDEPTH, HOUR, MINUTE, SECONDS, DAYTIME) %>%
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), EXPCATCHWT = sum(EXPCATCHWT), TOWCT = sum(TOWCT)) # revisit to see number of species in a given tow? 

sk_overlap$CATCHSEX <- as.integer(paste(0))
sk_overlap$TOWCT <- as.integer(paste(1))
sk_overlap$PURPOSE_CODE <- as.integer(paste(10))
sk_overlap$STATUS_CODE <- as.integer(paste(10))
sk_overlap$SVVESSEL <- paste("HB")
sk_overlap$SVSPP <- as.integer(paste(999))
sk_overlap$COMNAME <- paste("ALL SKATES")
sk_overlap$SCINAME <- paste(" ")
sk_overlap$LOGGED_SPECIES_NAME <- paste(" ")
sk_overlap$CODE <- paste(sk_overlap$YEAR, sk_overlap$SVSPP, sk_overlap$SEASON, sk_overlap$STRATUM, sk_overlap$DAYTIME)

# Remove observations of All Skates and Replace with aggregated Skates dataframe

overlap <- overlap %>%
  subset(!(SVSPP %in% c(22, 23, 24, 26)))

overlap <- rbind(overlap, sk_overlap)





species2 <- species %>%
  subset(!(SVSPP %in% c(22, 23, 24, 26)))
skates <- data.frame(as.integer(999), "ALL SKATES") %>% 
  rename(SVSPP = as.integer.999.,
         COMNAME = X.ALL.SKATES.)
species2 <- rbind(species2, skates)
write.csv(species2, "C:/Users/amiller7/Documents/cinar-osse/wind-analysis/data/summary-stats/species-v2.csv", row.names=FALSE)


#### EXPORT ####