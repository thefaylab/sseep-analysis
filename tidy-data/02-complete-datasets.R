# COMPLETE DATASETS #####

### created:      10/24/2022
### last update:  
###

### Load Packages ###
library(ggplot2)
library(tidyverse)
library(here)
library(stringr)

here()

### Load Data ###
wind <- read_csv(here("data", "clean-data", "tidy-wind.csv"))
bts <- read_csv(here("data", "clean-data", "tidy-bts.csv"))
outside <- read_csv(here("data", "clean-data", "tidy-outside.csv"))
species <- read_csv(here("data", "clean-data", "species-v2.csv"))

## Create Unique Codes #####

### Season Code ######
# Create a column for assigning an integer to the season values
# 01 = FALL; 02 = SPRING
outside$SEASON2 = ifelse(outside$SEASON=="FALL", as.integer(01), as.integer(02))
wind$SEASON2 = ifelse(wind$SEASON=="FALL", as.integer(01), as.integer(02))


### Area Code #####
# Assign a character and integer as to whether it is wind or outside data 

#### Outside #####
outside$AREA <- paste("OUTSIDE")  
outside$AREA_CODE <- paste(as.integer(2))

#### Wind #####
wind$AREA <- paste("WIND")
wind$AREA_CODE <- paste(as.integer(1))


### Tow ID #####
# Create unique Tow ID column by joining together unique tow information

#### Outside #####
outside$ID = str_c(outside$CRUISE6, outside$STATION, outside$STRATUM,
                   outside$EST_MONTH, outside$EST_DAY, outside$YEAR, 
                   outside$SEASON2, outside$HOUR, outside$MINUTE, 
                   outside$SECONDS, outside$AREA_CODE, 
                   sep = "-")

#### Wind #####
wind$ID = str_c(wind$CRUISE6, wind$STATION, wind$STRATUM, wind$EST_MONTH,
                wind$EST_DAY, wind$YEAR, wind$SEASON2, wind$HOUR, wind$MINUTE,
                wind$SECONDS, wind$AREA_CODE, 
                sep = "-")



### Coordinates #####
# Creates a data frame using the longitude, latitude, and the ID 

#### Outside #####
out_coords <- outside %>% select(DECDEG_BEGLON, DECDEG_BEGLAT, ID) %>% unique()
#coordso$LNL = stri_join(coordso$DECDEG_BEGLON, coordso$DECDEG_BEGLAT, sep = " : ")

#### Wind #####
wind_coords <- wind %>% select(DECDEG_BEGLON, DECDEG_BEGLAT, ID) %>% unique()
# coordsw$LNL = stri_join(coordsw$DECDEG_BEGLON, coordsw$DECDEG_BEGLAT, sep = " : ")


  
## Compete #####

# Binds two datasets together
merged_data <- bind_rows(outside, wind)

# Write binded data to CSV
write_csv(merged_data, here("data", "clean-data", "mergedpresence.csv"))

# Complete data by using every possible combination of ID and SVSPP 
merged_data <- merged_data %>% 
  complete(ID, 
           SVSPP, 
           fill=list(EXPCATCHNUM = as.integer(0),   #fill with 0
                     EXPCATCHWT = as.integer(0),    #fill with 0
                     STATUS_CODE = as.integer(10),  #fill with 10
                     PURPOSE_CODE = as.integer(10), #fill with 10
                     TOWCT = as.integer(1)          #fill with 1 for summarising
                     ),
           explicit = FALSE)


### Fill/Create Columns #####

#### Presence/Absence #####
# Create a column for assigning an integer to presence/absence of a species based on EXPCATCHNUM
# 00 = ABSENT; 01 = PRESENT
merged_data$PRESENCE <- ifelse(merged_data$EXPCATCHNUM == 0, as.integer(0), as.integer(1))

#### Break up ID column ##### 
merged_data <- merged_data %>% 
  separate(ID, 
           into = c("CRUISE6", "STATION", "STRATUM","EST_MONTH", "EST_DAY",
                    "EST_YEAR", "SEASON2","HOUR", "MINUTE", "SECONDS", 
                    "AREA_CODE"),
           sep="-", 
           remove=FALSE, 
           convert=TRUE)

# Recreate the AREA / AREA_CODE columns
merged_data$AREA = ifelse(merged_data$AREA_CODE==1, "WIND", "OUTSIDE")
merged_data$AREA_CODE = ifelse(merged_data$AREA=="WIND", 1, 2)

# Recreate the SEASON column
merged_data$SEASON = ifelse(merged_data$SEASON2==1, "FALL", "SPRING")


# Create coords data frame like earlier but for the merged frame 
coords <- merged_data %>% 
  select(DECDEG_BEGLON, DECDEG_BEGLAT, ID) %>%
  unique() %>% 
  drop_na()


# Merge coords and species back into data
merged_data <- inner_join(x=merged_data, y=species, by="SVSPP", keep = TRUE)
merged_data <- inner_join(x=merged_data, y=coords, by="ID", keep = TRUE)

# Clean columns in merged dataframe
merged_data <- merged_data %>% 
  select(-c(COMNAME.x, SVSPP.x, SHG, OBJECTID, TOGA,
            YEAR, AVGDEPTH, LOGGED_SPECIES_NAME, ID.y,
            CATCHSEX, SCINAME.x, EST_TIME, DECDEG_BEGLON.x, 
            DECDEG_BEGLAT.x, x, y, CODE, SVVESSEL)) %>%   
  dplyr::rename(ID = ID.x,
                SVSPP = SVSPP.y,
                SCINAME = SCINAME.y,
                COMNAME = COMNAME.y,
                DECDEG_BEGLON = DECDEG_BEGLON.y,
                DECDEG_BEGLAT = DECDEG_BEGLAT.y)


# Recreate EST_TIME
merged_data$EST_TIME <- str_c(merged_data$HOUR, merged_data$MINUTE, merged_data$SECONDS, sep = ":")

# Recreate DAYTIME
merged_data$DAYTIME <- paste(ifelse(merged_data$HOUR <= 5 | merged_data$HOUR >= 20, "Night", "Day"))


#yearstrat = split(merged_data, merged_data$EST_YEAR) 
#^stratifies the data by year

# lapply(names(merged_data), function(x){
#   write.csv(merged_data[[x]], paste(x, "_mergePA.csv", sep = ""), row.names = FALSE)
# })

write.csv(merged_data, here("data", "clean-data", "merged_data_clean.csv"))

