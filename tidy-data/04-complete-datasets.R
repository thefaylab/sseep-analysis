# COMPLETE DATASETS #####

### created:      10/24/2022
### last update:  11/22/2023
###

## OBJECTIVE ####

### Load Packages ####
library(ggplot2)
library(tidyverse)
library(here)
library(stringr)

here()

### Load Data ####
#wind <- readRDS(here("data", "rds", "tidy-wind.rds"))
data <- readRDS(file = here("data", "rds", "tidy-data", "tidy-full-bts.rds"))
bts <- readRDS(here("data", "rds", "tidy-data", "full-bts-indexed.rds"))
#outside <- readRDS(here("data", "rds", "tidy-outside.rds"))
#species <- read_csv(here("data", "clean-data", "species-v2.csv"))
species <- readRDS(here("data", "rds", "species.rds"))


## DATA SUMMARIES ####
# identify number of tows with NA depth values
na_bts <- bts |> filter(is.na(AVGDEPTH))
length(unique(na_bts$SVSPP))
length(unique(na_bts$TOWID))
na_bts  |> group_by(SEASON, YEAR) |> 
  summarise(uni_tow = length(unique(TOWID)))



###  identify number of unique tows 
# per season, year, and species 
bts |> group_by(SEASON, SVSPP, YEAR) |>
  summarise(uni_tow = length(unique(TOWID)))
bts |> dplyr::select(c(SVSPP, SEASON, TOWID, YEAR)) |>
  unique()

# per season, and year
bts  |> group_by(SEASON, YEAR) |> 
  summarise(uni_tow = length(unique(TOWID)))
bts |> dplyr::select(c(YEAR, SEASON)) |>
  unique()

# per season
bts  |> group_by(SEASON) |> 
  summarise(uni_tow = length(unique(TOWID)))
bts |> dplyr::select(c(SEASON, TOWID)) |>
  unique()

# per season, and species 
bts  |> group_by(SEASON, SVSPP) |> 
  summarise(uni_tow = length(unique(TOWID)))
length(unique(bts$YEAR))
length(unique(bts$SVSPP))
length(unique(bts$TOWID))

# per species 
bts |> dplyr::select(c(SVSPP, TOWID)) |>
  unique()

# per year
bts |> dplyr::select(c(YEAR, TOWID)) |>
  unique()


#####
## Create Unique Codes ###

### Season Code ###
# Create a column for assigning an integer to the season values
# 01 = FALL; 02 = SPRING
#outside$SEASON2 = ifelse(outside$SEASON=="FALL", as.integer(01), as.integer(02))
#wind$SEASON2 = ifelse(wind$SEASON=="FALL", as.integer(01), as.integer(02))


### Area Code ##
# Assign a character and integer as to whether it is wind or outside data 

#### Outside ##
#outside$AREA <- paste("OUTSIDE")  
#outside$AREA_CODE <- paste(as.integer(2))

#### Wind ##
#wind$AREA <- paste("WIND")
#wind$AREA_CODE <- paste(as.integer(1))


### Tow ID ##
# Create unique Tow ID column by joining together unique tow information

#### Outside ##
# outside$ID = str_c(outside$CRUISE6, outside$STATION, outside$STRATUM,
#                    outside$EST_MONTH, outside$EST_DAY, outside$YEAR, 
#                    outside$SEASON2, outside$HOUR, outside$MINUTE, 
#                    outside$SECONDS, outside$AREA_CODE, 
#                    sep = "-")

#### Wind ##
# wind$ID = str_c(wind$CRUISE6, wind$STATION, wind$STRATUM, wind$EST_MONTH,
#                 wind$EST_DAY, wind$YEAR, wind$SEASON2, wind$HOUR, wind$MINUTE,
#                 wind$SECONDS, wind$AREA_CODE, 
#                 sep = "-")



### Coordinates ##
# Creates a data frame using the longitude, latitude, and the ID 

#### Outside ###
# out_coords <- outside |>select(DECDEG_BEGLON, DECDEG_BEGLAT, ID) |>unique()
#coordso$LNL = stri_join(coordso$DECDEG_BEGLON, coordso$DECDEG_BEGLAT, sep = " : ")

#### Wind ##
# wind_coords <- wind |>select(DECDEG_BEGLON, DECDEG_BEGLAT, ID) |>unique()
# coordsw$LNL = stri_join(coordsw$DECDEG_BEGLON, coordsw$DECDEG_BEGLAT, sep = " : ")
# coords <- data |>dplyr::select(DECDEG_BEGLON, DECDEG_BEGLAT, TOWID) |>unique()
# 
# info <- data |>dplyr::select(DECDEG_BEGLON, DECDEG_BEGLAT, TOWID, CRUISE6, STATION, STRATUM, CODE, HOUR, MINUTE, SECONDS, ) |>unique()

  
## COMPLETE #####

# Binds two datasets together
#merged_data <- bind_rows(outside, wind)

# Write binded data to CSV
#write_csv(merged_data, here("data", "clean-data", "merged_present-only.csv"))
#saveRDS(merged_data, here("data", "rds", "merged_present-only.rds"))

# Complete data by using every possible combination of ID and SVSPP 
comp_data <- bts |>
  complete(TOWID, 
           SVSPP, 
           SEASON,
           fill=list(EXPCATCHNUM = as.integer(0),   #fill with 0
                     EXPCATCHWT = as.integer(0)#,    #fill with 0
                     #STATUS_CODE = as.integer(10),  #fill with 10
                     #PURPOSE_CODE = as.integer(10), #fill with 10
                     #TOWCT = as.integer(1)          #fill with 1 for summarising
           ),
           explicit = FALSE)

comp_data |> group_by(SEASON) |> summarise(uni_tow = length(unique(TOWID)))
comp_data |> group_by(SEASON, SVSPP) |> summarise(uni_tow = length(unique(TOWID)))
comp_data |> group_by(SEASON, YEAR) |> summarise(uni_tow = length(unique(TOWID)))

# extract the columns from the observed data to use to fill in NAs of same columns as result of complete()
info <- bts |> 
  dplyr::select(!c(SVSPP, COMNAME, SCINAME, LOGGED_SPECIES_NAME, EXPCATCHNUM, EXPCATCHWT, CATCHSEX)) |>
  unique()

# keep matching tow data to fill in NAs
comp_join <- inner_join(info, comp_data, by = c("TOWID", "SEASON"))

comp_join |> group_by(SEASON) |> summarise(uni_tow = length(unique(TOWID)))
comp_join |> group_by(SEASON, SVSPP) |> summarise(uni_tow = length(unique(TOWID)))
comp_join |> group_by(SEASON, YEAR.x) |> summarise(uni_tow = length(unique(TOWID)))

# extract column names that have .y at the end as a result of the inner join
columns <- str_subset(names(comp_join), ".y")

# remove the columns with the extracted column names 
comp_join <- comp_join |> 
  select(!all_of(columns))

# add in the common name based on the species code 
comp_join <- left_join(comp_join, species, by = "SVSPP") |> 
  select(!c(COMNAME.x, SCINAME.x)) |> 
  rename(COMNAME = COMNAME.y,
         SCINAME = SCINAME.y)

# extract the column names that end in .x as a result of the inner join and remove .x from the name string
rename <- str_remove(names(comp_join), ".x")

# rename all the columns that have .x at the end using the rename object above
names(comp_join) <- rename

# test inner join junction to add all additional info
# nas <- comp_data |> 
#   filter(is.na(CODE))
# test <- nas |> 
#   inner_join(info, by = "TOWID")
# test <- full_join(comp_data, info, by = "TOWID")

#complete_dat <- inner_join(bts, comp_data, by = "TOWID")

# mutate to fill all NA's with 0 for weight, number and catch sex, and add a presence column based on observed values of expcatchnum  
comp_join <- comp_join |>
  mutate(EXPCATCHNUM = ifelse(is.na(EXPCATCHNUM), as.integer(0), EXPCATCHNUM),
         EXPCATCHWT = ifelse(is.na(EXPCATCHWT), as.integer(0), EXPCATCHWT),
         PRESENCE = ifelse(EXPCATCHNUM == 0, as.integer(0), as.integer(1)), 
         CATCHSEX = ifelse(is.na(CATCHSEX), as.integer(0), CATCHSEX))

# identify number of tows with NA depth values
na_comp <- comp_join |> filter(is.na(AVGDEPTH))
length(unique(na_bts$SVSPP))
length(unique(na_bts$TOWID))
na_bts  |> group_by(SEASON, YEAR) |> 
  summarise(uni_tow = length(unique(TOWID)))

# 
# comp_join <- left_join(comp_join, species, by = "SVSPP") |> 
#   select(!c(COMNAME.x, SCINAME.x)) |> 
#   rename(COMNAME = COMNAME.y, 
#          SCINAME = SCINAME.y)

#####
### Fill/Create Columns ##

#### Presence/Absence ##
# Create a column for assigning an integer to presence/absence of a species based on EXPCATCHNUM
# 00 = ABSENT; 01 = PRESENT
#comp_join$PRESENCE <- ifelse(comp_join$EXPCATCHNUM == 0, as.integer(0), as.integer(1))

#### Break up ID column ###
# com_data <- merged_data |>
#   separate(ID, 
#            into = c("CRUISE6", "STATION", "STRATUM","EST_MONTH", "EST_DAY",
#                     "EST_YEAR", "SEASON2","HOUR", "MINUTE", "SECONDS", 
#                     "AREA_CODE"),
#            sep="-", 
#            remove=FALSE, 
#            convert=TRUE)

# Recreate the AREA / AREA_CODE columns
# merged_data$AREA = ifelse(merged_data$AREA_CODE==1, "WIND", "OUTSIDE")
# merged_data$AREA_CODE = ifelse(merged_data$AREA=="WIND", 1, 2)
# 
# # Recreate the SEASON column
# merged_data$SEASON = ifelse(merged_data$SEASON2==1, "FALL", "SPRING")


# Create coords data frame like earlier but for the merged frame 
# coords <- merged_data |>
#   select(DECDEG_BEGLON, DECDEG_BEGLAT, ID) %>%
#   unique() |>
#   drop_na()
# 
# 
# # Merge coords and species back into data
# merged_data <- inner_join(x=merged_data, y=species, by="SVSPP", keep = TRUE)
# merged_data <- inner_join(x=merged_data, y=coords, by="ID", keep = TRUE)
# 
# # Clean columns in merged dataframe
# merged_data <- merged_data |>
#   select(-c(COMNAME.x, SVSPP.x, SHG, OBJECTID, TOGA,
#             YEAR, AVGDEPTH, LOGGED_SPECIES_NAME, ID.y,
#             CATCHSEX, SCINAME.x, EST_TIME, DECDEG_BEGLON.x, 
#             DECDEG_BEGLAT.x, x, y, CODE, SVVESSEL)) |>  
#   dplyr::rename(ID = ID.x,
#                 SVSPP = SVSPP.y,
#                 SCINAME = SCINAME.y,
#                 COMNAME = COMNAME.y,
#                 DECDEG_BEGLON = DECDEG_BEGLON.y,
#                 DECDEG_BEGLAT = DECDEG_BEGLAT.y)
# 
# 
# # Recreate EST_TIME
# merged_data$EST_TIME <- str_c(merged_data$HOUR, merged_data$MINUTE, merged_data$SECONDS, sep = ":")
# 
# # Recreate DAYTIME
# merged_data$DAYTIME <- paste(ifelse(merged_data$HOUR <= 5 | merged_data$HOUR >= 20, "Night", "Day"))


#yearstrat = split(merged_data, merged_data$EST_YEAR) 
#^stratifies the data by year

# lapply(names(merged_data), function(x){
#   write.csv(merged_data[[x]], paste(x, "_mergePA.csv", sep = ""), row.names = FALSE)
# })

### SAVE THE DATA ####
write.csv(comp_join, here("data", "clean-data", "complete_data_clean.csv"))
saveRDS(comp_join, here("data", "rds", "completed_bts_data.rds"))

