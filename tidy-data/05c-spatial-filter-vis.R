### created: 02/20/2023
### last updated: 07/19/2023

# 05c - PLOT FILTERED DATASETS ####

## OBJECTIVE ####
# create seasonal distribution plots based on the 95% and 99% cumulative distribution for each species 

# script will: 
# filter the cumulative distribution datasets based on the species code 
# create seasonal plots for the filtered species that depict the  the proportion of biomass observed in a given strata makes up of the total biomass observed
# save the plots in a specified folder

### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


### LOAD DATA ####
# coastline shapefile created from `extract-usa-data.R` here("tidy-data")
coastline <- sf::st_read(dsn = here("gis", "east_coast.shp"))

# 95% cumulative distribution dataset created from `05a-spatial-filter.R` here("tidy-data"). 
filter95 <- readRDS(here("data", "rds", "spatial-filter", "cumul-biomass95.rds")) 

# 99% cumulative distribution dataset created from `05b-spatial-filter.R` here("tidy-data"). 
filter99 <- readRDS(here("data", "rds", "spatial-filter", "cumul-biomass99.rds")) 

# read the strata shapefile in to plot the polygons based on proportion and overlay the tow points
strata <-  readRDS(here("data", "rds", "active_strata.rds"))

### DATA WRANGLE ####
specieslookup95 <- filter95 |> 
  ungroup() |>
  select(SVSPP, COMNAME) |> 
  distinct() |> 
  mutate(spname = str_to_lower(gsub(" ", "_", COMNAME)))|> 
  arrange(SVSPP)

specieslookup99 <- filter99 |> 
  ungroup() |>
  select(SVSPP, COMNAME) |> 
  distinct() |> 
  mutate(spname = str_to_lower(gsub(" ", "_", COMNAME)))|> 
  arrange(SVSPP)


## PLOT FILTERED DATASET ####

##### TEST JOIN AND PLOT###
# test_join <- filter95 |> 
#   filter(SVSPP == 141) |> # filter the cumulative distribution dataframe for one species, in this case black sea bass
#   right_join(strata, by = "STRATUM") |> # join the filtered data to the strata shapefile; coearces the shapefile into a dataframe 
#   na.omit() |> # remove NAs
#   st_sf() # Coerce the dataframe back to an sf for plotting based on geometry; function help - "Create sf, which extends data.frame-like objects with a simple feature list column"
# 
# 
# # plot the strat shapefile and tow data on the same map
# ggplot() +
#   geom_sf(data = strata, fill = "white", color = "grey80") + 
#   geom_sf(data = coastline, fill = "grey60", color = "black") + 
#   geom_sf(data = test_join, aes(fill = csum_prop))+ #fill the strata polygons based on the values in the csum_prop column
#   scale_fill_distiller(palette = 8) +
#   coord_sf() +
#   #geom_point(data = data95 |> filter(SVSPP == 141), mapping = aes(DECDEG_BEGLON, DECDEG_BEGLAT, alpha = 0.4), color = "white") + 
#   facet_wrap(~SEASON) +
#   theme_bw() + 
#   theme(panel.grid = element_blank())


### 95 Cumulative Distribution ####
# create empty storage object
filtered_plots95 <- list()

# create species vector to loop over 
species95 <- unique(filter95$SVSPP)

#### PLOT LOOP ####
for(i in species95){ # for each value i in the species vector
  
  filtered_dist <- filter95 |> 
    filter(SVSPP == i) |> # filter the dataset where the species code is the value of i
    right_join(strata, by = "STRATUM") |> # join the filtered data to the strata shapefile; coearces the shapefile into a dataframe 
    na.omit() |> # remove NAs
    st_sf() # coerce to simple feature 
  
  filtered_plots95[[i]] <- ggplot() + 
    geom_sf(data = strata, fill = "white", color = "grey80") + 
    geom_sf(data = coastline, fill = "grey60", color = "black") + 
    geom_sf(data = filtered_dist, aes(fill = csum_prop))+ #fill the strata polygons based on the values in the csum_prop column
    scale_fill_distiller(palette = 8) +
    coord_sf() +
    #geom_point(data = x, mapping = aes(DECDEG_BEGLON, DECDEG_BEGLAT, alpha = 0.4), color = "white")
    facet_wrap(~SEASON) + 
    labs(title = str_to_title(filtered_dist$COMNAME), x = "Longitude", y = "Latitude", fill = "Proportion") +
    theme_bw() +
    theme(legend.position = "bottom", 
          panel.grid = element_blank()) 
}

filtered_plots95 <- compact(filtered_plots95) # remove NULL list items

#### PRINT AND SAVE LOOP ####
for(i in seq_along(species95)){ # move along the sequence of values in the species vector
  
  # save the printed plot with the species name based on the list value generated
  
  ggsave(filename = paste0(specieslookup95$spname[i], ".png"), device = "png" , plot = filtered_plots95[[i]], path = here("outputs", "plots", "95-dist-maps"), width = 8, height = 5)
  
}

### 99 Cumulative Distribution ####
#### PLOT LOOP ####
# create empty storage object
filtered_plots99 <- list()

# create species vector to loop over 
species99 <- unique(filter99$SVSPP)

for(i in species99){ # for each value i in the species vector
  
  filtered_dist <- filter99 |> 
    filter(SVSPP == i) |> # filter the dataset where the species code is the value of i
    right_join(strata, by = "STRATUM") |> # join the filtered data to the strata shapefile; coearces the shapefile into a dataframe 
    na.omit() |> # remove NAs
    st_sf() # coerce to simple feature 
  
  filtered_plots99[[i]] <- ggplot() + 
    geom_sf(data = strata, fill = "white", color = "grey80") + 
    geom_sf(data = coastline, fill = "grey60", color = "black") + 
    geom_sf(data = filtered_dist, aes(fill = csum_prop))+ #fill the strata polygons based on the values in the csum_prop column
    scale_fill_distiller(palette = 8) +
    coord_sf() +
    #geom_point(data = x, mapping = aes(DECDEG_BEGLON, DECDEG_BEGLAT, alpha = 0.4), color = "white")
    facet_wrap(~SEASON) + 
    labs(title = str_to_title(filtered_dist$COMNAME), x = "Longitude", y = "Latitude", fill = "Proportion") +
    theme_bw() +
    theme(legend.position = "bottom", 
          panel.grid = element_blank()) 
}

filtered_plots99 <- compact(filtered_plots99) # remove NULL list items

#### PRINT AND SAVE LOOP ####
for(i in seq_along(species99)){ # move along the sequence of values in the species vector
  
  # save the printed plot with the species name based on the list value generated
  
  ggsave(filename = paste0(specieslookup99$spname[i], ".png"), device = "png" , plot = filtered_plots99[[i]], path = here("outputs", "plots", "99-dist-maps"), width = 8, height = 5)
  
}
