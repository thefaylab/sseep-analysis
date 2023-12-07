### created: 12/1/2023
### updated: 

# 02b - CREATE SUMMER FLOUNDER SPATIAL FOOTPRINT: SPRING ####

## OBJECTIVE ####
# identify the sampling frame for spring observations of summer flounder 
# begin with the spatial footprint created by the 95% cumulative distribution of biomass 
# append with additional strata that are inshore of the mid-Atlantic and capture the full extent of the offshore wind energy areas. 

### LOAD PACKAGES ####
library(here)
library(sf)
library(tidyverse)
library(nationalparkcolors)

### LOAD DATA ####
# active survey strata 
strata <- readRDS(here("data", "rds", "active_strata.rds")) 

# 95% cumulative distribution dataset created from `05a-spatial-filter.R` here("tidy-data").
csum_dist95 <- readRDS(here("data", "rds", "spatial-filter", "cumul-biomass95.rds")) %>% filter(SVSPP == 103)

# read in the northeast coastline
ne_coast <- sf::st_read(dsn = here("gis", "eastern_coast_WGS84.shp"))

# color palette 
pal <- park_palette("Badlands")


## EXPLORE FOOTPRINTS ####
# plot all strata by depth 
ggplot(strata) + geom_sf(aes(fill = Depth_m), color = "black")

### 95 CUMULATIVE DISTRIBUTION FOOTPRINT ####
# create  strata distribution areas 
strata_95 <- strata |>
  right_join(csum_dist95, by = "STRATUM") |> # join the filtered data to the strata shapefile; coerces the dataframe into a shapefile 
  mutate(TYPE = "Distribution Areas") 

# plot it
ggplot() +
  geom_sf(data = ne_coast, fill = "#efe5c7", color = "#816c62") +
  geom_sf(data = strata, fill = NA) +
  geom_sf(data = strata_95, fill = pal[1], alpha = 0.6)+ 
  coord_sf() + 
  xlim(c(80, 65)) +
  ylim(c(32, 46)) +
  labs(x = "Longitude", y = "Latitude", fill = "Distribution Areas") +
  theme(legend.position = "bottom", 
        panel.grid = element_blank()) +
  facet_wrap(~SEASON)


## CREATE SPRING FOOTPRINT #### 
### EXTRACT SOUTHERN FALL STRATA ####
# extract only the fall footprint
fall_95 <- strata_95 |> 
  filter(SEASON == "FALL") |> 
  st_set_geometry(NULL)

# extract only the spring footprint
spring_95 <- strata_95 |> 
  filter(SEASON == "SPRING") |> 
  st_set_geometry(NULL)

# find the strata in the fall footprint that are not in the spring footprint
anti_spring_foot_df <- anti_join(fall_95, spring_95, by = "STRATUM") 

# make an sf for plotting
anti_spring_foot <- inner_join(strata, anti_spring_foot_df, by = "STRATUM") |> 
  rename(geometry = Shape)

# extract column names that have .y at the end as a result of the inner join
columns <- str_subset(names(anti_spring_foot), ".y")

# remove the columns with the extracted column names 
anti_spring_foot <- anti_spring_foot |> 
  select(!all_of(columns))

# extract the column names that end in .x as a result of the inner join and remove .x from the name string
rename <- str_remove(names(anti_spring_foot), ".x")

# rename all the columns that have .x at the end using the rename object above
names(anti_spring_foot) <- rename

# extract centroid coordinate for label plotting 
anti_spring_foot_df <- anti_spring_foot |> 
  st_centroid() |>
  st_coordinates()  |>
  bind_cols(anti_spring_foot) |> 
  select(!geometry)

# plot it
ggplot(anti_spring_foot) + geom_sf() + 
  geom_text(data = anti_spring_foot_df, aes(x = X, y = Y, label = STRATUM), size = 2.5,  color = "blue", check_overlap = TRUE, angle = 45)

#### ADD TO SPRING FOOTPRINT ####
# extract the southern fall strata to spring footprint
add_south_strata <- anti_spring_foot |> 
  filter(STRATUM == 1090 | STRATUM >= 3000) |> 
  mutate(SEASON ="SPRING") 

# plot it
ggplot(add_south_strata) + geom_sf()

# add the strata to the spring footprint 
spr_footprint <- strata_95 |> 
  filter(SEASON == "SPRING") |> 
  rename(geometry = Shape) |>
  bind_rows(add_south_strata)

# plot it
ggplot(spr_footprint) + geom_sf()


### CREATE CONTINUOUS COVERAGE ####
# find strata not in appended spring footprint 
anti_spring_foot <- strata |>
  anti_join(spr_footprint |> st_set_geometry(NULL), by = "STRATUM") 

# pull out centroid coordinates to add labels to strata
anti_spring_foot_df <- anti_spring_foot |> 
  st_centroid() |>
  st_coordinates()  |>
  bind_cols(anti_spring_foot) |> 
  select(!Shape)

# plot  
ggplot() + 
  geom_sf(data = anti_spring_foot, color = "black") + 
  geom_text(data = anti_spring_foot_df, aes(x = X, y = Y, label = STRATUM), size = 3,  color = "red", check_overlap = TRUE, angle = 45)

# manually identify strata that fill holes in footprint
select_spr_strat <- c(1150,1080,3350,3410,1640,3440)

#### ADD TO SPRING FOOTPRINT ####
# add identified strata to spring footprint 
spr_footprint <- strata |> 
  filter(STRATUM %in% select_spr_strat) |> 
  rename(geometry = Shape) |> 
  mutate(SEASON = "SPRING", 
         TYPE = "Distribution Areas") |> 
  bind_rows(spr_footprint)

# plot it 
ggplot(spr_footprint) + geom_sf()

## SAVE ####
saveRDS(spr_footprint, here("data", "sumflounder", "sf_spring_footprint.rds"))





