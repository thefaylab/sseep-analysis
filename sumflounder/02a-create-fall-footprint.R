### created: 12/1/2023
### updated: 

# 02a - CREATE SUMMER FLOUNDER SPATIAL FOOTPRINT: FALL ####

## OBJECTIVE ####
# identify the sampling frame for fall observations of summer flounder 
# begin with the spatial footprint created by the 95% cumulative distribution of biomass 
# append with additional strata that are in the same depth range and capture the full extent of the offshore wind energy areas. 

### LOAD PACKAGES ####

# active survey strata 
strata <- readRDS(here("data", "rds", "active_strata.rds")) 

# 95% cumulative distribution dataset created from `05a-spatial-filter.R` here("tidy-data").
csum_dist95 <- readRDS(here("data", "rds", "spatial-filter", "cumul-biomass95.rds")) %>% filter(SVSPP == 103)

# northeast coastline
ne_coast <- sf::st_read(dsn = here("gis", "eastern_coast_WGS84.shp"))

# wind areas
wind_areas <- readRDS(here("data", "rds", "wind_areas_062022", "all_wind_areas_Jun2022.rds"))

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
  geom_sf(data = strata_95, aes(fill = TYPE), alpha = 0.6)+ 
  geom_sf(data = wind_areas, aes(fill = Area_Type)) +
  scale_fill_manual(name = "Legend", values = pal[1:3], aesthetics = c("color", "fill"), labels = c("Distribution Areas", "Leased areas", "Planning Areas")) +
  coord_sf() + 
  xlim(c(80, 65)) +
  ylim(c(32, 46)) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom", 
        panel.grid = element_blank()) +
  facet_wrap(~SEASON)


## CREATE FALL FOOTPRINT #### 
### EXTRACT SOUTHERN FALL STRATA ####
# extract only the fall footprint
fall_95 <- strata_95 |> 
  filter(SEASON == "FALL") |> 
  st_set_geometry(NULL)

# extract only the spring footprint
spring_95 <- strata_95 |> 
  filter(SEASON == "SPRING") |> 
  st_set_geometry(NULL)

# find the strata in the spring footprint that are not in the fall footprint
anti_fall_foot_df <- anti_join(spring_95, fall_95, by = "STRATUM") 

# make an sf for plotting
anti_fall_foot <- inner_join(strata, anti_fall_foot_df, by = "STRATUM") |> 
  rename(geometry = Shape)

# extract column names that have .y at the end as a result of the inner join
columns <- str_subset(names(anti_fall_foot), ".y")

# remove the columns with the extracted column names 
anti_fall_foot <- anti_fall_foot |> 
  select(!all_of(columns))

# extract the column names that end in .x as a result of the inner join and remove .x from the name string
rename <- str_remove(names(anti_fall_foot), ".x")

# rename all the columns that have .x at the end using the rename object above
names(anti_fall_foot) <- rename

# extract centroid coordinate for label plotting 
anti_fall_foot_df <- anti_fall_foot |> 
  st_centroid() |>
  st_coordinates()  |>
  bind_cols(anti_fall_foot) |> 
  select(!geometry)

# plot it
ggplot(anti_fall_foot) + geom_sf(aes(fill = Depth_m)) + 
  geom_text(data = anti_fall_foot_df, aes(x = X, y = Y, label = STRATUM), size = 2.5,  color = "blue", check_overlap = TRUE, angle = 45)

#### ADD TO FALL FOOTPRINT ####
# extract the southern spring strata to add to fall footprint
add_south_strata <- anti_fall_foot |> 
  filter(Depth_m %in% c("18-27", "27-55", "55-110")) |> 
  mutate(SEASON ="FALL") 

# plot it
ggplot(add_south_strata) + geom_sf()

# add the strata to the fall footprint 
fall_footprint <- strata_95 |> 
  filter(SEASON == "FALL") |> 
  rename(geometry = Shape) |>
  bind_rows(add_south_strata)

# plot it
ggplot(fall_footprint) + geom_sf()


### CREATE CONTINUOUS COVERAGE ####
# find strata not in appended fall footprint 
anti_fall_foot <- strata |>
  anti_join(fall_footprint |> st_set_geometry(NULL), by = "STRATUM") 

# pull out centroid coordinates to add labels to strata
anti_fall_foot_df <- anti_fall_foot |> 
  st_centroid() |>
  st_coordinates()  |>
  bind_cols(anti_fall_foot) |> 
  select(!Shape)

# plot  
ggplot() + 
  geom_sf(data = anti_fall_foot, color = "black") + 
  geom_text(data = anti_fall_foot_df, aes(x = X, y = Y, label = STRATUM), size = 3,  color = "red", check_overlap = TRUE, angle = 45)

# manually identify strata that fill holes in footprint
select_fall_strat <- c(3350,3410,3440)

#### ADD TO FALL FOOTPRINT ####
# add identified strata to fall footprint 
fall_footprint <- strata |> 
  filter(STRATUM %in% select_fall_strat) |> 
  rename(geometry = Shape) |> 
  mutate(SEASON = "FALL", 
         TYPE = "Distribution Areas") |> 
  bind_rows(fall_footprint)

# plot it 
ggplot() +
  geom_sf(data = fall_footprint) + 
  geom_sf(data = wind_areas, aes(fill = Area_Type)) + ylim(NA, 40) + xlim(76,72)

## SAVE ####
saveRDS(fall_footprint, here("data", "sumflounder", "sf_fall_footprint.rds"))





