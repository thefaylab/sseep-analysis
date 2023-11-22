### created: 04/20/2023
### last updated: 08/14/2023

# PLOTTING FUNCTIONS ####

## OBJECTIVE ####
# save plotting functions to be sourced in various scripts 


## LOAD DATA ####
coastline <- sf::st_read(dsn = here("gis", "eastern_coast_UTM.shp"))

strata_utm <- sf::st_read(dsn = here("gis", "NEFSC-BTS_strata_Jun2022.gdb"), layer = "NEFSC_BTS_ActiveStrata_Jun2022") |> 
  rename(STRATUM = "Strata_Num") |> # change the STRATUM column to match the cumulative distribution by strata data frame for a join
  st_transform(strata, crs = 32618)

theme_set(theme_bw())


## PLOT FUNCTIONS ####
# a ggplot function without land data 
plot_preds <- function(dat, column, width = 10, height = 10) {
  ggplot(dat, aes(X, Y, fill = {{ column }})) +
    geom_tile(width = width, height = height) +
    #facet_wrap(~EST_YEAR) +
    coord_equal()
}



# a ggplot function with land data 
plot_map <- function(coastline, strata_utm, dat, column, width = 10000, height = 10000, ...) {
  ggplot(...) +
    geom_sf(data = coastline, fill = "#efe5c7", color = "#816c62") +
    geom_sf(data = strata_utm, fill = NA) +
    geom_tile(data = dat, mapping = aes(X*1000, Y*1000, fill = {{ column }}, ...), width = width, height = height, ...) +
    #facet_wrap(~EST_YEAR) + 
    labs(x = "Longitude", y = "Latitude") + 
    theme(panel.grid = element_blank()) 
  
}
