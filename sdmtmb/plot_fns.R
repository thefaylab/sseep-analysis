### created: 04/20/2023
### last updated: 

#### PLOTTING FUNCTIONS ####

###################
#### OBJECTIVE ####
###################
# save plotting functions to be sourced in various scripts 

####################




#### PLOT FUNCTIONS ####
# a ggplot function without land data 
plot_preds <- function(dat, column, width = 10, height = 10) {
  ggplot(dat, aes(X, Y, fill = {{ column }})) +
    geom_tile(width = width, height = height) +
    facet_wrap(~EST_YEAR) +
    coord_equal()
}



# a ggplot function with land data 
plot_map <- function(sf, dat, column, width = 10000, height = 10000) {
  ggplot() +
    geom_tile(data = dat, mapping = aes(X*1000, Y*1000, fill = {{ column }}), width = width, height = height) +
    geom_sf(data = sf) +
    facet_wrap(~EST_YEAR) + 
    labs(x = "Longitude", y = "Latitude")
}
