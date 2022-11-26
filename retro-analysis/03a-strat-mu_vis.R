### created: 11/25/2022
### last updated: 

#### 03a - PLOTTING STRATIFIED MEANS BY SPECIES ####

###################
#### OBJECTIVE ####
###################
# write loops to call stratified means and variance data and plot by species across time. 

# compare plots to identify potential impacts to abundance indexes when wind tows are removed. 

####################


#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


#### LOAD DATA ####
# dataset created from `02-bind-stratmeans.R` here("retro-analysis"). Contains stratified means and variances for each species and unique tow over time.
data <- readRDS(here("data", "rds", "strat-mu_all.rds")) 

# create a species vector to loop over below.
species <- unique(data$SVSPP)

# create empty list for storing plotting results of the loop below
mu_plots <- list()


#### FORMAT GGPLOT FOR LOOP ####

x <- stratmeans %>% filter(SVSPP == 141)

y <- ggplot(x) + 
  geom_line(aes(EST_YEAR, stratmu, color = TYPE)) + 
  labs(x = "YEAR", y = "Stratified Mean (kg/tow)", title = x$COMNAME)


#### GGPLOT LOOPS ####
# two loops created due to available device storage 

# Loop 01 - loop over the species vector and create stratified mean plots over time for each species and save into the empty plot list 
for(i in species){ # for each value i in the species vector
  
  x <- data %>% filter(SVSPP == i) # filter the dataset where the species code is the value of i
  
  mu_plots[[i]] <- ggplot(x) + 
    geom_line(aes(EST_YEAR, # x value 
                  stratmu, # y value - stratified mean value
                  color = TYPE)) + # plot all values and differentiate based on whether or not wind tows were included in the calculation 
    labs(x = "YEAR", y = "Stratified Mean (kg/tow)", title = x$COMNAME) # edit plot labels 
}

### save the plot list 
saveRDS(mu_plots, file = here("data", "rds", "mu-species-plots.rds"))


# Loop 02 - loop over the species vector, print and save the plot within the list object
for(i in seq_along(species)){ # move along the sequence of values in the species vector
  
  if(is.null(mu_plots[[i]])) next # if the sequential value i in the plot list is NULL, skip the iteration and move on to the next step in the sequence
  
   print(mu_plots[[i]]) # print the plot value corresponding with value i
  
  # save the printed plot with the species name based on the list value generated
  ggsave(filename = paste(unique(mu_plots[[i]]$data$COMNAME), ".png"), device = "png", path = here("outputs", "plots", "strat_mu"), width = 5, height = 5)

}
