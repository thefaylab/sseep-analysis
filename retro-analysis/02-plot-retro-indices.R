### created: 04/23/2024
### last updated: 11/10/2024

# 02 - PLOT RETROSPECTIVE INDICES BY SPECIES ####


## OBJECTIVE ####
# write loops to call stratified means and variance data and plot by species across time. 
# compare plots to identify potential impacts to abundance indexes when wind tows are removed. 


### Load packages ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

#install.packages("devtools")
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)

### File locations ####
dat.files <- here("data", "rds", "retro-analysis")

### Read in data ####
# dataset created from `01-calculate-retro-indices.R` here("retro-analysis"). 
data <- readRDS(here(dat.files, "all_stratmu_rows.rds"))

### DATA WRANGLE ####
# calculate the logistic standard deviation and logistic normal distribution 
data <- data |> 
  group_by(SVSPP, EST_YEAR, effort, SEASON) |> 
  mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper)) # if the upper quantile is NaN, replace with 0

# create a species vector to loop over below.
species <- unique(data$SVSPP)

# create empty list for storing plotting results of the loop below
mu_plots <- list()

# create universal aesthetics for ggplot
# names(park_palettes)
pal <- c("#548F01",  "#D58A60")#park_palette("Badlands")

# create a lookup table with filename-friendly species names
specieslookup <- data |> 
  ungroup() |>
  select(SVSPP, COMNAME) |> 
  distinct() |> 
  mutate(spname = str_to_lower(gsub(" ", "_", COMNAME)))|> 
  arrange(SVSPP)

## GGPLOT LOOPS ####
# two loops created due to available device storage 

### Loop 01 #####
# loop over the species vector and create stratified mean plots over time for each species and save into the empty plot list 
for(i in species){ # for each value i in the species vector
  
  x <- data |> filter(SVSPP == i) # filter the dataset where the species code is the value of i
  
  mu_plots[[i]] <- ggplot(x) + 
    aes(as.factor(EST_YEAR), # x value 
        stratmu, # y value - stratified mean value
        color = effort, # differentiate color based on whether or not wind tows were included in the calculation
        shape = effort) +  # differentiate shapes based on whether or not wind tows were included in the calculation
    geom_pointrange(aes(ymin=lower, ymax = upper), position = position_dodge2(width=0.4)) + # plot the upper and lower quantiles about the mean 
    facet_wrap(vars(SEASON), scales = "free_y") + # create sequence of panels based on SEASON variable
    #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") + # create sequence of panels based on SEASON variable
    labs(x = "Year", y = "Stratified Mean (kg/tow)", title = str_to_title(x$COMNAME), subtitle = str_c("Annual and seasonal stratified mean biomass for", str_to_lower(x$COMNAME), "when wind tows are included and precluded from the calculation", sep = " "), SEASON = "", TYPE = "") + # edit plot labels 
    ylim(0,NA) +
    theme_bw() + # black and white plot theme
    theme(legend.position="bottom", # move legend to the bottom
          legend.title = element_blank(), # leave legend title blank
          axis.text.x = element_text(angle = 90, hjust = -1), # rotate axis labels 90 deg
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")), # increase space between axis labels and title
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + # increase space between axis labels and title
    scale_color_manual(values = pal) # set color palette 
}

mu_plots <- compact(mu_plots) # remove NULL list items


### save the plot list 
saveRDS(mu_plots, file = here("data", "rds","retro-analysis",  "stratmu-plots.rds"))


##### PRINT AND SAVE #####
# to print single plots from from mu_plots, run the following code by replacing the value in the brackets with the SVSPP code of the species of interest found in the specieslookup table. Otherwise to save all 400 species, run loop 02.
# print(mu_plots[[###]])


### Loop 02 #####
# loop over the species vector, save the plot within the list object

for(i in seq_along(species)){ # move along the sequence of values in the species vector
  
  #if(is.null(mu_plots[[i]])) next # if the sequential value i in the plot list is NULL, skip the iteration and move on to the next step in the sequence
  
  # print(mu_plots[[i]]) # print the plot value corresponding with value i
  
  # save the printed plot with the species name based on the list value generated
  #ggsave(filename = paste(unique(mu_plots[[i]]$data$COMNAME), ".png"), device = "png", path = here("outputs", "plots", "strat_mu"), width = 5, height = 5)
  ggsave(filename = paste0(specieslookup$spname[i], ".png"), device = "png" , plot = mu_plots[[i]], path = here("outputs", "stratmu-plots"), width = 10, height = 5)
  
}
