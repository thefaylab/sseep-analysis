### created: 1/23/2022
### last updated: 2/6/2023

#### 00 - NULL HYPOTHESIS TESTING ####

###################
#### OBJECTIVE ####
###################
# calculate stratified means for each species, strata, and year combination based on the historical time series data. Slice a sample from the dataset using the same number of tows that are taken out based on the area in which the tows occur. The analysis contained herein calculates the stratified means for the full time series using a proportion of tows removed to represent the null hypothesis scenario.  

####################


##### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


##### LOAD DATA ####
# dataset created from `02-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow. 
data <- readRDS(here("data", "rds", "merged_data_complete.rds")) %>% mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# species dataframe for adding to final dataset 
species <- readRDS(here("data", "rds", "species.rds"))

# stratified means dataset containing averages calculated with wind tows and with wind tows removed
stratmeans <- readRDS(here("data", "rds", "strat-mu_all.rds"))

# dataset created from `01a-nowind-loop.R` here("retro-analyis"). Contains strata area weights
strata <- readRDS(here("data", "rds", "strata.rds"))

# calculate total survey area for use in future calculations  
BTSArea <- as.integer(sum(strata$Area_SqNm))


#### CALCULATE THE PROPORTION OF WIND TOWS ####
# count the number of wind tows and divide by the total number of tows  
length(which(data$AREA == "WIND"))/length(data$AREA)

# create a null dataset that has the same number of tows removed as the number calculated in the code above, to represent the same proportion of tows that were used in the "With Wind Precluded" calculation 
null_data <- data %>% 
  slice_sample(prop = 0.95, replace = FALSE)

#### CALCULATE INDIVIDUAL MEANS AND VARIANCES ####
# calculate individual means and variances for each combinations of species, year, and stratum
null_means <- null_data %>%  
  group_by(STRATUM, EST_YEAR, SVSPP, SEASON) %>% 
  summarise(towct = length(unique(STATION)), # calculate unique tows
            mu = sum(EXPCATCHWT)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
            var = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
                         sum((EXPCATCHWT - mu)^2)/(towct - 1))) %>% # if tow count does not equal 1, then find the variance of biomass
  left_join(strata, by = "STRATUM") %>% # add each stratum area and relative weight to the dataset based on STRATUM number
  mutate(wt_mu = Area_SqNm * mu, # part one of the stratified mean formula
         wt_var = ((((RelWt)^2) * var) / towct) * (1 - (towct / Area_SqNm))) # part one of the stratified variance formula


### save the data 
saveRDS(null_means, here("data", "rds", "null_means.rds"))



#### COMPLETE STRATIFIED MEAN AND VARIANCE CALCULATIONS ####
# calculate stratified means and variances for each combinations of species and year based on individual stratum means and variances 
null_stratmu <- null_means %>% 
  group_by(SVSPP, EST_YEAR, SEASON) %>% #, GEO_AREA) %>% 
  summarise(stratmu = (sum(wt_mu)) / BTSArea, # part two of the stratified mean formula
            stratvar = sum(wt_var)) %>% # part two of the stratified variance formula
  mutate(TYPE = paste("Null Test")) %>% # paste identifying information of means and variances for joining and plotting in later scripts 
  left_join(species, by = "SVSPP") %>% # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)

### save the data 
saveRDS(null_stratmu, here("data", "rds", "null_strat-mu.rds"))


#### BIND THE DATASETS FOR PLOT COMPARISONS ####
stratmeans <- bind_rows(stratmeans, null_stratmu)
  

#### CALCULATE THE QUANTILES OF DISTRIBUTIONS ####
stratmeans <- stratmeans %>% 
  #filter(GEO_AREA %in% c("MAB", "GOM"))%>%
  group_by(SVSPP, EST_YEAR, TYPE, SEASON) %>% #, GEO_AREA) %>%
  mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) %>% # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper)) # if the upper quantile is NaN, replace with 0
             
#### GGPLOT LOOPS ####
##### SET UP DATA VECTORS FOR PLOTTING LOOP ####
# create a species vector to loop over below.
species <- unique(stratmeans$SVSPP)

# create empty list for storing plotting results of the loop below
mu_plots <- list()

# library(nationalparkcolors)
# names(park_palettes)
# pal <- park_palette("SmokyMountains", 3)

##### LOOP 01: CREATE PLOTS #####
# loop over the species vector and create stratified mean plots over time for each species and save into the empty plot list 
for(i in species){ # for each value i in the species vector
  
  x <- data %>% filter(SVSPP == i) # filter the dataset where the species code is the value of i
  
  mu_plots[[i]] <- ggplot(x) + 
    aes(as.factor(EST_YEAR), # x value 
        stratmu, # y value - stratified mean value
        color = TYPE, # differentiate color based on whether or not wind tows were included in the calculation
        shape = TYPE) +  # differentiate shapes based on whether or not wind tows were included in the calculation
    geom_pointrange(aes(ymin=lower, ymax = upper), position = position_dodge2(width=0.4)) + # plot the upper and lower quantiles about the mean 
    facet_wrap(vars(SEASON), scales = "free_y") + # create sequence of panels based on SEASON variable
    #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") + # create sequence of panels based on SEASON variable
    labs(x = "YEAR", y = "Stratified Mean (kg/tow)", title = str_to_title(x$COMNAME), subtitle = str_c("Annual and seasonal stratified mean biomass for", str_to_lower(x$COMNAME), "when wind tows are included and precluded from the calculation", sep = " "), SEASON = "", TYPE = "") + # edit plot labels 
    ylim(0,NA) +
    theme_bw() + # black and white plot theme
    theme(legend.position="bottom", # move legend to the bottom
          legend.title = element_blank(), # leave legend title blank
          axis.text.x = element_text(angle = 90, hjust = -1), # rotate axis labels 90 deg
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")), # increase space between axis labels and title
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + # increase space between axis labels and title
    scale_color_manual(values = pal) # set color palette 
}

# remove null observations where no plots were created 
mu_plots <- compact(mu_plots)

### save the plot list 
saveRDS(mu_plots, file = here("data", "rds", "mu-species-plots_v2.rds"))

##### LOOP 02: SAVE PLOTS #####
# loop over the species vector, save the plot within the list object

for(i in seq_along(species)){ # move along the sequence of values in the species vector
  
  #if(is.null(mu_plots[[i]])) next # if the sequential value i in the plot list is NULL, skip the iteration and move on to the next step in the sequence
  
  # print(mu_plots[[i]]) # print the plot value corresponding with value i
  
  # save the printed plot with the species name based on the list value generated
  #ggsave(filename = paste(unique(mu_plots[[i]]$data$COMNAME), ".png"), device = "png", path = here("outputs", "plots", "strat_mu"), width = 5, height = 5)
  ggsave(filename = paste0(specieslookup$spname[i], ".png"), device = "png" , plot = mu_plots[[i]], path = here("outputs", "plots", "strat_mu2"), width = 10, height = 5)
  
}
