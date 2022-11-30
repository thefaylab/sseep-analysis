### created: 11/28/2022
### last updated: 

#### 03 - CALCULATING MEAN SQUARED DIFFERENCES FOR STRATIFIED MEANS BY SPECIES ####

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
# species <- unique(data$SVSPP)

# create a lookup table with filename-friendly species names
specieslookup <- data %>%
  select(SVSPP, COMNAME) %>%
  distinct() %>%
  mutate(spname = str_to_lower(gsub(" ", "_", COMNAME)))

mudiff_dat <- data %>%
  filter(EST_YEAR %in% c(2016:2019, 2021)) %>%
  group_by(SVSPP, EST_YEAR, SEASON) %>%
  summarize(sq_diff = diff(stratmu)^2, .groups = "drop") %>%
  group_by(SVSPP) %>%
  summarize(mudiff = mean(sq_diff), .groups = "drop") %>%
  arrange(desc(mudiff)) %>% 
  left_join(specieslookup, by = "SVSPP")

saveRDS(mudiff_dat, file = here("data", "rds", "species_mean-sq-diff.rds"))

head(mudiff_dat, 10)

mudiff_dat %>%
  ggplot() + aes(mudiff) + geom_histogram()

ggsave(filename ="species_mean-sq-diff.png", device = "png" , path = here("outputs", "plots"), width = 5, height = 5)




#### FORMAT GGPLOT FOR LOOP ####

# x <- data %>% filter(SVSPP == 141)
# 
# y <- ggplot(x) + 
#   geom_line(aes(EST_YEAR, stratmu, color = TYPE)) + 
#   labs(x = "YEAR", y = "Stratified Mean (kg/tow)", title = x$COMNAME)


#### GGPLOT LOOPS ####
# two loops created due to available device storage 

# Loop 01 - loop over the species vector and create stratified mean plots over time for each species and save into the empty plot list 
# for(i in species){ # for each value i in the species vector
#   
#   x <- data %>% filter(SVSPP == i) # filter the dataset where the species code is the value of i
#   
#   mu_plots[[i]] <- ggplot(x) + 
#     geom_line(aes(EST_YEAR, # x value 
#                   stratmu, # y value - stratified mean value
#                   color = TYPE)) + # plot all values and differentiate based on whether or not wind tows were included in the calculation 
#     labs(x = "YEAR", y = "Stratified Mean (kg/tow)", title = x$COMNAME,
#          TYPE = "") + # edit plot labels 
#     theme(legend.position="bottom",
#           legend.title = element_blank())
# }
# 
# ### save the plot list 
# saveRDS(mu_plots, file = here("data", "rds", "mu-species-plots.rds"))
# 
# 
# 
# 
# # Loop 02 - loop over the species vector, print and save the plot within the list object
# for(i in seq_along(species)){ # move along the sequence of values in the species vector
#   
#   if(is.null(mu_plots[[i]])) next # if the sequential value i in the plot list is NULL, skip the iteration and move on to the next step in the sequence
#   
# #   print(mu_plots[[i]]) # print the plot value corresponding with value i
#   
#   # save the printed plot with the species name based on the list value generated
#   #ggsave(filename = paste(unique(mu_plots[[i]]$data$COMNAME), ".png"), device = "png", path = here("outputs", "plots", "strat_mu"), width = 5, height = 5)
#   ggsave(filename = paste0(specieslookup$spname[i], ".png"), device = "png" , plot = mu_plots[[i]], path = here("outputs", "plots", "strat_mu"), width = 5, height = 5)
# 
# }
