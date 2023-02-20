### created: 11/28/2022
### last updated: 12/8/2022

#### 03 - CALCULATING MEAN SQUARED DIFFERENCES FOR STRATIFIED MEANS BY SPECIES ####

###################
#### OBJECTIVE ####
###################
# calculate mean squared differences for each species to attempt to identify species to prioritize  

####################


#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(kableExtra)
library(patchwork)


#### LOAD DATA ####
# dataset created from `02-bind-stratmeans.R` here("retro-analysis"). Contains stratified means and variances for each species and unique tow over time.
data <- readRDS(here("data", "rds", "strat-mu_all.rds")) 


# create a lookup table with filename-friendly species names
specieslookup <- data %>%
  ungroup() %>%
  select(SVSPP, COMNAME) %>%
  distinct() %>%
  mutate(spname = str_to_lower(gsub(" ", "_", COMNAME)))

#### CALCULATE MEAN SQUARED DIFFERENCES #####  
mudiff_dat <- data %>%
  filter(EST_YEAR %in% c(2016:2019, 2021)) %>% #filter for recent 5 years, skipping 2020
  group_by(SVSPP, EST_YEAR, SEASON) %>% #, GEO_AREA) %>%
  summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") %>% # calculate the relative differences and square them; drop the groups for further analysis
  group_by(SVSPP, SEASON) %>%
  summarize(mudiff = mean(sq_diff), .groups = "drop") %>% # calculate the average; drop the grouping factor 
  arrange(desc(mudiff)) %>% # arrange highest to lowest 
  left_join(specieslookup, by = "SVSPP") # add the species look up data 


# print the first 10 rows of the data 
topten_fall <- mudiff_dat %>%
  filter(SEASON == "FALL") %>%
  head(10) %>% 
  mutate(mudiff = round(mudiff, 2), 
         COMNAME = str_to_sentence(COMNAME)) %>% 
  rename(Species = COMNAME, 
         "Mean Squared Difference" = mudiff)

kable(topten_fall[,c(4,3,2)], align = "lcccc", caption = "Top Ten Mean Squared Differences by Species", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)

  
topten_spring <- mudiff_dat %>%
    filter(SEASON == "SPRING") %>% 
    head(10) %>% 
    mutate(mudiff = round(mudiff, 2), 
           COMNAME = str_to_sentence(COMNAME)) %>% 
    rename(Species = COMNAME, 
           "Mean Squared Difference" = mudiff)


kable(topten_spring[,c(4,3,2)], align = "lcccc", caption = "Top Ten Mean Squared Differences by Species", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)



### save the data 
saveRDS(mudiff_dat, file = here("data", "rds", "species_mean-sq-diff.rds"))


#### PLOT THE DISTRIBUTION #####
mudiff_dat %>%
  ggplot() + aes(mudiff) + geom_histogram()

### save the plot
ggsave(filename ="species_mean-sq-diff.png", device = "png" , path = here("outputs", "plots"), width = 5, height = 5)


#### CALCULATE TREND #####  
# trend_dat <- data %>%
#   filter(EST_YEAR %in% c(2016:2019, 2021)) %>% #filter for recent 5 years, skipping 2020
#   group_by(SVSPP, EST_YEAR, SEASON) %>% 
#   summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") %>% # calculate the relative differences and square them; drop the groups for further analysis
#   group_by(SVSPP, SEASON) %>%
#   summarize(mudiff = mean(sq_diff), .groups = "drop") %>% # calculate the average; drop the grouping factor 
#   arrange(desc(mudiff)) %>% # arrange highest to lowest 
#   left_join(specieslookup, by = "SVSPP") # add the species look up data 


ggplot(stratmeans %>% filter(SVSPP == 141)) +
  geom_point(aes(x = EST_YEAR, y = stratmu, color = TYPE)) +
  geom_smooth(aes(x = EST_YEAR, y = stratmu, color = TYPE)) +
  facet_wrap(~SEASON) 

# manual calculation of trend
# trend_dat <- stratmeans %>% 
#   group_by(SVSPP, SEASON, TYPE) %>%
#   filter(EST_YEAR %in% c(2009, 2021)) %>%
#   summarize(diff = diff(stratmu), 
#             slope = diff / (2021-2009))
# 
# trend_dat1 <- stratmeans %>% 
#   group_by(SVSPP, SEASON, TYPE) %>%
#   summarize(diff = diff(stratmu)) # adding sum() results in same values as above

# first 3 years vs last 3 years # average of first three / average of last three 
trend09_12 <- stratmeans %>% 
  group_by(SVSPP, SEASON, TYPE) %>% 
  filter(EST_YEAR %in% c(2009:2012)) %>%
  summarise(sums_first = sum(stratmu)) 
trend19_21 <-  stratmeans %>% 
  group_by(SVSPP, SEASON, TYPE) %>% 
  filter(EST_YEAR %in% c(2019:2021)) %>%
  summarise(sums_last = sum(stratmu))

trend_dat2 <- left_join(trend09_12, trend19_21, by = c("SVSPP", "SEASON", "TYPE")) %>% 
  mutate(diff = sums_last - sums_first)




# Linear Models  # look at change to the slope
trend_dat1 <- stratmeans %>% 
  #mutate(code = str_c(SVSPP, SEASON, TYPE)) %>%
  group_by(SVSPP, SEASON, TYPE) %>% 
  nest() %>%
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .x))) %>% 
  mutate(coefs = map(model, broom::tidy, conf.int = TRUE)) %>% 
  unnest(coefs) #%>% 
  #unnest(data) #%>% 
  # mutate(name = "estimate") %>%
  # pivot_wider(names_from = "name", values_from = "estimate")

slopes1 <- trend_dat1 %>% 
  select(SVSPP, SEASON, TYPE, term, estimate) %>% 
  group_by(SVSPP, SEASON, TYPE) %>%
  #mutate(name = "estimate") %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>% 
  rename(intercept = `(Intercept)`)


ggplot() +
  geom_point(data = stratmeans %>% filter(SVSPP == 141), mapping = aes(x = EST_YEAR, y = stratmu, color = TYPE)) +
  geom_abline(data = slopes1 %>% filter(SVSPP == 141), mapping = aes(intercept = intercept, slope = EST_YEAR, color = TYPE)) +
  facet_wrap(~SEASON) 





