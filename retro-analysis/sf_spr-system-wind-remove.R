### created: 03/17/2023
### last updated:

#### SPRING ####

###################
#### OBJECTIVE ####
###################
# 


####################


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

#### LOAD DATA ####
# summer flounder data 
sf_spring <- readRDS(here("sdmtmb", "data", "sumflounder_spring.rds"))

# strata dataframe with area weights, to be used in stratified mean functions
strata <- readRDS(here("data", "rds", "strata_wts.rds"))

# stratified mean function
source(here("R", "StratMeanFXs_v2.R"))

#### Stratified Mean Average ####
# read in stratified mean values
stratmeans <- readRDS(here("data", "rds", "strat-mu_all.rds"))

# filter for summer flounder data only
sf_stratmean <- stratmeans %>% 
  filter(SVSPP == 103) %>% 
  group_by(EST_YEAR, TYPE, SEASON) %>% 
  mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) %>% # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper)) # if the upper quantile is NaN, replace with 0

# find the average over 12 years for comparison later
sum(sf_stratmean$stratmu)/length(sf_stratmean$stratmu)

### FORMAT LOOP ####
test <- strata.mean(sf_spring)

x <- 2009
x <- append(x, 2010)

y <- sf_spring |> 
  filter(EST_YEAR %in% x) |>
  strata.mean() |>
  mutate(step = length(x))
ww.data <- bind_rows(all.data, y)

y <- sf_spring |> 
  filter(!EST_YEAR %in% x) |>
  strata.mean() |>
  mutate(step = length(x))
wo.data <- bind_rows(wo.data, y)

### WIND INCLUDED LOOP ####
# create storage vector for years 
x <- c()

# create storage data frame for stratified mean values 
ww.data <- data.frame()

# loop 
for(i in seq(2021, 2009, -1)){
  x <- append(x, i)
  y <- sf_spring |> 
    filter(!EST_YEAR %in% x) |>
    strata.mean() |>
    mutate(STEP = length(x), 
           TYPE = "Included")
  ww.data <- bind_rows(ww.data, y)
}

# calculate stratified mean with all twelve years with wind included 
full.strats <- sf_spring |>
  strata.mean() |>
  mutate(STEP = 0, 
         TYPE = "Included") 

# add to looped dataframe as step 1 
ww.data <-  bind_rows(ww.data, full.strats) |>
  arrange(STEP)

### save data 
saveRDS(ww.data, here("data", "sumflounder", "sf_spr-with_wind-system_rm.rds"))

### WIND PRECLUDED LOOP ####
# create storage vector for years 
x <- c()

# create storage dataframe for stratified mean values
wo.data <- data.frame()


# Loop
for(i in seq(2021,2009, -1)){
  x <- append(x, i)
  y <- sf_spring |> 
    filter(EST_YEAR %in% c(x), AREA == "OUTSIDE") |>
    strata.mean() |>
    mutate(STEP = length(x), 
           TYPE = "Precluded")
  wo.data <- bind_rows(wo.data, y)
}


### save data 
saveRDS(wo.data, here("data", "sumflounder", "sf_spr-without_wind-system_rm.rds"))

# bind with wind and without wind stratified mean values to the same dataframe 
all.data <- bind_rows(ww.data, wo.data) |>
  arrange(STEP)

### save data 
saveRDS(all.data, here("data", "sumflounder", "sf_spr-system_rm-fullset.rds"))

# averages <- all.data |>
#   group_by(STEP) |>
#   summarise(avg = mean(stratmu))

### CHANGE IN TREND WITH EACH ADDITIONAL YEAR PRECLUDED ####
# fit a linear model to each step containing 12 stratified mean values in the dataframe 
slopes <- all.data |>
  group_by(STEP) |>
  nest() |>
  mutate(mods = map(data, ~lm(stratmu~EST_YEAR, data = .x)), 
         coefs = map(mods, broom::tidy, conf.int = TRUE), 
         SEASON = "SPRING") |>
  unnest(coefs) |>
  filter(term == "EST_YEAR") 

### save data 
saveRDS(slopes, here("data", "sumflounder", "sf_spr-system_rm-slopes.rds"))

# plot the slopes to compare how they change with the systematic removal of wind with each year beginnning in 2021. 
slope_plot <- ggplot(slopes) +
  aes(x = factor(STEP), y = estimate) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width= 0.2) + #add the bootstrap confidence interval
  coord_flip() +
  facet_wrap(~SEASON) +
  #theme_minimal() +
  labs(y = "Slope",
       x = "Number of years precluded") +
  #facet_grid(cols = vars(METHOD), rows = vars(SEASON)) + 
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        #axis.text.x = element_text(angle = 15, vjust = 0.5), 
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

### save plot
ggsave(slope_plot, plot = last_plot(), device = "png", path = here())

       