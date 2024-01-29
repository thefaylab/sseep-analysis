### created: 01/29/2024
### last updated: 

# 02 - ATLANTIC MACKEREL SYSTEMATIC PRECLUSION PLOT  ####

## OBJECTIVE ####
# fits a linear model to each time step where years may have a stratified mean calculated under the preclusion scenario
# tests how the linear regression slope estimate changes under the assumption of each additional year of wind energy operation and preclusion 
# plots the linear regression slope estimate with each time step


## LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
source(here("R", "StratMeanFXs_v2.R")) # stratified mean functions

system.precl.dat <- here("data", "atlmackerel", "systematic-preclusion")
atlmackerel.dat <- here("data", "atlmackerel")
atlmackerel.plots <- here("outputs", "atlmackerel")

## LOAD DATA ####
# systematic preclusion data containing a stepwise removal of potential wind energy area tows from the annual stratified mean calculation created here("atlmackerel", "systematic-preclusion", "01-atlmackerel-systematic-preclusion.R")
data <- readRDS(here(system.precl.dat, "system_rm-fullset.rds"))

### CHANGE IN TREND WITH EACH ADDITIONAL YEAR PRECLUDED ####
# fit a linear model to each step containing 12 stratified mean values in the dataframe 
slopes <- data |>
  group_by(STEP) |>
  nest() |>
  mutate(mods = map(data, ~lm(stratmu~EST_YEAR, data = .x)), 
         coefs = map(mods, broom::tidy, conf.int = TRUE), 
         SEASON = "SPRING") |>
  unnest(coefs) |>
  filter(term == "EST_YEAR") 

### save data 
saveRDS(slopes, here(system.precl.dat, "system_rm-slopes.rds"))

# plot the slopes to compare how they change with the systematic removal of wind with each year beginning in 2021. 
slope_plot <- ggplot(slopes) +
  aes(x = factor(STEP), y = estimate) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width= 0.2) + 
  coord_flip() +
  facet_wrap(~str_to_title(SEASON)) +
  labs(y = "Linear regression slope of abundance index",
       x = "Number of years precluded") +
  theme(legend.position="bottom",
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
        strip.text = element_text(size = 11))

### save plot
ggsave("sys-precl_plot.png", plot = slope_plot, device = "png", path = here(atlmackerel.plots), width = 7, height = 4)

