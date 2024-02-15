### created: 07/28/2023
### last updated: 02/14/2024

# 02b - SYSTEMATIC PRECLUSION PLOTS: SPRING  ####

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

system.precl.dat <- here("data", "sumflounder", "systematic-preclusion")
sumflounder.dat <- here("data", "sumflounder")
sumflounder.plots <- here("outputs", "sumflounder")

## LOAD DATA ####
# systematic preclusion data containing a stepwise removal of potential wind energy area tows from the annual stratified mean calculation created here("sumflounder", "systematic-preclusion", "02a-spring-systematic-preclusion.R")
data <- readRDS(here(system.precl.dat, "sf_spr-system_rm-fullset.rds"))

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
saveRDS(slopes, here(system.precl.dat, "sf_spr-system_rm-slopes.rds"))

# plot the slopes to compare how they change with the systematic removal of wind with each year beginning in 2021. 
slope_plot <- ggplot(slopes) +
  aes(x = factor(STEP), y = estimate) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width= 0.2) + #add the bootstrap confidence interval
  coord_flip() +
  facet_wrap(~str_to_title(SEASON)) +
  #theme_minimal() +
  labs(y = "Linear regression slope of abundance index",
       x = "Number of years precluded") +
  #facet_grid(cols = vars(METHOD), rows = vars(SEASON)) + 
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        #axis.text.x = element_text(angle = 15, vjust = 0.5), 
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
        strip.text = element_text(size = 11))

### save plot
ggsave("spr_sys-precl_plot.png", plot = slope_plot, device = "png", path = here(sumflounder.plots), width = 7, height = 4)

