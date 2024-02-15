### created: 01/26/2024
### last updated: 02/14/2024

# 04 - CALCULATE STRATIFIED MEAN: ATLANTIC MACKEREL  ####


## OBJECTIVE ####
# calculate stratified mean index using the Atlantic mackerel footprint under the status quo and preclusion scenarios 
# compare the indices using a mean percent relative difference for the most recent 5 years 
# calculate the change in indices over time using a linear regression
# plot the indices over time 

### LOAD PACKAGES ####
library(here)
library(tidyverse)
library(nationalparkcolors)
source(here("R", "StratMeanFXs_v2.R"))

pal <- park_palette("Badlands")

### LOAD DATA ####
# spring Atlantic mackerel data consisting of observations filtered here("atlmackerel", "02-filter-atlantic-mackerel.R")
atlmackerel <- readRDS(here("data", "atlmackerel", "atlmackerel_spring.rds"))

#active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))


## CALCULATE STRATIFIED MEAN ####
# with wind included
stratmu_incl <- atlmackerel |> 
  group_by(SEASON, EST_YEAR) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  mutate(effort = "With Wind Included") |> 
  select(!data) |> 
  unnest(cols = stratmu)

# with wind precluded
stratmu_precl <- atlmackerel |> 
  filter(AREA == "OUTSIDE") |> 
  group_by(SEASON, EST_YEAR) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  mutate(effort = "With Wind Precluded") |> 
  select(!data) |> 
  unnest(cols = stratmu)

## CALCULATE STANDARD ERROR #### 
stratmu <- bind_rows(stratmu_incl, stratmu_precl) |> # bind the data sets 
  group_by(EST_YEAR, effort, SEASON) |> 
  mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
         lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
         upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
         lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
         upper = ifelse(is.nan(upper), 0, upper))


## MEAN PERCENT RELATIVE DIFFERENCE ####
mudiff_dat <- stratmu |> 
  # filter(EST_YEAR %in% c(2016, 2017, 2018, 2019, 2021)) |> #filter for recent 5 years, skipping 2020 and 
  arrange(desc(stratmu)) |>
  group_by(EST_YEAR, SEASON) |> #, 
  summarise(diff_mu = diff(stratmu)) |>
  arrange(desc(diff_mu)) |>
  mutate(exp_mu = (exp(diff_mu))-1) |>
  arrange(desc(exp_mu)) |>
  mutate(sq_diff = exp_mu^2) |>
  arrange(desc(sq_diff))|>
  ungroup()|>
  group_by(SEASON) |>
  summarize(mudiff = mean(sq_diff), .groups = "drop") |> # calculate the average; drop the grouping factor 
  mutate(mudiff = sqrt(mudiff)*100) |>
  arrange(desc(mudiff))

## FIT LINEAR REGRESSIONS ####
stratmu_lms <- stratmu |>
  group_by(SEASON, effort) |> 
  nest() |>
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)),  
         coef = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(coef) |>
  select(SEASON, effort, term, estimate, conf.low, conf.high) |>
  filter(term == "EST_YEAR")


## PLOT ####
ggplot(stratmu) +
  aes(x = as.factor(EST_YEAR), y = stratmu, color = effort, shape = effort) +
  #geom_point() +
  geom_pointrange(aes(ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(SEASON), scales = "free_y") +
  #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") + 
  labs(x = "Year", y = "Stratified Mean (kg/tow)", SEASON = "", effort = "") +
  ylim(0,NA) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = -1), 
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + 
  scale_color_manual(values = pal)

ggsave("am_stratmeans.png", last_plot(), device = "png", here("outputs", "atlmackerel"), width = 9, height = 5)

# save data 
saveRDS(mudiff_dat, here("data", "atlmackerel", "am_mudiffdat.rds"))
saveRDS(stratmu_incl, here("data", "atlmackerel", "am_stratmu_included.rds"))
saveRDS(stratmu_precl, here("data", "atlmackerel", "am_stratmu_precluded.rds"))
saveRDS(stratmu, here("data", "atlmackerel", "am_stratmu.rds"))
saveRDS(stratmu_lms, here("data", "atlmackerel", "am_obs_slopes.rds"))
