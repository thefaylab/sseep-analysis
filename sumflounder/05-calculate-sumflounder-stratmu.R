### created: 12/05/2023
### last updated: 03/08/2024

# 05 - CALCULATE STRATIFIED MEAN: SUMMER FLOUNDER ####


## OBJECTIVE ####
# calculate stratified mean index using the summer flounder footprint under the status quo and preclusion scenarios 
# compare the indices using a mean percent relative difference for the most recent 5 years 
# calculate the change in indices over time using a linear regression
# plot the indices over time 

### LOAD PACKAGES ####
library(here)
library(tidyverse)
library(nationalparkcolors)
source(here("R", "StratMeanFXs_v2.R"))
source(here("R", "plot_fns.R"))

pal <- park_palette("Badlands")

### LOAD DATA ####
# summer flounder data consisting of observations filtered based on seasonal footprints created here("sumflounder", "04-filter-summer-flounder.R")
sumflounder <- readRDS(here("data", "sumflounder", "sumflounder.rds"))

#active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here("data", "rds", "active_strata_wts.rds"))


## CALCULATE STRATIFIED MEAN ####
# with wind included
sf_stratmu_incl <- sumflounder |> 
  group_by(SEASON, EST_YEAR) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  mutate(effort = "With Wind Included") |> 
  select(!data) |> 
  unnest(cols = stratmu)

# with wind precluded
sf_stratmu_precl <- sumflounder |> 
  filter(AREA == "OUTSIDE") |> 
  group_by(SEASON, EST_YEAR) |> 
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  mutate(effort = "With Wind Precluded") |> 
  select(!data) |> 
  unnest(cols = stratmu)


sf_stratmu_rows <- bind_rows(sf_stratmu_incl, sf_stratmu_precl) #|> # bind the data sets 
  # group_by(EST_YEAR, effort, SEASON) |> 
  # mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
  #        lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
  #        upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
  # mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
  #        lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
  #        upper = ifelse(is.nan(upper), 0, upper))


## MEAN PERCENT RELATIVE DIFFERENCE ####
# mudiff_dat <- sf_stratmu|> 
#   filter(EST_YEAR %in% c(2015, 2016, 2018, 2019, 2021)) |> #filter for recent 5 years, skipping 2020 and 
#   arrange(desc(stratmu)) |>
#   group_by(EST_YEAR, SEASON) |> #, 
#   summarise(diff_mu = diff(stratmu)) |>
#   arrange(desc(diff_mu)) |>
#   mutate(exp_mu = (exp(diff_mu))-1) |>
#   arrange(desc(exp_mu)) |>
#   mutate(sq_diff = exp_mu^2) |>
#   arrange(desc(sq_diff))|>
#   ungroup()|>
#   group_by(SEASON) |>
#   summarize(mudiff = mean(sq_diff), .groups = "drop") |> # calculate the average; drop the grouping factor 
#   mutate(mudiff = sqrt(mudiff)*100) |>
#   arrange(desc(mudiff))

sf_stratmu_cols <- left_join(sf_stratmu_incl, sf_stratmu_precl, by = c("SEASON", "EST_YEAR")) 

mudiff_dat <- sf_stratmu_cols |> 
  calc.errors(observed = stratmu.y, expected = stratmu.x) |> 
  group_by(SEASON) |> 
  mean.diff()



## FIT LINEAR REGRESSIONS ####
sf_stratmu_lms <- sf_stratmu_rows |>
  group_by(SEASON, effort) |> 
  nest() |>
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)),  
         coef = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(coef) |>
  select(SEASON, effort, term, estimate, conf.low, conf.high) 

fall_lms <- sf_stratmu_lms |> 
  filter(SEASON == "FALL")

spring_lms <- sf_stratmu_lms |> 
  filter(SEASON == "SPRING")

sf_stratmu_slopes <- sf_stratmu_lms |>
  filter(term == "EST_YEAR")

## PLOT ####
sf_stratmu_rows |> 
  plot.stratmu(EST_YEAR, color = effort, shape = effort) + 
  facet_wrap(~str_to_title(SEASON)) #+ 
  
# ggplot(sf_stratmu) +
#   aes(x = as.factor(EST_YEAR), y = stratmu, color = effort, shape = effort) +
#   #geom_point() +
#   geom_pointrange(aes(ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) + geom_smooth(aes(x = EST_YEAR, y = stratmu), method = lm, se = FALSE) +
#   facet_wrap(vars(SEASON), scales = "free_y") +
#   #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") + 
#   labs(x = "YEAR", y = "Stratified Mean (kg/tow)", SEASON = "", effort = "") +
#   ylim(0,NA) +
#   theme_bw() + 
#   theme(legend.position="bottom",
#         legend.title = element_blank(), 
#         axis.text.x = element_text(angle = 90, hjust = -1), 
#         axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")), 
#         axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + 
#   scale_color_manual(values = pal)

ggsave("sf_stratmeans.png", last_plot(), device = "png", here("outputs", "sumflounder"), width = 9, height = 5)

# save data 
saveRDS(mudiff_dat, here("data", "sumflounder", "mudiffdat.rds"))
saveRDS(sf_stratmu_incl, here("data", "sumflounder", "stratmu_included.rds"))
saveRDS(sf_stratmu_precl, here("data", "sumflounder", "stratmu_precluded.rds"))
saveRDS(sf_stratmu_rows, here("data", "sumflounder", "stratmu_rows.rds"))
saveRDS(sf_stratmu_cols, here("data", "sumflounder", "stratmu_cols.rds"))
saveRDS(sf_stratmu_lms, here("data", "sumflounder", "obs_slopes.rds"))
