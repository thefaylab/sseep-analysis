### created: 12/28/2023
### last updated: 03/18/2024

# 02 - DISTRIBUTION OF SIMULATED LINEAR REGRESSION SLOPE ESTIMATES ####


## OBJECTIVE ####
# plot the distribution of 1000 linear regression estimates for each season and scenario

### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
library(infer)
suppressPackageStartupMessages(library(tidyverse))
set.seed(156)
source(here("R", "StratMeanFXs_v2.R"))

post.check.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/atlmackerel/data/post-check"
post.check.plots <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/atlmackerel/plots/post-check"

### LOAD DATA ####
# active strata weights
strata <- readRDS(here(dat, "active_strata_wts.rds"))

# read in dataset used to fit model and where observations at depths greater than 200m were removed to recalculate the stratified mean and change in stratified mean over time.
am_spring <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring_rm200.rds")) |> 
  mutate(EST_YEAR = YEAR, 
         AREA = as.character(AREA))

# simulated changes in spring abundance indices over time for each scenario, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01b-spring-mod-ppcheck1.R") 
## with wind included
# spr_ww_lms <- readRDS( file = here("sdmtmb", "sumflounder", "data", "post-check", "atlmackerel_ww_slopes.rds"))
# ## without wind (wind precluded)
# spr_wow_lms <- readRDS(file = here("sdmtmb", "sumflounder", "data", "post-check", "atlmackerel_wow_slopes.rds"))
spring_slopes <- readRDS(file = here(post.check.dat, "spring_slopes.rds")) |> 
  rename(effort = TYPE)

## CALCULATE OBSERVED STRATIFIED MEAN AND SLOPE ESTIMATES ####
# with wind
# obs_stratmu_ww <- am_spring |>
#   stratified.mean() |> 
#   mutate(TYPE = "With Wind Included")
# 
# # without wind
# obs_stratmu_wow <- am_spring |> 
#   filter(AREA == "OUTSIDE") |> 
#   stratified.mean() |> 
#   mutate(TYPE = "With Wind Precluded")

obs_stratmu <- am_spring |>
  group_by(EST_YEAR) |> 
  nest() |> 
  mutate(incl_stratmu = map(data, ~stratified.mean(.,strata) |> mutate(effort = "With Wind Included")),
         precl_stratmu = map(data, ~filter(., AREA == "OUTSIDE") |> stratified.mean(strata) |> mutate(effort = "With Wind Precluded")))#bind_rows(obs_stratmu_ww, obs_stratmu_wow)

saveRDS(obs_stratmu, here("sdmtmb", "atlmackerel", "data", "atlmackerel_obs_stratmu_rm200.rds"))

incl_slopes <- obs_stratmu |> 
  unnest(cols = incl_stratmu) |> 
  select(!c(data, precl_stratmu)) |>
  group_by(effort) |> 
  nest() |> 
  mutate(model = map(data, ~lm(stratmu~EST_YEAR, data = .)), 
         coefs = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(cols = coefs) |>
  select(effort, term, estimate,conf.low, conf.high)|> 
  filter(term=="EST_YEAR") |> 
  mutate(SEASON ="SPRING")

precl_slopes <- obs_stratmu |> 
  unnest(cols = precl_stratmu) |> 
  select(!c(data, incl_stratmu)) |>
  group_by(effort) |> 
  nest() |> 
  mutate(model = map(data, ~lm(stratmu~EST_YEAR, data = .)), 
         coefs = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(cols = coefs) |>
  select(effort, term, estimate,conf.low, conf.high)|> 
  filter(term=="EST_YEAR") |> 
  mutate(SEASON ="SPRING")

obs_slopes <- bind_rows(incl_slopes, precl_slopes) #obs_stratmu |> 
  # group_by(TYPE) |> 
  # nest() |> 
  # mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)), 
  #        coef = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  # unnest(coef) |> 
  # select(TYPE, term, estimate, conf.low, conf.high) |>
  # filter(term == "EST_YEAR") |> 
  # mutate(SEASON = "SPRING") 

saveRDS(obs_slopes, here("sdmtmb", "atlmackerel", "data", "atlmackerel_obs_slopes_rm200.rds"))


## CALCULATE CONFIDENCE INTERVALS OF SIMULATED ESTIMATES ####
# without wind (wind precluded)
spr.wow.ci <- spring_slopes |>
  filter(effort == "With Wind Precluded") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975)) 
saveRDS(spr.wow.ci, here(post.check.dat, "atlmackerel_sim-wow-ci.rds"))


# with wind included
spr.ww.ci <- spring_slopes |>
  filter(effort == "With Wind Included") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975)) 
saveRDS(spr.ww.ci, here(post.check.dat, "atlmackerel_sim-ww-ci.rds"))


## CALCULATE P-VALUE####
# comparing the estimates of trend to the estimate of trend when wind is precluded
pval <- spring_slopes |> 
  group_by(effort) |> 
  nest() |> 
  # left_join(obs_slopes_nest, by = "effort") |>
  mutate(test_ct = map(data, ~filter(., .x$estimate >= obs_slopes$estimate[2]) |> nrow()), 
         pval = map(test_ct, ~ .x / 1000)) |> 
  select(effort, pval) |> 
  unnest(cols = pval)



## PLOT DISTRIBUTION OF SLOPES ####
### WITH WIND PRECLUDED: SPRING ####
spr_precl_plot <- spring_slopes |> 
  filter(effort == "With Wind Precluded") |>
  plot_distribution(ci_dat = spr.wow.ci, scenario = "With Wind Precluded") +
  #ggplot(spring_slopes |> filter(TYPE == "With Wind Precluded")) +
#   geom_histogram(aes(x = estimate, fill = "orange")) +
#   scale_fill_manual(values = c("orange"), labels = c("With Wind Precluded"), name = NULL) + 
#   shade_confidence_interval(endpoints = spr.wow.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(aes(xintercept = as.numeric(obs_slopes[2,3]), linetype = effort, color = effort), linewidth = 1) +
  scale_color_manual(values = c("#1A1237"), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  # annotate("text", x = -0.005, y = 60, label = str_c("Observed Slope", round(obs_slopes[2,3], 4), sep = ": "), angle = 90) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) + 
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON)) #+
#   labs(x = "Linear regression slope of abundance index", y = "") +
#   theme_bw() +
#   theme(title = element_text(size = 10), legend.position = "bottom")
# saveRDS(spr_precl_plot, here(post.check.dat, "atlmackerel_sim-wow-plot.rds"))

### WITH WIND INCLUDED: SPRING ####
spr_incl_plot <- spring_slopes |> 
  filter(effort == "With Wind Included") |>
  plot_distribution(ci_dat = spr.ww.ci) +
  # ggplot(spring_slopes |> filter(TYPE == "With Wind Included")) +
  # geom_histogram(aes(x = estimate, fill = "#3f7f00")) + 
  # scale_fill_manual(values = c("#3f7f00"), labels = c("With Wind Included"), name = NULL) +
  # shade_confidence_interval(endpoints = spr.ww.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(aes(xintercept = as.numeric(obs_slopes[2,3]), linetype = effort, color = effort), linewidth = 1) +
    scale_color_manual(values = c("#1A1237"), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
    scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) + 
    scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  # annotate("text", x = -0.005, y = 60, label = str_c("Observed Slope", round(obs_slopes[2,3], 4), sep = ": "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) #+
  # labs(x = "Linear regression slope of abundance index", y = "Number of linear regression slopes") +
  # theme_bw() +
  # theme(title = element_text(size = 10), legend.position = "bottom")
saveRDS(spr_incl_plot, here(post.check.dat, "atlmackerel_sim-ww-plot.rds"))


## PATCHWORK ####
spr_dist_plots <- (spr_incl_plot + spr_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
saveRDS(spr_dist_plots, here(post.check.dat, "atlmackerel_sim-dist-plots.rds"))


### save plots
ggsave("atlmackerel-sim-slope-dists.png", plot = spr_dist_plots, device = "png", path = here(post.check.plots), width = 15, height = 10)

