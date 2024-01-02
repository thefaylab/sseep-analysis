### created: 12/28/2023
### last updated: 01/02/2024

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

post.check.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/atlmackerel/data/post-check"
post.check.plots <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/atlmackerel/plots/post-check"

### LOAD DATA ####
# read in dataset used to fit model and where observations at depths greater than 200m were removed to recalculate the stratified mean and change in stratified mean over time.
am_spring <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring_rm200.rds")) |> 
  mutate(EST_YEAR = YEAR, 
         AREA = as.character(AREA))

# simulated changes in spring abundance indices over time for each scenario, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01b-spring-mod-ppcheck1.R") 
## with wind included
# spr_ww_lms <- readRDS( file = here("sdmtmb", "sumflounder", "data", "post-check", "atlmackerel_ww_slopes.rds"))
# ## without wind (wind precluded)
# spr_wow_lms <- readRDS(file = here("sdmtmb", "sumflounder", "data", "post-check", "atlmackerel_wow_slopes.rds"))
spring_slopes <- readRDS(file = here(post.check.dat, "spring_slopes.rds"))

## CALCULATE OBSERVED STRATIFIED MEAN AND SLOPE ESTIMATES ####
# with wind
obs_stratmu_ww <- am_spring |>
  stratified.mean() |> 
  mutate(TYPE = "With Wind Included")

# without wind
obs_stratmu_wow <- am_spring |> 
  filter(AREA == "OUTSIDE") |> 
  stratified.mean() |> 
  mutate(TYPE = "With Wind Precluded")

obs_stratmu <- bind_rows(obs_stratmu_ww, obs_stratmu_wow)

saveRDS(obs_stratmu, here("sdmtmb", "atlmackerel", "data", "atlmackerel_obs_stratmu_rm200.rds"))

obs_slopes <- obs_stratmu |> 
  group_by(TYPE) |> 
  nest() |> 
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .)), 
         coef = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
  unnest(coef) |> 
  select(TYPE, term, estimate, conf.low, conf.high) |>
  filter(term == "EST_YEAR") |> 
  mutate(SEASON = "SPRING") 

saveRDS(obs_slopes, here("sdmtmb", "atlmackerel", "data", "atlmackerel_obs_slopes_rm200.rds"))


## CALCULATE CONFIDENCE INTERVALS OF SIMULATED ESTIMATES ####
# without wind (wind precluded)
spr.wow.ci <- spring_slopes |>
  filter(TYPE == "With Wind Precluded") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))
saveRDS(spr.wow.ci, here(post.check.dat, "atlmackerel_sim-wow-ci.rds"))


# with wind included
spr.ww.ci <- spring_slopes |>
  filter(TYPE == "With Wind Included") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))
saveRDS(spr.ww.ci, here(post.check.dat, "atlmackerel_sim-ww-ci.rds"))


## PLOT DISTRIBUTION OF SLOPES ####
### WITH WIND PRECLUDED: SPRING ####
spr_precl_plot <- ggplot(spring_slopes |> filter(TYPE == "With Wind Precluded")) +
  geom_histogram(aes(x = estimate, fill = "orange")) +
  scale_fill_manual(values = c("orange"), labels = c("With Wind Precluded"), name = NULL) + 
  shade_confidence_interval(endpoints = spr.wow.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[2,3]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.005, y = 60, label = str_c("Observed Slope", round(obs_slopes[2,3], 4), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")
saveRDS(spr_precl_plot, here(post.check.dat, "atlmackerel_sim-wow-plot.rds"))

### WITH WIND INCLUDED: SPRING ####
spr_incl_plot <- ggplot(spring_slopes |> filter(TYPE == "With Wind Included")) +
  geom_histogram(aes(x = estimate, fill = "#3f7f00")) + 
  scale_fill_manual(values = c("#3f7f00"), labels = c("With Wind Included"), name = NULL) +
  shade_confidence_interval(endpoints = spr.ww.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[2,3]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.005, y = 60, label = str_c("Observed Slope", round(obs_slopes[2,3], 4), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "Number of linear regression slopes") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")
saveRDS(spr_incl_plot, here(post.check.dat, "atlmackerel_sim-ww-plot.rds"))


## PATCHWORK ####
spr_dist_plots <- (spr_incl_plot + spr_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
saveRDS(spr_dist_plots, here(post.check.dat, "atlmackerel_sim-dist-plots.rds"))


### save plots
ggsave("atlmackerel-sim-slope-dists.png", plot = spr_dist_plots, device = "png", path = here(post.check.plots), width = 15, height = 10)

