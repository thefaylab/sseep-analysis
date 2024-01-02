### created: 03/06/2023
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

# sseep.analysis <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis"
post.check.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data/post-check"
post.check.plots <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/plots/post-check"

### LOAD DATA ####
# observed change in abundance index over time for summer flounder created here("sumflounder", "05-calculate-sumflounder-stratmu.R") 
obs_slopes <- readRDS( here("data", "sumflounder", "sf_obs_slopes.rds"))

# simulated changes in fall abundance indices over time for each scenario, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01a-fall-mod-ppcheck1.R") 
## with wind included
# fall_ww_lms <- readRDS(file = here("sdmtmb", "sumflounder", "data", "post-check", "fall_ww_slopes.rds"))
# ## without wind (wind precluded)
# fall_wow_lms <- readRDS(file = here("sdmtmb", "sumflounder", "data", "post-check", "fall_wow_slopes.rds"))
fall_slopes <- readRDS(file = here(post.check.dat, "fall_slopes.rds"))

# simulated changes in spring abundance indices over time for each scenario, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01b-spring-mod-ppcheck1.R") 
## with wind included
# spr_ww_lms <- readRDS( file = here("sdmtmb", "sumflounder", "data", "post-check", "spring_ww_slopes.rds"))
# ## without wind (wind precluded)
# spr_wow_lms <- readRDS(file = here("sdmtmb", "sumflounder", "data", "post-check", "spring_wow_slopes.rds"))
spring_slopes <- readRDS(file = here(post.check.dat, "spring_slopes.rds"))

## CALCULATE CONFIDENCE INTERVALS OF SIMULATED ESTIMATES ####
# without wind (wind precluded)
fall.wow.ci <- fall_slopes |>
  filter(TYPE == "With Wind Precluded") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

spr.wow.ci <- spring_slopes |>
  filter(TYPE == "With Wind Precluded") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

saveRDS(fall.wow.ci, here(post.check.dat, "fall_sim-wow-ci.rds"))
saveRDS(spr.wow.ci, here(post.check.dat, "spr_sim-wow-ci.rds"))

# fall_ci <- fall_slopes |> 
#   group_by(SEASON, TYPE) |> 
#   summarise(lower = quantile(estimate, 0.025),
#             upper = quantile(estimate, 0.975))

# with wind included
fall.ww.ci <- fall_slopes |>
  filter(TYPE == "With Wind Included") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

spr.ww.ci <- spring_slopes |>
  filter(TYPE == "With Wind Included") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

saveRDS(fall.ww.ci, here(post.check.dat, "fall_sim-ww-ci.rds"))
saveRDS(spr.ww.ci, here(post.check.dat, "spr_sim-ww-ci.rds"))

# spring_ci <- spring_slopes |> 
#   group_by(SEASON, TYPE) |> 
#   summarise(lower = quantile(estimate, 0.025),
#             upper = quantile(estimate, 0.975))


## PLOT DISTRIBUTION OF SLOPES ####
### WITH WIND PRECLUDED: FALL ####
fall_precl_plot <- ggplot(fall_slopes |> filter(TYPE == "With Wind Precluded")) +
  geom_histogram(aes(x = estimate), fill = "orange") + 
  scale_fill_manual(values = c("orange"), labels = c("With Wind Precluded"), name = NULL) +
  shade_confidence_interval(endpoints = fall.wow.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[4,4]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.056, y = 30, label = str_c("Observed Slope", round(obs_slopes[4,4], 2), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")

saveRDS(fall_precl_plot, here(post.check.dat, "fall_sim-precl-post-check_plot.rds"))

### WITH WIND INCLUDED: FALL ####
fall_incl_plot <- ggplot(fall_slopes |> filter(TYPE == "With Wind Included")) +
  geom_histogram(aes(x = estimate), fill = "#3f7f00") + 
  scale_fill_manual(values = c("#3f7f00"), labels = c("With Wind Included"), name = NULL) + 
  shade_confidence_interval(endpoints = fall.ww.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[4,4]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.056, y = 50, label = str_c("Observed Slope", round(obs_slopes[4,4], 2), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "Number of linear regression slopes") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")

saveRDS(fall_incl_plot, here(post.check.dat, "fall_sim-incl-post-check_plot.rds"))

### WITH WIND PRECLUDED: SPRING ####
spr_precl_plot <- ggplot(spring_slopes |> filter(TYPE == "With Wind Precluded")) +
  geom_histogram(aes(x = estimate), fill = "orange") +
  scale_fill_manual(values = c("orange"), labels = c("With Wind Precluded"), name = NULL) + 
  shade_confidence_interval(endpoints = spr.wow.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[3,4]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.026, y = 45, label = str_c("Observed Slope", round(obs_slopes[3,4], 2), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")

saveRDS(spr_precl_plot, here(post.check.dat, "spr_sim-precl-post-check_plot.rds"))

### WITH WIND INCLUDED: SPRING ####
spr_incl_plot <- ggplot(spring_slopes |> filter(TYPE == "With Wind Included")) +
  geom_histogram(aes(x = estimate), fill = "#3f7f00") + 
  scale_fill_manual(values = c("#3f7f00"), labels = c("With Wind Included"), name = NULL) +
  shade_confidence_interval(endpoints = spr.ww.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[3,4]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.0265, y = 45, label = str_c("Observed Slope", round(obs_slopes[3,4], 2), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "Number of linear regression slopes") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")

saveRDS(spr_incl_plot, here(post.check.dat, "fall_sim-incl-post-check_plot.rds"))

## PATCHWORK ####
dist_plots <- (fall_incl_plot + fall_precl_plot) / (spr_incl_plot + spr_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
saveRDS(dist_plots, here(post.check.dat, "sim-post-check_plots.rds"))

fall_dist_plots <- (fall_incl_plot + fall_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
saveRDS(fall_dist_plots, here(post.check.dat, "fall_sim-post-check_plots.rds"))

spr_dist_plots <- (spr_incl_plot + spr_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
saveRDS(spr_dist_plots, here(post.check.dat, "spr_sim-post-check_plots.rds"))


### save plots
ggsave("post-check-dists.png", plot = dist_plots, device = "png", path = here(post.check.plots), width = 15, height = 10)

ggsave("fall-post-check-dists.png", plot = fall_dist_plots, device = "png", path = here(post.check.plots), width = 15, height = 10)

ggsave("spr-post-check-dists.png", plot = spr_dist_plots, device = "png", path = here(post.check.plots), width = 15, height = 10)



# slopes <- bind_rows(obs_slopes, fall_wow_slope, spr_wow_slope, fall_ww_slope, spr_ww_slope) |>
#   mutate(ID = str_c(str_sub(SEASON,1,1), str_sub(TYPE, 11, 11), str_sub(METHOD, 1, 1))) |>
#   arrange(SEASON)
# 
# myLevels <- c("FIO", "FPO", "FIS", "FPS", "SIO", "SPO", "SIS", "SPS")
# slopes$ID <- factor(slopes$ID, levels = myLevels)
# 
# 
# slope_plot <- ggplot(slopes) + 
#   aes(x = factor(ID), y = slope, color = TYPE, shape = METHOD) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = lower, ymax = upper, color = TYPE, lty = TYPE), width = 0.2) +
#   #coord_flip() + 
#   labs(y = "slope", 
#        x = "Calculation Type") +
#   theme_minimal() +
#   theme(legend.position = "bottom") 
#   #facet_wrap(~SEASON)
# 
# ggsave(slope_plot, device = "png", path = here())

