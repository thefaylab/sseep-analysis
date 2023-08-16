### created: 03/06/2023
### last updated: 08/16/2023

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

sseep.analysis <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis"

### LOAD DATA ####
# observed change in abundance index over time. created here(sseep.analysis, "retro-analysis", "05-AI-linear-regressions.R") and filtered for summer flounder
obs_slopes <- readRDS( here("data", "sumflounder", "sf_obs-slopes.rds"))

# simulated changes in fall abundance indices over time for each scenario, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01a-fall-mod-ppcheck1.R") 
## with wind included
fall_ww_lms <- readRDS(file = here("sdmtmb", "sumflounder", "data", "post-check", "fall_ww_slopes.rds"))
## without wind (wind precluded)
fall_wow_lms <- readRDS(file = here("sdmtmb", "sumflounder", "data", "post-check", "fall_wow_slopes.rds"))

# simulated changes in spring abundance indices over time for each scenario, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01b-spring-mod-ppcheck1.R") 
## with wind included
spr_ww_lms <- readRDS( file = here("sdmtmb", "sumflounder", "data", "post-check", "spring_ww_slopes.rds"))
## without wind (wind precluded)
spr_wow_lms <- readRDS(file = here("sdmtmb", "sumflounder", "data", "post-check", "spring_wow_slopes.rds"))

## CALCULATE CONFIDENCE INTERVALS OF SIMULATED ESTIMATES ####
# without wind (wind precluded)
fall.wow.ci <- fall_wow_lms %>%
  ungroup() %>%
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

spr.wow.ci <- spr_wow_lms %>%
  ungroup() %>%
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))


# with wind included
fall.ww.ci <- fall_ww_lms %>%
  ungroup() %>%
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

spr.ww.ci <- spr_ww_lms %>%
  ungroup() %>%
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))


## PLOT DISTRIBUTION OF SLOPES ####

### WITH WIND PRECLUDED: FALL ####
fall_precl_plot <- ggplot(fall_wow_lms) +
  geom_histogram(aes(x = estimate, fill = "orange")) + 
  scale_fill_manual(values = c("orange"), labels = c("With Wind Precluded"), name = NULL) +
  shade_confidence_interval(endpoints = fall.wow.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[2,6]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.066, y = 60, label = str_c("Observed Slope", round(obs_slopes[2,6], 4), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")

### WITH WIND INCLUDED: FALL ####
fall_incl_plot <- ggplot(fall_ww_lms) +
  geom_histogram(aes(x = estimate, fill = "#3f7f00")) + 
  scale_fill_manual(values = c("#3f7f00"), labels = c("With Wind Included"), name = NULL) + 
  shade_confidence_interval(endpoints = fall.ww.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[2,6]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.066, y = 100, label = str_c("Observed Slope", round(obs_slopes[2,6], 4), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "Number of linear regression slopes") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")

### WITH WIND PRECLUDED: SPRING ####
spr_precl_plot <- ggplot(spr_wow_lms) +
  geom_histogram(aes(x = estimate, fill = "orange")) +
  scale_fill_manual(values = c("orange"), labels = c("With Wind Precluded"), name = NULL) + 
  shade_confidence_interval(endpoints = spr.wow.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[4,6]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.045, y = 45, label = str_c("Observed Slope", round(obs_slopes[4,6], 4), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")

### WITH WIND INCLUDED: SPRING ####
spr_incl_plot <- ggplot(spr_ww_lms) +
  geom_histogram(aes(x = estimate, fill = "#3f7f00")) + 
  scale_fill_manual(values = c("#3f7f00"), labels = c("With Wind Included"), name = NULL) +
  shade_confidence_interval(endpoints = spr.ww.ci, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[4,6]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  annotate("text", x = -0.045, y = 45, label = str_c("Observed Slope", round(obs_slopes[4,6], 4), sep = ": "), angle = 90) +
  facet_wrap(~SEASON) +
  labs(x = "Linear regression slope of abundance index", y = "Number of linear regression slopes") +
  theme_bw() +
  theme(title = element_text(size = 10), legend.position = "bottom")


## PATCHWORK ####
dist_plots <- (fall_incl_plot + fall_precl_plot) / (spr_incl_plot + spr_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fall_dist_plots <- (fall_incl_plot + fall_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

spr_dist_plots <- (spr_incl_plot + spr_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


### save plots
ggsave("sim-lin-reg-dists.png", plot = dist_plots, device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 15, height = 10)

ggsave("fall-sim-slope-dists.png", plot = fall_dist_plots, device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 15, height = 10)

ggsave("spr-sim-slope-dists.png", plot = spr_dist_plots, device = "png", path = here("sdmtmb", "sumflounder", "plots"), width = 15, height = 10)



# slopes <- bind_rows(obs_slopes, fall_wow_slope, spr_wow_slope, fall_ww_slope, spr_ww_slope) %>%
#   mutate(ID = str_c(str_sub(SEASON,1,1), str_sub(TYPE, 11, 11), str_sub(METHOD, 1, 1))) %>%
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

