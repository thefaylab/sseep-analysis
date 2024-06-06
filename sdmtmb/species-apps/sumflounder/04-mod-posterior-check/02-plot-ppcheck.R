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
source(here("R", "plot_fns.R"))

# sseep.analysis <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis"
post.check.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data/post-check"
post.check.plots <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/plots/post-check"

### LOAD DATA ####
# observed change in abundance index over time for summer flounder created here("sumflounder", "05-calculate-sumflounder-stratmu.R") 
obs_slopes <- readRDS(here("data", "sumflounder", "sf_obs_slopes.rds"))

# predicted changes in fall abundance indices over time for each scenario based on the fitted model, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01a-fall-mod-ppcheck.R") 
fall_pred_slopes <- readRDS(here(post.check.dat, "fall_predicted_slopes.rds"))

# redicted changes in fall abundance indices over time for each scenario based on the fitted model, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01b-spring-mod-ppcheck.R") 
spr_pred_slopes <- readRDS(here(post.check.dat, "spring_predicted_slopes.rds"))

# simulated changes in fall abundance indices over time for each scenario, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01a-fall-mod-ppcheck.R") 
fall_sim_slopes <- readRDS(file = here(post.check.dat, "fall_slopes.rds"))

# simulated changes in spring abundance indices over time for each scenario, created here("sdmtmb", "sumflounder", "04-mod-posterior-check", "01b-spring-mod-ppcheck.R") 
spring_sim_slopes <- readRDS(file = here(post.check.dat, "spring_sim_slopes.rds"))

## CALCULATE CONFIDENCE INTERVALS OF SIMULATED ESTIMATES ####
# without wind (wind precluded)
fall.wow.ci <- fall_sim_slopes |>
  filter(effort == "With Wind Precluded") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

spr.wow.ci <- spring_sim_slopes |>
  filter(effort == "With Wind Precluded") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

saveRDS(fall.wow.ci, here(post.check.dat, "fall_sim-wow-ci.rds"))
saveRDS(spr.wow.ci, here(post.check.dat, "spr_sim-wow-ci.rds"))


# with wind included
fall.ww.ci <- fall_sim_slopes |>
  filter(effort == "With Wind Included") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

spr.ww.ci <- spring_sim_slopes |>
  filter(effort == "With Wind Included") |>
  ungroup() |>
  summarise(lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

saveRDS(fall.ww.ci, here(post.check.dat, "fall_sim-ww-ci.rds"))
saveRDS(spr.ww.ci, here(post.check.dat, "spr_sim-ww-ci.rds"))


## PLOT DISTRIBUTION OF SLOPES ####
pal <- park_palette("SmokyMountains")

### SIMULATED VS TRUE ####
fall.precl_sim.v.true <- plot_distribution(fall_sim_slopes, fall.wow.ci, scenario = "With Wind Precluded") + 
  geom_vline(aes(xintercept = as.numeric(fall_pred_slopes[2,3]), linetype = effort, color = effort), linewidth = 1.5) +
  # annotate("text", x = -0.056, y = 30, label = str_c("Predicted Slope", round(fall_pred_slopes[2,3], 2), sep = ": "), angle = 90) + 
  scale_color_manual(values = c(pal[6]), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON))

fall.incl_sim.v.true <- plot_distribution(fall_sim_slopes, fall.ww.ci, scenario = "With Wind Included") + 
  geom_vline(aes(xintercept = as.numeric(fall_pred_slopes[2,3]), linetype = effort, color = effort), linewidth = 1.5) +
  # annotate("text", x = -0.056, y = 30, label = str_c("Predicted Slope", round(fall_pred_slopes[1,3], 2), sep = ": "), angle = 90) + 
  scale_color_manual(values = c(pal[6]), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON))

spr.precl_sim.v.true <- plot_distribution(spring_sim_slopes, spr.wow.ci, scenario = "With Wind Precluded") + 
  geom_vline(aes(xintercept = as.numeric(spr_pred_slopes[2,3]), linetype = effort, color = effort), linewidth = 1.5) +
  # annotate("text", x = -0.056, y = 30, label = str_c("Predicted Slope", round(spr_pred_slopes[2,3], 2), sep = ": "), angle = 90) + 
  scale_color_manual(values = c(pal[6]), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON))

spr.incl_sim.v.true <- plot_distribution(spring_sim_slopes, spr.ww.ci, scenario = "With Wind Included") + 
  geom_vline(aes(xintercept = as.numeric(spr_pred_slopes[2,3]), linetype = effort, color = effort), linewidth = 1.5) +
  # annotate("text", x = -0.056, y = 30, label = str_c("Predicted Slope", round(spr_pred_slopes[1,3], 2), sep = ": "), angle = 90) + 
  scale_color_manual(values = c(pal[6]), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON))


### SIMULATED VS OBSERVED ####
#### WITH WIND PRECLUDED: FALL ####
fall.precl_sim.v.obs. <- plot_distribution(fall_sim_slopes, fall.wow.ci, scenario = "With Wind Precluded") +
  geom_vline(aes(xintercept = as.numeric(obs_slopes[4,4]), linetype = effort, color = effort), linewidth = 1.5) +
  # annotate("text", x = -0.101, y = 30, label = str_c("Observed Slope", round(obs_slopes[4,4], 2), sep = ": "), angle = 90) +
  scale_color_manual(values = c(pal[6]), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON))
  
# saveRDS(fall_precl_plot, here(post.check.dat, "fall_sim-precl-post-check_plot.rds"))

#### WITH WIND INCLUDED: FALL ####
fall.incl_sim.v.obs <- plot_distribution(fall_sim_slopes, fall.ww.ci, scenario = "With Wind Included") +
  geom_vline(aes(xintercept = as.numeric(obs_slopes[4,4]), linetype = effort, color = effort), linewidth = 1.5) +
  # annotate("text", x = -0.101, y = 50, label = str_c("Observed Slope", round(obs_slopes[4,4], 2), sep = ": "), angle = 90) +
  scale_color_manual(values = c(pal[6]), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON))

# saveRDS(fall_incl_plot, here(post.check.dat, "fall_sim-incl-post-check_plot.rds"))

#### WITH WIND PRECLUDED: SPRING ####
spr.precl_sim.v.obs <- plot_distribution(spring_sim_slopes, spr.wow.ci, scenario = "With Wind Precluded") +
  geom_vline(aes(xintercept = as.numeric(obs_slopes[3,4]), linetype = effort, color = effort), linewidth = 1.5) +
  # annotate("text", x = -0.046, y = 45, label = str_c("Observed Slope", round(obs_slopes[3,4], 2), sep = ": "), angle = 90) +
  scale_color_manual(values = c(pal[6]), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON)) 

# saveRDS(spr_precl_plot, here(post.check.dat, "spr_sim-precl-post-check_plot.rds"))

#### WITH WIND INCLUDED: SPRING ####
spr.incl_sim.v.obs <- plot_distribution(spring_sim_slopes, spr.ww.ci, scenario = "With Wind Included") +
  geom_vline(aes(xintercept = as.numeric(obs_slopes[3,4]), linetype = effort, color = effort), linewidth = 1.5) +
  # annotate("text", x = -0.046, y = 45, label = str_c("Observed Slope", round(obs_slopes[3,4], 2), sep = ": "), angle = 90) +
  scale_color_manual(values = c(pal[6]), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON)) 
  
# 
# saveRDS(spr_incl_plot, here(post.check.dat, "fall_sim-incl-post-check_plot.rds"))

### PATCHWORK ####
#### SIMULATED VS TRUE ####
sim.v.true_plots <- (fall.incl_sim.v.true + fall.precl_sim.v.true) / (spr.incl_sim.v.true + spr.precl_sim.v.true) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# saveRDS(dist_plots, here(post.check.dat, "sim-post-check_plots.rds"))

fall_sim.v.true_plots <- (fall.incl_sim.v.true + fall.precl_sim.v.true) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# saveRDS(fall_dist_plots, here(post.check.dat, "fall_sim-post-check_plots.rds"))

spr_sim.v.true_plots <- (spr.incl_sim.v.true + spr.precl_sim.v.true) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# saveRDS(spr_dist_plots, here(post.check.dat, "spr_sim-post-check_plots.rds"))

#### SIMULATED VS OBSERVED ####
sim.v.obs_plots <- (fall.incl_sim.v.obs + fall.precl_sim.v.obs.) / (spr.incl_sim.v.obs + spr.precl_sim.v.obs) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# saveRDS(dist_plots, here(post.check.dat, "sim-post-check_plots.rds"))

fall_sim.v.obs_plots <- (fall.incl_sim.v.obs + fall.precl_sim.v.obs.) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# saveRDS(fall_dist_plots, here(post.check.dat, "fall_sim-post-check_plots.rds"))

spr_sim.v.obs_plots <- (spr.incl_sim.v.obs + spr.precl_sim.v.obs) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# saveRDS(spr_dist_plots, here(post.check.dat, "spr_sim-post-check_plots.rds"))


### save plots
ggsave("sim-v-true_dists.png", plot = sim.v.true_plots, device = "png", path = here(post.check.plots), width = 15, height = 10)

ggsave("fall_sim-v-true_dists.png", plot = fall_sim.v.true_plots, device = "png", path = here(post.check.plots), width = 10, height = 7)

ggsave("spr_sim-v-true_dists.png", plot = spr_sim.v.true_plots, device = "png", path = here(post.check.plots), width = 10, height = 7)

ggsave("sim-v-obs_dists.png", plot = sim.v.obs_plots, device = "png", path = here(post.check.plots), width = 15, height = 10)

ggsave("fall_sim-v-obs_dists.png", plot = fall_sim.v.obs_plots, device = "png", path = here(post.check.plots), width = 10, height = 7)

ggsave("spr_sim-v-obs_dists.png", plot = spr_sim.v.obs_plots, device = "png", path = here(post.check.plots), width = 10, height = 7)




