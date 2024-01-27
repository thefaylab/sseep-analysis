### created: 07/27/2023
### last updated: 01/26/2024

#  04 - NULL HYPOTHESIS TESTING: DISTRIBUTION OF SLOPES  ####

## OBJECTIVE ####
# The slopes and their confidence intervals from the two methods are plotted based on season and whether wind tows were included in the calculation or not.   

### LOAD PACKAGES #### 
library(tidyverse)
library(here)
library(patchwork)
library(boot)
library(infer)
library(broom)
library(nationalparkcolors)
theme_set(theme_bw())

pal <- park_palette("SmokyMountains")
null.hyp.data <- here("data", "sumflounder", "null-hypothesis")
sumflounder.dat <- here("data", "sumflounder")
sumflounder.plots <- here("outputs", "sumflounder")

set.seed(120)

### LOAD DATA #### 
# slope estimates from linear regressions of resampled stratified mean abundance indices over time when all tows are included created here("sumflounder", "null-hypothesis", "03-resampled-linear-regression.R") 
fullreps_mod <- readRDS(here(null.hyp.data, "fullresamp_mods.rds"))

# slope estimates from linear regressions of resampled stratified mean abundance indices over time when wind tows are precluded created here("sumflounder", "null-hypothesis", "03-resampled-linear-regression.R") 
windreps_mod <- readRDS(here(null.hyp.data, "windresamp_mods.rds"))

# slope estimates from linear regressions of the observed stratified mean abundance indices over time from the historical times series for both scenarios, included and precluded, created here("sumflounder", "05-calculate-sumflounder-stratmu.R")
obs_slopes <- readRDS(here(sumflounder.dat, "sf_obs_slopes.rds"))

## DATA WRANGLE ####
### WITH WIND INCLUDED ####
#### FALL ####
# select fall slopes only
fall_incl_dist <- fullreps_mod |>
  ungroup() |>
  select(SEASON, estimate, replicate, TYPE) |> 
  filter(SEASON == "FALL") 


# identify 95% confidence intervals of fall slope distribution
fall_incl_ci <- fall_incl_dist |> 
  summarise(lower = quantile(estimate, 0.025), 
            upper = quantile(estimate, 0.975))

#### SPRING ####
# select spring slopes only
spr_incl_dist <- fullreps_mod |>
  ungroup() |>
  select(SEASON, estimate, replicate, TYPE) |> 
  filter(SEASON == "SPRING") 

# identify 95% confidence intervals of spring slope distribution
spr_incl_ci <- spr_incl_dist |> 
  summarise(lower = quantile(estimate, 0.025), 
            upper = quantile(estimate, 0.975))

### WITH WIND PRECLUDED ####
#### FALL ####
# select fall slopes only
fall_precl_dist <- windreps_mod |>
  ungroup() |>
  select(SEASON, estimate, replicate, TYPE) |> 
  filter(SEASON == "FALL") 

# identify 95% confidence intervals of fall slope distribution
fall_precl_ci <- fall_precl_dist |> 
  summarise(lower = quantile(estimate, 0.025), 
            upper = quantile(estimate, 0.975))

#### SPRING ####
# select spring slopes only
spr_precl_dist <- windreps_mod |>
  ungroup() |>
  select(SEASON, estimate, replicate, TYPE) |> 
  filter(SEASON == "SPRING") 

# identify 95% confidence intervals of spring slope distribution
spr_precl_dist_ci <- spr_precl_dist |> 
  summarise(lower = quantile(estimate, 0.025), 
            upper = quantile(estimate, 0.975))

## DISTRIBUTION PLOTS ####

### WITH WIND INCLUDED: FALL ####
fall_incl_plot <- ggplot(fall_incl_dist) +
  geom_histogram(aes(x = estimate, fill = pal[4])) + 
  scale_fill_manual(values = c(pal[4]), labels = c("With status quo survey effort"), name = NULL) + 
  shade_confidence_interval(endpoints = fall_incl_ci, color = pal[3], fill = pal[3], alpha = 0.45) +
  # geom_vline(xintercept = as.numeric(obs_slopes[4,4]), linetype = 6, color = pal[6], linewidth = 1.5) +
### uncaption/caption below to add/remove legend item for dotted line and caption in/out above 
  geom_vline(aes(xintercept = as.numeric(obs_slopes[4,4]), linetype = TYPE, color = TYPE), linewidth = 1.5) +
  scale_color_manual(values = c(pal[6]), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON)) +
  ylim(0,NA)+
### caption labels on/of below to add x-axis title and caption on/off above to remove
  # labs(x = "", y = "") +
  labs(x = str_wrap("Linear regression slope estimates of abundance index", width = 30, whitespace_only = FALSE), y = str_wrap("Number of linear regression slope estimates", width = 30, whitespace_only = FALSE)) +
  # labs(x = "", y = str_wrap("Number of linear regression slope estimates", width = 30, whitespace_only = FALSE)) +
  theme(axis.title = element_text(size = 16), axis.title.x =  element_text(margin = margin(10, 0, 0, 10)), axis.title.y =  element_text(margin = margin(0, 10, 0, 5)), axis.text = element_text(size = 14), legend.position = "bottom", legend.text = element_text(size = 14), strip.text = element_text(size = 16))


### WITH WIND INCLUDED: SPRING ####
spr_incl_plot <- ggplot(spr_incl_dist) +
  geom_histogram(aes(x = estimate, fill = pal[4])) + 
  scale_fill_manual(values = c(pal[4]), labels = c("With status quo survey effort"), name = NULL) + 
  shade_confidence_interval(endpoints = spr_incl_ci, color = pal[3], fill = pal[3], alpha = 0.45) +
  # geom_vline(xintercept = as.numeric(obs_slopes[3,4]), linetype = 6, color = pal[6], linewidth = 1.5) +
### uncaption/caption below to add/remove legend item for dotted line and caption in/out above 
  geom_vline(aes(xintercept = as.numeric(obs_slopes[3,4]), linetype = TYPE, color = TYPE), linewidth = 1.5) +
  scale_color_manual(values = c(pal[6]), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON)) +
  ylim(0,NA)+
  # labs(x = str_wrap("Linear regression slope estimates of abundance index", width = 30, whitespace_only = FALSE), y = str_wrap("Number of linear regression slope estimates", width = 30, whitespace_only = FALSE))+ 
  labs(x = str_wrap("Linear regression slope estimates of abundance index", width = 30, whitespace_only = FALSE), y = "") +
  # theme(axis.title = element_text(size = 16), axis.title.x =  element_text(margin = margin(10, 0, 0, 10)), axis.text =  element_text(size = 14), legend.position = "bottom", legend.text = element_text(size = 14), strip.text = element_text(size = 16)) + 
  theme(axis.title = element_text(size = 16), axis.title.x =  element_text(margin = margin(10, 0, 0, 10)), axis.text =  element_text(size = 14), legend.position = element_blank(), legend.text = element_text(size = 14), strip.text = element_text(size = 16))


### WITH WIND PRECLUDED: FALL ####
fall_precl_plot <- ggplot(fall_precl_dist) +
  geom_histogram(aes(x = estimate, fill = pal[1])) + 
  scale_fill_manual(values = c(pal[1]), labels = c("With survey effort precluded"), name = NULL) + 
  shade_confidence_interval(endpoints = fall_precl_ci, color = pal[3], fill = pal[3], alpha = 0.45) +
  geom_vline(aes(xintercept = as.numeric(obs_slopes[4,4]), linetype = TYPE, color = TYPE), linewidth = 1.5) +
  scale_color_manual(values = c(pal[6]), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) + 
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) + 
  facet_wrap(~str_to_title(SEASON)) +
  ylim(0,150) +
  # labs(x = "", y = "") +
### caption labels on/of below to add x-axis title and caption on/off above to remove
  labs(x = str_wrap("Linear regression slope estimates of abundance index", width = 30, whitespace_only = FALSE), y = "") +
  theme(title = element_text(size = 16), legend.position = "bottom",axis.title.x =  element_text(margin = margin(10, 0, 0, 10)), axis.text =  element_text(size = 14), legend.text = element_text(size = 14), strip.text = element_text(size = 16))



### WITH WIND PRECLUDED: SPRING ####
spr_precl_plot <- ggplot(spr_precl_dist) +
### move fill in and out of aes if want to use scale_fill_manual
  geom_histogram(aes(x = estimate, fill = pal[1])) + 
  scale_fill_manual(values = c(pal[1]), labels = c("With Wind Precluded"), name = NULL) +
  shade_confidence_interval(endpoints = spr_precl_dist_ci, color = pal[3], fill = pal[3], alpha = 0.45) +
  # geom_vline(xintercept = as.numeric(obs_slopes[3,4]), linetype = 6, color = pal[6], linewidth = 1.5) +
### uncaption/caption below to add/remove legend item for dotted line and caption in/out above 
  geom_vline(aes(xintercept = as.numeric(obs_slopes[3,4]), linetype = TYPE, color = TYPE), linewidth = 1.5) +
  scale_color_manual(values = c(pal[6]), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Observed linear regression slope\nof the precluded abundance index"), name = NULL) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = str_wrap("Linear regression slope estimates of abundance index", width = 30, whitespace_only = FALSE), y = "") +
  ylim(0, 150) +
  # theme(title = element_text(size = 16), legend.position = "bottom",axis.title.x =  element_text(margin = margin(10, 0, 0, 10)), axis.text =  element_text(size = 14), legend.text = element_text(size = 14), strip.text = element_text(size = 16))
theme(axis.title = element_text(size = 16), axis.title.x =  element_text(margin = margin(10, 0, 0, 10)), axis.text =  element_text(size = 14), legend.position = "bottom", legend.text = element_text(size = 14), strip.text = element_text(size = 16))




## PATCHWORK ####
dist_plots <- (fall_incl_plot + fall_precl_plot) / (spr_incl_plot + spr_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fall_dist_plots <- (fall_incl_plot + fall_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

spr_dist_plots <- (spr_incl_plot + spr_precl_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

precl_plots <- (fall_precl_plot + spr_precl_plot) + plot_annotation(title = "Distribution of changes in resampled abundance indices over time") + plot_layout(guides = "collect") & theme(title = element_text(size = 12), axis.title = element_text(size = 10), legend.position = "bottom")

incl_plots <- ((fall_incl_plot + theme(plot.margin = unit(c(5, 2, 5, 5), "pt"))) + (spr_incl_plot + theme(plot.margin = unit(c(5,5,5,2), "pt")))) + plot_annotation(title = "Distribution of changes in resampled abundance indices over time", theme = theme(plot.title = element_text(size = 18, hjust = 0.5, margin = margin(5, 0, 10, 0)))) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


### save plots
ggsave("lin-reg-slope-dists.png", plot = dist_plots, device = "png", path = here(sumflounder.plots), width = 15, height = 10)

ggsave("fall-lin-reg-dists.png", plot = fall_dist_plots, device = "png", path = here(sumflounder.plots), width = 15, height = 10)

ggsave("spr-lin-reg-dists.png", plot = spr_dist_plots, device = "png", path = here(sumflounder.plots), width = 15, height = 10)

ggsave("incl-lin-reg-dists.png", plot = incl_plots, device = "png", path = here(sumflounder.plots), width = 10, height = 5)
