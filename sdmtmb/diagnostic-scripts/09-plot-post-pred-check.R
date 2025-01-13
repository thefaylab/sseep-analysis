### created: 10/31/2024
### last updated: 

# 09 - DIAGNOSTICS: PLOT POSTERIOR PREDICTIVE CHECK ####

## OBJECTIVE ####
# plot distribution of simulated trend estimates from the model fit compared to the model-predicted trend when survey effort is precluded by wind
# calculate p-value for differences between simulated data and predicted data and  the quantile of the cumulative distribution 

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(infer)
library(nationalparkcolors)
library(patchwork)
set.seed(123)
theme_set(theme_bw())

### Environment Set Up ####
# season 
season <- "spring"

# species
species <- "atlmackerel"

species_name <- "atlantic mackerel"

### File locations ####
dat.files <- here("data", "rds", "sdmtmb",  species)  
post.check <- here(dat.files, "post-check") 
plot.files <- here("outputs", "sdmtmb", species, "plots", "post-check")

### Read in data ####
names <- c("pred_slopes.rds", "post-check_sim-slopes.rds")

slopes <- map(names, ~readRDS(here(post.check, str_c(season, .x, sep = "_"))))

pred_slopes <- slopes[[1]]
sim_slopes <- slopes[[2]]

## CALCULATE CONFIDENCE INTERVALS OF SIMULATED ESTIMATES ####
sim_cis <- sim_slopes |> 
  group_by(effort, season) |> 
  nest() |> 
  mutate(lower = map(data, ~quantile(.$estimate, 0.025)),
         upper = map(data, ~quantile(.$estimate, 0.975))) |> 
  unnest(cols = c(lower, upper))

## CALCULATE DISTRIBUTION QUANTITIES ####
# extract precluded slope only
pred_precl_slope <- filter(pred_slopes, effort == "With Wind Precluded") |> 
  ungroup() |> 
  select(estimate)

# Permutation value
pval <- sim_slopes |>
  group_by(effort) |>
  nest() |>
  bind_cols(pred_precl_slope) |>
  mutate(test_ct = map2(data, estimate, ~filter(., .x$estimate >= .y) |> nrow()),
         pval = map(test_ct, ~ .x / 1000)) |>
  select(effort, pval) |>
  unnest(cols = pval)

# Representative Quantile
# the location of the predicted slope within the distribution 
quant <- sim_slopes |>
  group_by(effort) |>
  nest() |>
  mutate(quant.fn = map(data, ~ecdf(.x$estimate)), # for each set of effort data find the CDF
         quant = map(quant.fn, ~.x(pred_precl_slope$estimate))) |> # use the resulting CDF and find the representative quantile
  unnest(cols = quant)


## PLOT POSTERIOR PREDICTIVE CHECK ####
source(here("R", "plot_fns.R"))
pal <- park_palette("SmokyMountains")

# Simulated status quo vs predicted preclusion estimates
sim.v.pred_sq.plot <- plot_distribution(sim_slopes, ci_dat = c(sim_cis$lower[1], sim_cis$upper[1])) + 
  geom_vline(aes(xintercept = as.numeric(pred_precl_slope$estimate), linetype = effort, color = effort), linewidth = 1.5) +
  scale_color_manual(values = c("black"), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  theme(text = element_text(size = 12), strip.text = element_text(size = 12), legend.position = "bottom") +
  annotate("text", x = as.numeric(pred_precl_slope$estimate)-0.015, y = 60, label = str_c(quant$quant[1]*100, "%", sep = ""), angle = 90, size = 5) +
  facet_wrap(~str_to_title(season)) +
  ylim(0,115)

# Simulated preclusion vs predicted preclusion estimates
sim.v.pred_precl.plot <- plot_distribution(sim_slopes, ci_dat = c(sim_cis$lower[2], sim_cis$upper[2]), scenario = "With Wind Precluded") +
  geom_vline(aes(xintercept = as.numeric(pred_precl_slope$estimate), linetype = effort, color = effort), linewidth = 1.5) +
  scale_color_manual(values = c("black"), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linetype_manual(values = c(6), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  scale_linewidth(range = c(1,1), labels = c("Model predicted linear regression slope\nof the precluded abundance index"), name = NULL) +
  theme(text = element_text(size = 12), strip.text = element_text(size = 12), legend.position = "bottom") +
  annotate("text", x = as.numeric(pred_precl_slope$estimate)-0.015, y = 60, label = str_c(quant$quant[2]*100, "%", sep = ""), angle = 90, size = 5) +
  facet_wrap(~str_to_title(season)) +
  ylim(0, 115)
  
# patchwork 
patchwork <- sim.v.pred_sq.plot + sim.v.pred_precl.plot + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

### Save the data ####
plots <- list("sim-pred_sq-plot.png" = sim.v.pred_sq.plot,
              "sim-pred_precl-plot.png" = sim.v.pred_precl.plot,
              "sim-pred_plots.png" = patchwork)

pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = str_c(season, .y, sep = "_"), device = "png", path = here(plot.files), width = 10, height = 6))


data <- list("post-check_conf-int.rds" = sim_cis,
             "post-pred_pvalue.rds" = pval, 
             "post-pred_quantile.rds" = quant) 

pmap(list(data, names(data)), ~saveRDS(.x, here(post.check, str_c(season, .y, sep = "_"))))
