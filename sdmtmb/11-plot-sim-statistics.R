### created: 09/22/2024
### updated: 11/11/2024

# 11 - Plot mean differences ####

## OBJECTIVE ####


### Load Packages #### 
library(here)
library(tictoc)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
library(nationalparkcolors)
source(here("R", "StratMeanFXs_v2.R"))
source(here("R", "plot_fns.R"))

theme <- theme_bw(base_size = 16) + theme(legend.position = "bottom")
theme_set(theme)

### Environment Set-Up ####
# set season and species to be simulated
season <- "fall"

species <- "sumflounder"

# number of simulations 
nsims <- 1:1000

# set file locations of where to read in data and save outputs
dat.files <- here("data", "rds", "sdmtmb", species)
sim.dat <- here("data", "rds", "sdmtmb", species, "simulations", season)
plot.files <- here("outputs", "sdmtmb", species, "plots", "simulations")


### Load Data ####
# simulated abundance indices 
baseline_stratmu <- readRDS(here(sim.dat, "baseline",  str_c(season, species, "baseline", length(nsims), "sim.stratmu_rows.rds", sep = "_")))
enhanced_stratmu <- readRDS(here(sim.dat, "enhanced", str_c(season, species, "enhanced", length(nsims), "sim.stratmu_rows.rds", sep = "_")))
reduced_stratmu <- readRDS(here(sim.dat, "reduced", str_c(season, species, "reduced", length(nsims), "sim.stratmu_rows.rds", sep = "_")))


# simulated mean difference of abundance indices 
# no_wind_mudiff <- readRDS(here(sim.dat, "no-wind", str_c(season, species, "no-wind", length(nsims), "sim.stratmu_diff.rds", sep = "_")))
baseline_mudiff <- readRDS(here(sim.dat, "baseline",  str_c(season, species, "baseline", length(nsims), "sim.stratmu_diff.rds", sep = "_")))
enhanced_mudiff <- readRDS(here(sim.dat, "enhanced", str_c(season, species, "enhanced", length(nsims), "sim.stratmu_diff.rds", sep = "_")))
reduced_mudiff <- readRDS(here(sim.dat, "reduced", str_c(season, species, "reduced", length(nsims), "sim.stratmu_diff.rds", sep = "_")))

# simulated slopes 
# no_wind_slope<- readRDS(here(sim.dat, "no-wind", str_c(season, species, "no-wind", length(nsims), "sim.stratmu.slopes.rds", sep = "_")))
baseline_slope <- readRDS(here(sim.dat, "baseline", str_c(season, species, "baseline", length(nsims), "sim.stratmu.slopes.rds", sep = "_")))
enhanced_slope <- readRDS(here(sim.dat, "enhanced", str_c(season, species, "enhanced", length(nsims), "sim.stratmu.slopes.rds", sep = "_")))
reduced_slope <- readRDS(here(sim.dat, "reduced", str_c(season, species, "reduced", length(nsims), "sim.stratmu.slopes.rds", sep = "_")))

# simulated mean difference of slopes 
# no_wind_slope.diff <- readRDS(here(sim.dat, "no-wind", str_c(season, species, "no-wind", length(nsims), "sim.slope_diff.rds", sep = "_")))
baseline_slope.diff <- readRDS(here(sim.dat, "baseline", str_c(season, species, "baseline", length(nsims), "sim.slope_diff.rds", sep = "_")))
enhanced_slope.diff <- readRDS(here(sim.dat, "enhanced", str_c(season, species, "enhanced", length(nsims), "sim.slope_diff.rds", sep = "_")))
reduced_slope.diff <- readRDS(here(sim.dat, "reduced", str_c(season, species, "reduced", length(nsims), "sim.slope_diff.rds", sep = "_")))

# model predictions 
pred_stratmu <- readRDS(here(dat.files, "post-check", str_c(season, "preds_stratmus.rds", sep = "_")))
pred_slopes <- readRDS(here(dat.files, "post-check", str_c(season, "pred_slopes.rds", sep = "_")))

pred_strat_cols <- pred_stratmu |>
  select(!data) |> 
  unnest(cols = c(stratmu_sq, stratmu_precl), names_repair = "unique") |> 
  janitor::clean_names() |> 
  select(est_year_1, stratmu_2, cv_4, effort_5, season_6, stratmu_8, cv_10, effort_11)

pred_strat_error <- pred_strat_cols |> 
  calc.errors(observed = stratmu_8, expected = stratmu_2) |> 
  mean.diff()

pred_cv_error <- pred_strat_cols |> 
  calc.errors(observed = cv_4, expected = cv_10) |> 
  mean.diff()

pred_slope_err <- tibble("error" = (as.numeric(pred_slopes[2,3]) - as.numeric(pred_slopes[1,3])),
                         "abs.err" = abs(error),
                         "rel.err" = (error / as.numeric(pred_slopes[1,3])),
                         "abs.rel.err" = abs(rel.err))



## Bind statistics ####
# mean difference of abundance indices
# all_stratmu <- bind_rows(no_wind_stratmu, baseline_stratmu, enhanced_stratmu, reduced_stratmu)
all_stratmu <- bind_rows(baseline_stratmu, enhanced_stratmu, reduced_stratmu)

# slopes 
all_stratmu_list <- list(baseline_stratmu, enhanced_stratmu, reduced_stratmu) |> 
  map(~group_by(., EFFORT, SCENARIO, SIM) |> 
        nest() |> 
        mutate(model = map(data, ~lm(STRATMU~FUTURE_YEAR, data = .)),
               coefs = map(model, ~broom::tidy(., conf.int = TRUE))) |> 
        unnest(coefs) |>
        select(EFFORT, SCENARIO, SIM, term, estimate, statistic, p.value, conf.low, conf.high))

all_trends <- bind_rows(all_stratmu_list[[1]], all_stratmu_list[[2]], all_stratmu_list[[3]])

one_sim_trend <- all_trends |> filter(SIM == 1)

# mean difference of abundance indices
# all_mudiff <- bind_rows(no_wind_mudiff, baseline_mudiff, enhanced_mudiff, reduced_mudiff)
all_mudiff <- bind_rows(baseline_mudiff, enhanced_mudiff, reduced_mudiff)

# simulated slopes
# all_slopes<- bind_rows(no_wind_slope, baseline_slope, enhanced_slope, reduced_slope)
all_slopes <- bind_rows(baseline_slope, enhanced_slope, reduced_slope)

# mean difference of slopes
# all_slopes.diff <- bind_rows(no_wind_slope.diff, baseline_slope.diff, enhanced_slope.diff, reduced_slope.diff)
all_slopes.diff <- bind_rows(baseline_slope.diff, enhanced_slope.diff, reduced_slope.diff)


## Plots ####
# myLevels <- c("no-wind", "baseline", "enhanced", "reduced")
pal1 <- park_palette("Zion")
pal2 <- park_palette("SmokyMountains")

# single simulation 
base_plot <- all_stratmu |> 
  rename(stratmu = STRATMU,
         stratvar = STRATVAR) |> 
  filter(SIM == 1, SCENARIO == "baseline") |>
  plot.stratmu(year_col = FUTURE_YEAR, color = EFFORT) + 
  geom_abline(slope = one_sim_trend$estimate[2], intercept = one_sim_trend$estimate[1], color = "#548F01", linewidth = 0.75, linetype = "dashed", show.legend = TRUE) +
  geom_abline(slope = one_sim_trend$estimate[4], intercept = one_sim_trend$estimate[3], color = "#D58A60", linewidth = 0.75, linetype = "dashed", show.legend = TRUE) +
  # scale_color_identity(labels=c("With Wind Included", "With Wind Precluded"), guide="legend") +
  # scale_colour_manual(name='Trends',
  #                     labels = c("With Wind Included", "With Wind Precluded"), 
  #                     values=c("#548F01", "#D58A60"))+
  facet_wrap(~str_to_title(SCENARIO)) 

enhanced_plot <- all_stratmu |> 
  rename(stratmu = STRATMU,
         stratvar = STRATVAR) |> 
  filter(SIM ==1, SCENARIO == "enhanced") |>
  plot.stratmu(year_col = FUTURE_YEAR, color = EFFORT) + 
  geom_abline(slope = one_sim_trend$estimate[6], intercept = one_sim_trend$estimate[5], color = "#548F01", linewidth = 0.75, linetype = "dashed") +
  geom_abline(slope = one_sim_trend$estimate[8], intercept = one_sim_trend$estimate[7], color = "#D58A60", linewidth = 0.75, linetype = "dashed") + labs(y = "") +
  facet_wrap(~str_to_title(SCENARIO)) 

reduced_plot <- all_stratmu |> 
  rename(stratmu = STRATMU,
         stratvar = STRATVAR) |> 
  filter(SIM == 1, SCENARIO == "reduced") |>
  plot.stratmu(year_col = FUTURE_YEAR, color = EFFORT) + 
  geom_abline(slope = one_sim_trend$estimate[10], intercept = one_sim_trend$estimate[9], color = "#548F01", linewidth = 0.75, linetype = "dashed") +
  geom_abline(slope = one_sim_trend$estimate[12], intercept = one_sim_trend$estimate[11], color = "#D58A60", linewidth = 0.75, linetype = "dashed") + labs(y = "") +
  facet_wrap(~str_to_title(SCENARIO)) 

one_sim_scenario_plot <- (base_plot + enhanced_plot + reduced_plot) + 
  plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")


# mean difference of abundance indices
mudiff_plot <- ggplot(all_mudiff) + 
  geom_boxplot(aes(x = str_to_title(SCENARIO), y = MARE_perc, color = SCENARIO), linewidth = 0.65)  +
  geom_hline(data = pred_strat_error, aes(yintercept = MARE*100), linetype = "longdash", alpha = 0.75) +
  scale_color_manual(values = c(pal1[5], pal1[1], pal1[4]), labels = c("Baseline", "Enhanced", "Reduced"), name = "") +
  ylim(0, NA) +
  facet_wrap(~str_to_title(season)) + 
  labs(x = "Productivity Scenario", y = "Mean absolute relative difference") + 
  theme(legend.position = "bottom")

# mean difference of cvs
cv_diff_plot <- ggplot(all_stratmu) + 
  geom_boxplot(aes(x = str_to_title(SCENARIO), y = CV, color = EFFORT), linewidth = 0.65)  +
  scale_color_manual(values = c(pal2[2], pal2[1]), labels = c( "With Wind Included", "With Wind Precluded"), name = "") +
  ylim(0, NA) +
  facet_wrap(~str_to_title(season)) + 
  labs(x = "Productivity Scenario", y = "Coefficient of Variation") + 
  theme(legend.position = "bottom")

# mean difference of slopes 
slope_diff_plot <- ggplot(all_slopes.diff) + 
  geom_boxplot(aes(x = str_to_title(SCENARIO), y = abs(ME), color = SCENARIO), linewidth = 0.65)  +
  geom_hline(data = pred_slope_err, aes(yintercept = abs.err), linetype = "longdash", alpha = 0.75) +
  scale_color_manual(values = c(pal1[5], pal1[1], pal1[4]), labels = c("Baseline", "Enhanced", "Reduced"), name = "") +
  ylim(0, NA) +
  facet_wrap(~str_to_title(season)) + 
  labs(x = "Productivity Scenario", y = "Mean absolute difference") + 
  theme(legend.position = "bottom")



## Save the data ####
id <- str_c(season, species, length(nsims), "productivity", sep = "_")

data <- list(all_stratmu, 
             all_trends, 
             all_mudiff, 
             all_slopes, 
             all_slopes.diff)
  
pmap(list(data, names(data)), ~saveRDS(.x, here(sim.dat, .y)))

plots <- list(one_sim_scenario_plot,
              mudiff_plot, 
              cv_diff_plot, 
              slope_diff_plot)

names(plots) <- c(str_c(season, species, "one_sim_scenario_plot.png", sep = "_"), 
              str_c(id, "mudiff_plot.png", sep = "_"), 
              str_c(id, "cv_diff_plot.png", sep = "_"), 
              str_c(id, "slope_diff_plot.png", sep = "_"))

pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = .y, device = "png", path = here(plot.files), width = 12, height = 8))
