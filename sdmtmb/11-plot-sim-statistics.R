### created: 09/22/2024
### updated: 10/16/2024

# 11 - Plot mean differences ####

## OBJECTIVE ####


### Load Packages #### 
library(here)
library(tictoc)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
library(nationalparkcolors)
source(here("R", "StratMeanFXs_v2.R"))
theme_set(theme_bw())


### Environment Set-Up ####
# set season and species to be simulated
season <- "spring"

species <- "sumflounder"

# # type of scenario simulated - c("no-wind", "baseline", "enhanced", "reduced")
# scenario <- "baseline"

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

# simulated mean difference of cvs 
# no_wind_cv.diff <- readRDS(here(sim.dat, "no-wind", str_c(season, species, "no-wind", length(nsims), "sim.cv_diff.rds", sep = "_")))
# baseline_cv.diff <- readRDS(here(sim.dat, "baseline", str_c(season, species, "baseline", length(nsims), "sim.cv_diff.rds", sep = "_")))
# enhanced_cv.diff <- readRDS(here(sim.dat, "enhanced", str_c(season, species, "enhanced", length(nsims), "sim.cv_diff.rds", sep = "_")))
# reduced_cv.diff <- readRDS(here(sim.dat, "reduced", str_c(season, species, "reduced", length(nsims), "sim.cv_diff.rds", sep = "_")))

# simulated mean difference of slopes 
# no_wind_slope.diff <- readRDS(here(sim.dat, "no-wind", str_c(season, species, "no-wind", length(nsims), "sim.slope_diff.rds", sep = "_")))
baseline_slope.diff <- readRDS(here(sim.dat, "baseline", str_c(season, species, "baseline", length(nsims), "sim.slope_diff.rds", sep = "_")))
enhanced_slope.diff <- readRDS(here(sim.dat, "enhanced", str_c(season, species, "enhanced", length(nsims), "sim.slope_diff.rds", sep = "_")))
reduced_slope.diff <- readRDS(here(sim.dat, "reduced", str_c(season, species, "reduced", length(nsims), "sim.slope_diff.rds", sep = "_")))


## Bind statistics ####
# mean difference of abundance indices
# all_stratmu <- bind_rows(no_wind_stratmu, baseline_stratmu, enhanced_stratmu, reduced_stratmu)
all_stratmu <- bind_rows(baseline_stratmu, enhanced_stratmu, reduced_stratmu)


# mean difference of abundance indices
# all_mudiff <- bind_rows(no_wind_mudiff, baseline_mudiff, enhanced_mudiff, reduced_mudiff)
all_mudiff <- bind_rows(baseline_mudiff, enhanced_mudiff, reduced_mudiff)

# mean difference of cvs 
# all_cv.diff <- bind_rows(no_wind_cv.diff, baseline_cv.diff, enhanced_cv.diff, reduced_cv.diff)
# all_cv.diff <- bind_rows(baseline_cv.diff, enhanced_cv.diff, reduced_cv.diff)

# mean difference of slopes
# all_slopes.diff <- bind_rows(no_wind_slope.diff, baseline_slope.diff, enhanced_slope.diff, reduced_slope.diff)
all_slopes.diff <- bind_rows(baseline_slope.diff, enhanced_slope.diff, reduced_slope.diff)


## Plots ####
# myLevels <- c("no-wind", "baseline", "enhanced", "reduced")
pal1 <- park_palette("Zion")
pal2 <- park_palette("SmokyMountains")

# mean difference of abundance indices
mudiff_plot <- ggplot(all_mudiff) + 
  geom_boxplot(aes(x = str_to_title(SCENARIO), y = MARE_perc, color = SCENARIO), linewidth = 0.65)  +
  scale_color_manual(values = c(pal1[4], pal1[1], pal1[6]), labels = c("Baseline", "Enhanced", "Reduced"), name = "") +
  ylim(0, NA) +
  facet_wrap(~str_to_title(season)) + 
  labs(x = "Productivity Scenario", y = "Mean absolute relative difference (%)") + 
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
  geom_boxplot(aes(x = str_to_title(SCENARIO), y = abs(ME)*100, color = SCENARIO), linewidth = 0.65)  +
  scale_color_manual(values = c(pal1[4], pal1[1], pal1[6]), labels = c("Baseline", "Enhanced", "Reduced"), name = "") +
  ylim(0, NA) +
  facet_wrap(~str_to_title(season)) + 
  labs(x = "Productivity Scenario", y = "Mean absolute difference (%)") + 
  theme(legend.position = "bottom")

## Save the data ####
id <- str_c(season, species, length(nsims), "productivity", sep = "_")

plots <- list(mudiff_plot, 
              cv_diff_plot, 
              slope_diff_plot)

names(plots) <- c(str_c(id, "mudiff_plot.png", sep = "_"), 
              str_c(id, "cv_diff_plot.png", sep = "_"), 
              str_c(id, "slope_diff_plot.png", sep = "_"))

pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = .y, device = "png", path = here(plot.files), width = 10, height = 8))
