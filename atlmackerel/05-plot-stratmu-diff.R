### created: 01/26/2024
### last updated: 11/10/2024

# 05 - PLOT ATLANTIC MACKEREL MEAN DIFFERENCES ####


## OBJECTIVE ####
# plot the mean relative difference between survey effort scenario indices for Atlantic mackerel compared to the relative differences across all species observed on the bottom trawl survey



### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(kableExtra)
library(patchwork)

### dependencies ####
# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()


### LOAD DATA ####
# dataset created from `03-strat-mu-diff.R` here("retro-analysis"). 
# datasets created from `03-strat-mu-diff.R` here("retro-analysis"). 
mudiff_all <- readRDS(here("data", "rds", "retro-analysis", "species_stratmu-diff.rds")) 

cvdiff_all <- readRDS(here("data", "rds", "retro-analysis", "species_cv-diff.rds")) 

slope_diff_all <- readRDS(here("data", "rds", "retro-analysis", "species_slope-diff.rds"))

# dataset created from `05-calculate-sumflounder-stratmu.R` here("sumflounder"). 
mudiff_dat <- readRDS(here("data", "atlmackerel", "mudiff_dat.rds"))

cvdiff_dat <- readRDS(here("data", "atlmackerel", "cvdiff_dat.rds"))

slope_diff_dat <- readRDS(here("data", "atlmackerel", "slope_diff.rds"))


# PLOTS ####
# mean difference
mudiff_plot <- ggplot() +  
  geom_histogram(data = mudiff_all |> filter(SEASON == "SPRING"), aes((MARE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric(mudiff_dat[1,5]*100), linetype = 6, color = pal[3], linewidth = 1) + 
  annotate("text", x = as.numeric(mudiff_dat[1,5]*100)-0.5, y = 50, label = str_c(round((mudiff_dat[1,5]*100), 0), "%", sep = " "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Mean absolute relative differences", y = "Number of species") +
  ylim(0,100) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11), axis.text = element_text(size = 11),  strip.text = element_text(size = 11))

# cv difference
cvdiff_plot <- ggplot() +  
  geom_histogram(data = cvdiff_all |> filter(SEASON == "SPRING"), aes((MARE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric(cvdiff_dat[1,5]*100), linetype = 6, color = pal[3], linewidth = 1) + 
  annotate("text", x = as.numeric(cvdiff_dat[1,5]*100)-0.5, y = 50, label = str_c(round((cvdiff_dat[1,5]*100), 0), "%", sep = " "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Mean absolute relative differences", y = "Number of species") +
  ylim(0,100) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11), axis.text.x = element_text(size = 11), strip.text = element_text(size = 11))

# slope diff
slope_diff_plot <- ggplot() +
  geom_histogram(data = mudiff_all |> filter(SEASON == "SPRING"), aes((MAE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric(slope_diff_dat[1,3]*100), linetype = 6, color = pal[3], linewidth = 1) +
  annotate("text", x = as.numeric(slope_diff_dat[1,3]*100)+ 120, y = 165, label = str_c(round(slope_diff_dat[1,3]*100, 2), "%", sep = " "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  ylim(0, 200)+
  labs(x = "Relative absolute differences", y = "Number of species") +
  theme_bw() +
  theme(axis.title = element_text(size = 11), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.text.x = element_text(size = 11), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text.y =  element_blank(), axis.ticks.y = element_blank(), strip.text = element_text(size = 11))



### save the plot
ggsave(filename ="mudiff_plot.png", device = "png", plot = mudiff_plot, path = here("outputs", "init-analysis-plots", "atlmackerel"), width = 8, height = 6)

ggsave(filename ="cvdiff_plot.png", device = "png", plot = cvdiff_plot, path = here("outputs", "init-analysis-plots", "atlmackerel"), width = 8, height = 6)

ggsave(filename ="slope-diff.png", device = "png", plot = slope_diff_plot, path = here("outputs", "init-analysis-plots", "atlmackerel"), width = 10, height = 6)
