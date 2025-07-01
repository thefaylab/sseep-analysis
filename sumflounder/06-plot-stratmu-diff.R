### created: 01/26/2024
### last updated: 11/10/2024

# 06 - PLOT SUMMER FLOUNDER MEAN DIFFERENCES ####


## OBJECTIVE ####
# plot the mean relative difference between survey effort scenario indices for summer flounder compared to the relative differences across all species observed on the bottom trawl survey



### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
# library(kableExtra)
library(nationalparkcolors)


### dependencies ####
# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()


### LOAD DATA ####
# datasets created from `03-strat-mu-diff.R` here("retro-analysis"). 
mudiff_all <- readRDS(here("data", "rds", "retro-analysis", "species_stratmu-diff.rds")) 

cvdiff_all <- readRDS(here("data", "rds", "retro-analysis", "species_cv-diff.rds")) 

slope_diff_all <- readRDS(here("data", "rds", "retro-analysis", "species_slope-diff.rds"))

# datasets created from `05-calculate-sumflounder-stratmu.R` here("sumflounder"). 
mudiff_dat <- readRDS(here("data", "sumflounder", "mudiff_dat.rds"))

cvdiff_dat <- readRDS(here("data", "sumflounder", "cvdiff_dat.rds"))

slope_diff_dat <- readRDS(here("data", "sumflounder", "slope_diff.rds"))


## PLOT THE DISTRIBUTION #####
pal <- park_palette("Badlands")


mudiff_plot <- ggplot() +
  geom_histogram(data = mudiff_all, aes((MARE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Relative percent differences between preclusion and status quo annual stratified mean abundance indices", y = "Number of species") +
  theme_bw() 

cvdiff_plot <- ggplot() +
  geom_histogram(data = cvdiff_all, aes((MARE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Relative percent differences between preclusion and status quo annual stratified mean abundance indices", y = "Number of species") +
  theme_bw() 

slope_plot <- ggplot() +
  geom_histogram(data = slope_diff_all, aes(MAE, after_stat(count)), color = pal[5], fill = pal[1]) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Mean absolute differences between preclusion and status quo annual stratified mean abundance indices", y = "Number of species") +
  theme_bw() 



# presentation plots
# mean difference plots
sf_mudiff_fall_plot <- ggplot() +  
  geom_histogram(data = mudiff_all |> filter(SEASON == "FALL"), aes((MARE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric((mudiff_dat[1,5]*100)), linetype = 6, color = pal[3], linewidth = 1) + 
  annotate("text", x = as.numeric(mudiff_dat[1,5]*100)-1.5, y = 50, label = str_c(round((mudiff_dat[1,5]*100), 0), "%", sep = " "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Mean absolute relative differences", y = "Number of species") +
  ylim(0, 100)+
  theme_bw() + 
  theme(axis.title = element_text(size = 11), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 11), strip.text = element_text(size = 11))

sf_mudiff_spr_plot <- ggplot() +  
  geom_histogram(data = mudiff_all |> filter(SEASON == "SPRING"), aes((MARE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric((mudiff_dat[2,5]*100)), linetype = 6, color = pal[3], linewidth = 1) + 
  annotate("text", x = as.numeric(mudiff_dat[2,5]*100)-1, y = 50, label = str_c(round((mudiff_dat[2,5]*100), 0), "%", sep = " "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Mean absolute relative differences", y = "") +
  ylim(0,100) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.text.x = element_text(size = 11), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text.y =  element_blank(), axis.ticks.y = element_blank(), strip.text = element_text(size = 11))

sf_mudiff_present <- ((sf_mudiff_fall_plot + theme(plot.margin = unit(c(5, 2, 5, 5), "pt"))) + (sf_mudiff_spr_plot + theme(plot.margin = unit(c(5,5,5,2), "pt")))) + plot_annotation(tag_levels = "A")#title = "Average absolute relative differences of annual stratified mean abundance indices between survey effort scenarios", theme = theme(plot.title = element_text(size = 11, hjust = 0.5)))

# cv difference
cvdiff_fall_plot <- ggplot() +  
  geom_histogram(data = cvdiff_all |> filter(SEASON == "FALL"), aes((MARE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric((cvdiff_dat[1,5]*100)), linetype = 6, color = pal[3], linewidth = 1) + 
  annotate("text", x = as.numeric(cvdiff_dat[1,5]*100)-1, y = 50, label = str_c(round((cvdiff_dat[1,5]*100), 0), "%", sep = " "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Mean absolute relative differences", y = "Number of species") +
  ylim(0, 100)+
  theme_bw() + 
  theme(axis.title = element_text(size = 11), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 11), strip.text = element_text(size = 11))

cvdiff_spr_plot <- ggplot() +  
  geom_histogram(data = cvdiff_all |> filter(SEASON == "SPRING"), aes((MARE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric((cvdiff_dat[2,5]*100)), linetype = 6, color = pal[3], linewidth = 1) + 
  annotate("text", x = as.numeric(cvdiff_dat[2,5]*100)-1, y = 50, label = str_c(round((cvdiff_dat[2,5]*100), 0), "%", sep = " "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Mean absolute relative differences", y = "") +
  ylim(0,100) +
  theme_bw() + 
  theme(axis.title = element_text(size = 11), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.text.x = element_text(size = 11), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text.y =  element_blank(), axis.ticks.y = element_blank(), strip.text = element_text(size = 11))

cvdiff_present <- ((cvdiff_fall_plot + theme(plot.margin = unit(c(5, 2, 5, 5), "pt"))) + (cvdiff_spr_plot + theme(plot.margin = unit(c(5,5,5,2), "pt")))) + plot_annotation(tag_levels = "A")

# slope difference
slope_fall_plot <- ggplot() +
  geom_histogram(data = slope_diff_all |> filter(SEASON == "FALL"), aes((MAE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric(slope_diff_dat[1,3]*100), linetype = 6, color = pal[3], linewidth = 1) +
  annotate("text", x = as.numeric(slope_diff_dat[1,3]*100)+3.5, y = 165, label = str_c(round(slope_diff_dat[1,3]*100, 2), "%", sep = ""), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Mean absolute differences", y = "Number of species") +
  ylim(0, 200)+
  theme_bw() +
  theme(axis.title = element_text(size = 11), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 11), strip.text = element_text(size = 11))

slope_spr_plot <- ggplot() +
  geom_histogram(data = slope_diff_all |> filter(SEASON == "SPRING"), aes((MAE*100), after_stat(count)), color = pal[5], fill = pal[1]) +
  geom_vline(xintercept = as.numeric(slope_diff_dat[2,3]*100), linetype = 6, color = pal[3], linewidth = 1) +
  annotate("text", x = as.numeric(slope_diff_dat[2,3]*100)+ 10, y = 165, label = str_c(round(slope_diff_dat[2,3]*100, 2), "%", sep = " "), angle = 90) +
  facet_wrap(~str_to_title(SEASON)) +
  ylim(0, 200)+
  labs(x = "Relative absolute differences", y = "") +
  theme_bw() +
  theme(axis.title = element_text(size = 11), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.text.x = element_text(size = 11), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text.y =  element_blank(), axis.ticks.y = element_blank(), strip.text = element_text(size = 11))

slope_present <- ((slope_fall_plot + theme(plot.margin = unit(c(5, 2, 5, 5), "pt"))) + (slope_spr_plot + theme(plot.margin = unit(c(5,5,5,2), "pt")))) + plot_annotation(tag_levels = "A")#title = "Average absolute relative differences of



### save the plot
ggsave(filename ="species_mean-sq-diff.png", plot = mudiff_plot, device = "png" , path = here("outputs", "init-analysis-plots"), width = 10, height = 6)

ggsave(filename ="sumflounder_mudiff.png", device = "png", plot = sf_mudiff_present, path = here("outputs", "init-analysis-plots", "sumflounder"), width = 10, height = 6)

ggsave(filename ="sumflounder_cvdiff.png", device = "png", plot = cvdiff_present, path = here("outputs", "init-analysis-plots", "sumflounder"), width = 10, height = 6)

ggsave(filename ="sumflounder_slope-diff.png", device = "png", plot = slope_present, path = here("outputs", "init-analysis-plots", "sumflounder"), width = 10, height = 6)


