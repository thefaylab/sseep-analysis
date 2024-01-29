### created: 01/26/2024
### last updated: 

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
mudiff_all <- readRDS(here("data", "rds", "retro-analysis", "species_mean-sq-diff.rds"))

# dataset created from `05-calculate-sumflounder-stratmu.R` here("sumflounder"). 
mudiff_dat <- readRDS(here("data", "atlmackerel", "am_mudiffdat.rds"))



# PLOTS ####
# am_mudiff_fall_plot <- ggplot() +  
#   geom_histogram(data = mudiff_all |> filter(SEASON == "FALL"), aes(mudiff, after_stat(count)), color = "white", fill = "#5dc5e9") +
#   geom_vline(xintercept = as.numeric(mudiff_dat[1,3]), linetype = 6, color = "#0a4c8a", linewidth = 1) + 
#   facet_wrap(~str_to_title(SEASON)) +
#   labs(x = "Relative percent differences", y = "Number of species") +
#   ylim(0, 125)+
#   theme_bw() + 
#   theme(axis.title = element_text(size = 16), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14))

mudiff_spr_plot <- ggplot() +  
  geom_histogram(data = mudiff_all |> filter(SEASON == "SPRING"), aes(mudiff, after_stat(count)), color = "white", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(mudiff_dat[1,3]), linetype = 6, color = "#0a4c8a", linewidth = 1) + 
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Relative percent differences", y = "Number of species", title = "Average squared relative differences of annual stratified mean abundance indices between survey effort scenarios") +
  ylim(0,120) +
  theme_bw() + 
  theme(axis.title = element_text(size = 16), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 16), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), strip.text = element_text(size = 14), plot.title = element_text(size = 16, hjust = 0.5))

# sf_mudiff_present <- ((sf_mudiff_fall_plot + theme(plot.margin = unit(c(5, 2, 5, 5), "pt"))) + (sf_mudiff_spr_plot + theme(plot.margin = unit(c(5,5,5,2), "pt")))) + plot_annotation(title = "Average squared relative differences of annual stratified mean abundance indices between survey effort scenarios", theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))



### save the plot
ggsave(filename ="mudiff_present.png", device = "png", plot = last_plot(), path = here("outputs", "atlmackerel"), width = 13, height = 8)


