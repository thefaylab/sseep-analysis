### created:      01/26/2024
### last update:  04/04/2024
###

# 03 - PLOT PERCENT IMPACTS ####

## Objective ####
# plot the percent lost in overlapped strata only based on tows and catch in wind areas only for a key set of species 


### LOAD PACKAGES ###
library(stringr)
library(patchwork)
library(here)
library(tidyverse)
library(nationalparkcolors)

here()
init.analysis.dat <- here("data", "rds", "init-analysis")


### LOAD DATA ###
# percent loss data created here("init-analysis", "02b-percent-impacts")
compare_species <- readRDS(here(init.analysis.dat, "species-impacts.rds"))

# species code of interest
svspp <- c(15, 32, 72, 103, 105, 106, 121, 131, 141, 503, 999)

# filter the impact data frame for the species of interest
species_impact <- compare_species |> 
  filter(SVSPP %in% svspp)

saveRDS(species_impact, here(init.analysis.dat, "stakeholder-species-impacts.rds"))

pal <- park_palette("Acadia")

# PLOT ### 
# in strata overlapped wind only
# total numbers caught
catch_lost_plot <- species_impact |> 
  ungroup() |>
  mutate(COMNAME = str_to_sentence(COMNAME)) |>
  ggplot() + 
  geom_col(aes(x = str_wrap(COMNAME, width = 10, whitespace_only = FALSE), y = WIND_OVERLAP_CATCH), fill = pal[3]) + 
  labs(x = "Species", y = "Percentage of total number of fish removed", subtitle = "Percent of total number of fish observed by the Bigelow in a strata overlapped by wind energy areas") +  
  ylim(0, 40) +
  coord_flip() + 
  theme_bw()+
  theme(title = element_text(size = 16, hjust = 0.5, margin = margin(5, 0, 10, 0, "pt")), axis.title.x = element_text(size = 18, margin = margin(10, 0, 5, 0, "pt")), axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 5, "pt")), axis.text = element_text(size = 16), axis.text.y =  element_text(margin = margin(75, 10, 75, 0, "pt")))

# total tows conducted
tows_lost_plot <- species_impact |> 
  ungroup() |>
  mutate(COMNAME = str_to_sentence(COMNAME)) |>
  ggplot() + 
  geom_col(aes(x = str_wrap(COMNAME, width = 10, whitespace_only = FALSE), y = WIND_OVERLAP_TOW), fill = pal[2]) + 
  labs(x = "Species", y = "Percentage of tows removed", subtitle = "Percent of Bigelow tows that observed a particular species in strata overlapped by wind energy areas") + 
  ylim(0, 40)+
  coord_flip() +
  theme_bw()+
  theme(title = element_text(size = 16, hjust = 0.5, margin = margin(5, 0, 10, 0, "pt")), axis.title.x = element_text(size = 18, margin = margin(10, 0, 5, 0, "pt")), axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 5, "pt")), axis.text = element_text(size = 16), axis.text.y = element_text(margin = margin(75, 10, 75, 0, "pt")))


# patchwork 
impact_plot <- (catch_lost_plot + theme(plot.margin = unit(c(10, 5, 15, 5), "pt"))) / (tows_lost_plot + theme(plot.margin = unit(c(15, 5, 5, 5), "pt"))) + plot_layout(ncol = 1, nrow = 2, height = c(10, 10))


# save plot
ggsave("species-impact-plot.png", device = "png", plot = impact_plot, here("outputs", "plots"), width = 15, height = 17)
ggsave("species-numbers-lost-plot.png", device = "png", plot = catch_lost_plot, here("outputs", "plots"), width = 14, height = 8)
ggsave("species-tows-lost-plot.png", device = "png", plot = tows_lost_plot, here("outputs", "plots"), width = 14, height = 8)
