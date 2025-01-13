### created:      01/26/2024
### last update:  11/18/2024
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

theme <- theme_bw() + theme(title = element_text(size = 16, hjust = 0.5, margin = margin(5, 0, 10, 0, "pt")), axis.title.x = element_text(size = 18, margin = margin(10, 0, 5, 0, "pt")), axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 5, "pt")), axis.text = element_text(size = 16), axis.text.y =  element_text(margin = margin(75, 10, 75, 0, "pt")))
theme_set(theme)

here()
init.analysis.dat <- here("data", "rds", "init-analysis")


### LOAD DATA ###
# percent loss data created here("init-analysis", "02b-percent-impacts")
compare_species <- readRDS(here(init.analysis.dat, "species-impacts.rds"))

compare_sp_yr <- readRDS(here(init.analysis.dat, "compare_species-year.rds"))

compare_sp_yr_ssn <- readRDS(here(init.analysis.dat, "species-season-year-impacts.rds"))


# species code of interest
svspp <- c(15, 32, 72, 103, 105, 106, 121, 131, 141, 503, 999)

# filter the impact data frame for the species of interest
species_impact <- compare_species |> 
  filter(SVSPP %in% svspp) |> 
  ungroup() |>
  mutate(COMNAME = str_to_sentence(COMNAME))

sp_yr_impact <- compare_sp_yr |> 
  filter(SVSPP %in% svspp) |> 
  ungroup() |>
  mutate(COMNAME = str_to_sentence(COMNAME))

sp_yr_ssn_impact <- compare_sp_yr_ssn |> 
  filter(SVSPP %in% svspp) |> 
  ungroup() |>
  mutate(COMNAME = str_to_sentence(COMNAME))


saveRDS(species_impact, here(init.analysis.dat, "stakeholder-species-impacts.rds"))

pal <- park_palette("Acadia")

# PLOTS #### 
## in overall survey area ####
### total numbers caught ####
total_catch_plot <- species_impact |> 
  ggplot() + 
  geom_col(aes(x = fct_reorder(str_wrap(COMNAME, width = 8, whitespace_only = FALSE), WIND_CATCHPCT), y = WIND_CATCHPCT), fill = pal[3]) + 
  labs(x = "Species", y = "Percentage of total number of fish removed") + #, subtitle = "Percent of total number of fish observed throughout the survey area by the Bigelow in wind energy areas") +  
  ylim(0, 20) +
  coord_flip() #+ 
  #theme(title = element_text(size = 16, hjust = 0.5, margin = margin(5, 0, 10, 0, "pt")), axis.title.x = element_text(size = 18, margin = margin(10, 0, 5, 0, "pt")), axis.title.y = element_text(size = 18, margin = margin(0, 10, 0, 5, "pt")), axis.text = element_text(size = 16), axis.text.y =  element_text(margin = margin(75, 10, 75, 0, "pt")))

### total biomass ####
total_biomass_plot <- species_impact |> 
  ggplot() + 
  geom_col(aes(x = fct_reorder(str_wrap(COMNAME, width = 10, whitespace_only = FALSE), WIND_BIOPCT), y = WIND_BIOPCT), fill = pal[4]) + 
  labs(x = "Species", y = "Percentage of total biomass removed") + #, subtitle = "Percent of total biomass of fish observed throughout the survey area by the Bigelow in wind energy areas") +  
  ylim(0, 20) +
  coord_flip() 

### total tows ####
total_tows_plot <- species_impact |> 
  ggplot() + 
  geom_col(aes(x = fct_reorder(str_wrap(COMNAME, width = 10, whitespace_only = FALSE), WIND_TOWPCT), y = WIND_TOWPCT), fill = pal[2]) + 
  labs(x = "Species", y = "Percentage of tows removed") +#, subtitle = "Percent of total number of fish observed by the Bigelow in a strata overlapped by wind energy areas") +  
  ylim(0, 20) +
  coord_flip() 

## in strata overlapped wind only ####
### total numbers caught ####
overlap_catch_plot <- species_impact |> 
  ungroup() |>
  mutate(COMNAME = str_to_sentence(COMNAME)) |>
  ggplot() + 
  geom_col(aes(x = fct_reorder(str_wrap(COMNAME, width = 10, whitespace_only = FALSE), WIND_OVERLAP_CATCH), y = WIND_OVERLAP_CATCH), fill = pal[3]) + 
  labs(x = "Species", y = "Percentage of total number of fish removed") + #, subtitle = "Percent of total number of fish observed by the Bigelow in a strata overlapped by wind energy areas") +  
  ylim(0, 40) +
  coord_flip() 

### total biomass caught ####
overlap_biomass_plot <- species_impact |> 
  ungroup() |>
  mutate(COMNAME = str_to_sentence(COMNAME)) |>
  ggplot() + 
  geom_col(aes(x = fct_reorder(str_wrap(COMNAME, width = 10, whitespace_only = FALSE), WIND_OVERLAP_BIO), y = WIND_OVERLAP_BIO), fill = pal[4]) + 
  labs(x = "Species", y = "Percentage of total biomass removed") + #, subtitle = "Percent of total number of fish observed by the Bigelow in a strata overlapped by wind energy areas") +  
  ylim(0, 40) +
  coord_flip() 

# total tows conducted
overlap_tows_plot <- species_impact |> 
  ggplot() + 
  geom_col(aes(x = fct_reorder(str_wrap(COMNAME, width = 10, whitespace_only = FALSE), WIND_OVERLAP_TOW), y = WIND_OVERLAP_TOW), fill = pal[2]) + 
  labs(x = "Species", y = "Percentage of tows removed") + #, subtitle = "Percent of Bigelow tows that observed a particular species in strata overlapped by wind energy areas") + 
  ylim(0, 40)+
  coord_flip() 


# patchwork 
total_impact <- (total_tows_plot + theme(plot.margin = unit(c(6,2,4,5), "pt"))) / (total_biomass_plot + theme(plot.margin = unit(c(4,2,4,5), "pt"))) / (total_catch_plot+ theme(plot.margin = unit(c(4,2,8,5), "pt"))) + plot_layout(ncol = 1, nrow = 3, height = c(10, 10, 10)) + plot_annotation(tag_levels = "A")

overlap_impact <- (overlap_tows_plot + theme(plot.margin = unit(c(6,2,4,5), "pt"))) / (overlap_biomass_plot + theme(plot.margin = unit(c(4,2,4,5), "pt"))) / (overlap_catch_plot+ theme(plot.margin = unit(c(4,2,8,5), "pt"))) + plot_layout(ncol = 1, nrow = 3, height = c(10, 10, 10)) + plot_annotation(tag_levels = "A")


## percent wind over time ####
sub_impact <- filter(sp_yr_ssn_impact, SVSPP != 103) |> filter(SVSPP != 121)
fluke_impact <- filter(sp_yr_ssn_impact, SVSPP == 103)
atlmack_impact <- filter(sp_yr_ssn_impact, SVSPP == 121)

cbPalette <- c("#999999", "#A22522", "#FF01FB", "#57B8FF", "#FDCA40", "#0A4C8A", "#FF8552", "#40531B", "#3EC300", "#0B6E4F", "#A1683A")


all.catch_plot <- ggplot(data = sub_impact, mapping = aes(x = EST_YEAR, y = WIND_CATCHPCT)) +
  aes(color = str_to_sentence(COMNAME)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5, shape = 1) +
  labs (x = "Year", y = "Percent of Total Survey Observations", color = "Species") +
  scale_fill_manual(values = cbPalette, name = "Species", aesthetics = "color") +
  scale_shape_manual(values = 1, name = "Species") +
  facet_wrap(~str_to_title(SEASON)) +
  theme_bw()

sp <- c("SUMFLOUNDER" = 1, "ATLMACKEREL" = 1)

catch.pct_plot <- all.catch_plot + geom_line(data = fluke_impact, aes(x = EST_YEAR, y = WIND_CATCHPCT, color = str_to_sentence(COMNAME))) +
  geom_point(data = fluke_impact, aes(x = EST_YEAR, y = WIND_CATCHPCT, color = str_to_sentence(COMNAME)), size = 2)  + 
  geom_line(data = atlmack_impact, aes(x = EST_YEAR, y = WIND_CATCHPCT, color = str_to_sentence(COMNAME))) +
  geom_point(data = atlmack_impact, aes(x = EST_YEAR, y = WIND_CATCHPCT, color = str_to_sentence(COMNAME)), size = 2) #+
  # guides(shape = guide_legend(override.aes = list(shape = c(1, 1))))
  # scale_shape_manual(values = sp, name = "Species")
  # 

all.bio_plot <- ggplot(data = sub_impact, mapping = aes(x = EST_YEAR, y = WIND_BIOPCT)) +
  aes(color = str_to_sentence(COMNAME)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5, shape = 1) +
  labs (x = "Year", y = "Percent of Total Survey Biomass", color = "Species") +
  scale_fill_manual(values = cbPalette, name = "Species", aesthetics = "color") +
  facet_wrap(~str_to_title(SEASON)) +
  theme_bw()

bio.pct_plot <- all.bio_plot + geom_line(data = fluke_impact, aes(x = EST_YEAR, y = WIND_BIOPCT, color = str_to_sentence(COMNAME))) +
  geom_point(data = fluke_impact, aes(x = EST_YEAR, y = WIND_BIOPCT, color = str_to_sentence(COMNAME)), size = 2)  + 
  geom_line(data = atlmack_impact, aes(x = EST_YEAR, y = WIND_BIOPCT, color = str_to_sentence(COMNAME))) +
  geom_point(data = atlmack_impact, aes(x = EST_YEAR, y = WIND_BIOPCT, color = str_to_sentence(COMNAME)), size = 2) #+
#   guides(shape = guide_legend(override.aes = list(shape = c(1, 1))))
# scale_shape_manual(values = sp, name = "Species")


all.tow_plot <- ggplot(data = sp_yr_ssn_impact, mapping = aes(x = EST_YEAR, y = WIND_TOWPCT)) +
  aes(color = str_to_sentence(COMNAME)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5, shape = 1) +
  labs (x = "Year", y = "Percent of Total Survey Tows", color = "Species") +
  scale_fill_manual(values = cbPalette, name = "Species", aesthetics = "color") +
  facet_wrap(~str_to_title(SEASON)) +
  theme_bw()

tow.pct_plot <- all.tow_plot + geom_line(data = fluke_impact, aes(x = EST_YEAR, y = WIND_TOWPCT, color = str_to_sentence(COMNAME))) +
  geom_point(data = fluke_impact, aes(x = EST_YEAR, y = WIND_TOWPCT, color = str_to_sentence(COMNAME)), size = 2)  + 
  geom_line(data = atlmack_impact, aes(x = EST_YEAR, y = WIND_TOWPCT, color = str_to_sentence(COMNAME))) +
  geom_point(data = atlmack_impact, aes(x = EST_YEAR, y = WIND_TOWPCT, color = str_to_sentence(COMNAME)), size = 2) #+

all.pct_plots <- (tow.pct_plot / bio.pct_plot / catch.pct_plot) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")


# TABLES ####
avgs <- compare_sp_yr_ssn |> ungroup() |> group_by(SEASON) |>#, SVSPP) |> 
  summarise(avg_bts_catch = mean(BTS_CATCH), 
            avg_bts_bio = mean(BTS_BIO), 
            avg_bts_tow = mean(BTS_TOW), 
            avg_wind_catch = mean(WIND_CATCH), 
            avg_wind_bio = mean(WIND_BIO), 
            avg_wind_tow = mean(WIND_TOW),
            avg_wind_catch.pct = mean(WIND_CATCHPCT), 
            avg_wind_bio.pct = mean(WIND_BIOPCT), 
            avg_wind_tow.pct = mean(WIND_TOWPCT)) 
species_avgs <- avgs |> 
  filter(SVSPP %in% svspp)



# SAVE ####
plots1 <- list("species_pct-catch_plot.png"= catch.pct_plot, 
               "species_pct-biomass_plot.png" = bio.pct_plot, 
               "species_pct-tows_plot.png" =tow.pct_plot)

plots2 <- list("species-total-num-plot.png" = total_catch_plot,
               "species-total-tows-plot.png" = total_tows_plot,
               "species-total-bio-plot.png" = total_biomass_plot, 
               "species-overlap-num-plot.png" = overlap_catch_plot,
               "species-overlap-tows-plot.png" = overlap_tows_plot,
               "species-overlap-bio-plot.png" = overlap_biomass_plot)
                  
patchworks <- list("species-all-total-plot.png" = total_impact,
               "species-all-overlap-plot.png" = overlap_impact, 
               "species-all-percents-plot.png" = all.pct_plots)

pmap(list(plots1, names(plots1)), ~ggsave(plot = .x, filename = .y, device = "png", path = here("outputs", "init-analysis-plots"), width = 10, height = 6))

pmap(list(plots2, names(plots2)), ~ggsave(plot = .x, filename = .y, device = "png", path = here("outputs", "init-analysis-plots"), width = 10, height = 8))

pmap(list(patchworks, names(patchworks)), ~ggsave(plot = .x, filename = .y, device = "png", path = here("outputs", "init-analysis-plots"), width = 15, height = 20))

ggsave("species-impact-plot.png", device = "png", plot = impact_plot, here("outputs", "plots"), width = 15, height = 20)

