### created: 02/08/2024
### last updated: 

# consolidate retro analysis ####


## OBJECTIVE ####

# consolidate all scripts in retro-analysis folder by calling on stratified mean functions 
# calculate stratified mean based on strata that appear in the spatially filtered trawl data only


### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
source(here("R", "StratMeanFXs_v2.R"))

dat.files <- here("data", "rds")

### LOAD DATA ####
# dataset created from `05b-spatial-filter-data.R` here("tidy-data"). Contains complete observations for each species and unique tow filtered based on 95% cumulative distribution of biomass. 
data <- readRDS(here(dat.files, "95filtered_complete_bts.rds"))

# species dataframe for adding to final dataset 
species <- readRDS(here(dat.files, "95filtered-species.rds"))

# load the active bottom trawl survey strata and their relative area weights created here(tidy-data, "02b-filter-current-strata.R")
strata <- readRDS(here(dat.files, "active_strata_wts.rds"))

## CALCULATE STRATIFIED MEANS ####
### With wind included ####
incl_stratmu <- data |>
  group_by(SVSPP, SEASON, EST_YEAR) |>
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  dplyr::select(!data) |>
  unnest(cols = stratmu) |> 
  mutate(Type = "With Wind Included")

### With wind precluded ####
precl_stratmu <- data |>
  filter(AREA == "OUTSIDE") |> 
  group_by(SVSPP, SEASON, EST_YEAR) |>
  nest() |> 
  mutate(stratmu = map(data, ~stratified.mean(., strata))) |> 
  dplyr::select(!data) |>
  unnest(cols = stratmu) |> 
  mutate(Type = "With Wind Precluded")

## BIND DATA #### 
stratmeans_rows <- bind_rows(incl_stratmu, precl_stratmu) |> #bind rows of the two datasets
  left_join(species, by = "SVSPP") |> # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)

stratmeans_cols <- left_join(incl_stratmu, precl_stratmu, by = c("SVSPP", "SEASON", "EST_YEAR")) |> 
  left_join(species, by = "SVSPP") |> # add common names to the dataset by joining the species dataframe based on species code
  arrange(SVSPP)


## CALCULATE RELATIVE PERCENT DIFFERENCE ####
mudiff <- stratmeans_cols |> 
  group_by(SVSPP, SEASON, COMNAME) |> 
  nest() |> 
  mutate(errors = map(data, ~calc.errors(., expected = stratmu.x, observed = stratmu.y)),
         mudiff = map(errors, ~tail(., 5) |> mean.diff())) |> 
  dplyr::select(!c(data, errors)) |> 
  unnest(cols = mudiff)

topten_fall <- mudiff |>
  filter(SEASON == "FALL") |>
  arrange(desc(MARE_perc)) |> 
  head(10) |> 
  mutate(MARE_perc = round(MARE_perc,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = MARE_perc)

kable(topten_fall[,c(3, 1, 6)], align = "lcccc", caption = "Ten Highest Percent Differences in Abundance Indices by Species", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)

topten_spring <- mudiff |>
  filter(SEASON == "SPRING") |>
  arrange(desc(MARE_perc)) |> 
  head(10) |> 
  mutate(MARE_perc = round(MARE_perc,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = MARE_perc)

kable(topten_spring[,c(3, 6, 1)], align = "lcccc", caption = "Ten Highest Percent Differences in Abundance Indices by Species", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)

saveRDS(incl_stratmu, here(dat.files, "retro-analysis",  "stratmu_included.rds"))
saveRDS(precl_stratmu, here(dat.files, "retro-analysis", "stratmu_precluded.rds"))
saveRDS(stratmeans_rows, here(dat.files, "retro-analysis", "all_stratmu_rows.rds"))
saveRDS(stratmeans_cols, here(dat.files, "retro-analysis","all_stratmu_cols.rds"))
saveRDS(mudiff, here(dat.files, "retro-analysis", "species_stratmu-diff.rds"))

# 
# ggplot(mudiff) + 
#   geom_histogram(aes(MARE_perc, after_stat(count)), color = "white", fill = "#5dc5e9") +
#   facet_wrap(~str_to_title(SEASON)) +
#   labs(x = "Mean absolute relative percent differences", y = "Number of species") +
#   # ylim(0, 125)+
#   theme_bw() + 
#   theme(axis.title = element_text(size = 16), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14))
# 
# 
# #summer flounder 
# sf_mudiff <- mudiff |> 
#   filter(SVSPP == 103)
# 
# pal <- park_palette("GreatBasin")
# 
# sf_mudiff_fall_plot <- ggplot() +
#   geom_histogram(data = mudiff |> filter(SEASON == "FALL"), aes(MARE_perc, after_stat(count)), color = pal[2], fill = pal[1]) +
#   geom_vline(xintercept = as.numeric(sf_mudiff[1,2]), linetype = 6, color =pal[3], linewidth = 1) + #
#   facet_wrap(~str_to_title(SEASON)) +
#   labs(x = "Mean absolute relative percent differences", y = "Number of species") +
#   # ylim(0, 100)+
#   theme_bw() +
#   theme(axis.title = element_text(size = 16), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14))
# 
# 
# sf_mudiff_spr_plot <- ggplot() +  
#   geom_histogram(data = mudiff |> filter(SEASON == "SPRING"), aes(MARE_perc, after_stat(count)), color = pal[2], fill = pal[1]) +
#   geom_vline(xintercept = as.numeric(sf_mudiff[2,2]), linetype = 6, color = pal[3], linewidth = 1) + 
#   facet_wrap(~str_to_title(SEASON)) +
#   labs(x = "Mean absolute relative percent differences", y = "Number of species") +
#   ylim(0, 100)+
#   theme_bw() + 
#   theme(axis.title = element_text(size = 16), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14))
# 
# sf_mudiff_present <- ((sf_mudiff_fall_plot + theme(plot.margin = unit(c(5, 2, 5, 5), "pt"))) + (sf_mudiff_spr_plot + theme(plot.margin = unit(c(5,5,5,2), "pt")))) + plot_annotation(title = "Average absolute relative percent differences of annual stratified mean abundance indices\nbetween survey effort scenarios", theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))
# 
# ggsave(filename ="sf_mudiff_present.png", device = "png", plot = sf_mudiff_present, path = here("outputs", "sumflounder"), width = 12, height = 5)
# 
# #Atlantic Mackerel
# am_mudiff <- mudiff |> filter(SVSPP == 121)
# 
# am_mudiff_plot <- ggplot() +  
#   geom_histogram(data = mudiff |> filter(SEASON == "SPRING"), aes(MARE_perc, after_stat(count)), color = pal[2], fill = pal[1]) +
#   geom_vline(xintercept = as.numeric(am_mudiff[1,6]), linetype = 6, color = pal[3], linewidth = 1) + 
#   facet_wrap(~str_to_title(SEASON)) +
#   labs(x = "Mean absolute relative percent differences", y = "Number of species") +
#   # ylim(0, 125)+
#   theme_bw() + 
#   theme(axis.title = element_text(size = 16), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14))
# 
# am_mudiff_present <- am_mudiff_plot + plot_annotation(title = "Average absolute relative percent differences of annual stratified\nmean abundance indices between survey effort scenarios", theme = theme(plot.title = element_text(size = 16, hjust = 0.65)))
# 
# ggsave(filename ="am_mudiff_present.png", device = "png", plot = am_mudiff_present, path = here("outputs", "atlmackerel"), width = 8, height = 5)
