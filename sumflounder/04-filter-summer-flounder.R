### created: 07/24/2023
### last updated: 11/11/2024

# 04 - FILTER SUMMER FLOUNDER DATA ####


## OBJECTIVE ####
# filter observations of summer flounder from the bottom trawl dataset based on the strata identified in the seasonal spatial footprints. 


### LOAD PACKAGES ####
# library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

### LOAD AND FILTER DATA ####

# raw data based on cumulative distribution filter created here("tidy-data", "05b-spatial-filter-data.R")
# data <- readRDS(here("data", "rds", "95filtered_complete_bts.rds")) |>
#   filter(SVSPP == 103)

# expanded bts data created here("tidy-data", "04-complete-datasets.R")
sumflounder_complete <- readRDS(here("data", "rds", "completed_bts_data.rds")) |> filter(SVSPP == 103) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# fall footprint
fall_footprint <- readRDS(here("data", "sumflounder", "sf_fall_footprint.rds"))
fall_strata <- unique(fall_footprint$STRATUM)

# spring footprint
spring_footprint <- readRDS(here("data", "sumflounder", "sf_spring_footprint.rds"))
spring_strata <- unique(spring_footprint$STRATUM)

# read in the northeast coastline
ne_coast <- sf::st_read(dsn = here("gis", "eastern_coast_WGS84.shp"))

# active strata shapefile 
strata <- readRDS(here("data", "rds", "active_strata.rds"))


## FILTER DATA ####
# fall data 
sf_fall <- sumflounder_complete |> 
  filter(SEASON == "FALL", STRATUM %in% fall_strata, YEAR != 2017, !AVGDEPTH > 150)

#spring data
sf_spring <- sumflounder_complete |> 
  filter(SEASON == "SPRING", STRATUM %in% spring_strata, YEAR != 2020)

# full dataset 
sumflounder <- bind_rows(sf_spring, sf_fall)

## PLOT THE OBSERVATIONS ####
obs_plot <- ggplot() +
  geom_sf(data = ne_coast, fill = "#efe5c7", color = "#816c62") +
  geom_sf(data = strata, fill = NA) +
  geom_point(data = sumflounder, aes(x = DECDEG_BEGLON, y = DECDEG_BEGLAT, color = EXPCATCHWT, size = EXPCATCHWT, alpha = EXPCATCHWT)) +
  scale_colour_viridis_c(guide = "legend", direction = -1)+
  scale_size(range=c(1,10)) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  labs(x = "Longitude", y = "Latitude", color = "Biomass\n(kg/tow)", size = "Biomass\n(kg/tow)", alpha = "Biomass\n(kg/tow)") +
  facet_grid(rows = vars(str_to_title(AREA)), cols = vars(str_to_title(SEASON))) +
  theme(panel.background = element_blank(), panel.grid = element_blank())

## PLOT WIND OVER TIME ####
sumflounder_summ <- sumflounder |> 
  group_by(EST_YEAR, AREA, SEASON) |> 
  summarise(sum_tow = length(unique(TOWID)), 
            sum_catch = sum(EXPCATCHNUM), 
            sum_bio = sum(EXPCATCHWT))

# sumflounder_summ |> group_by(AREA, SEASON) |> summarise(sum(sum_tow))
# sumflounder_summ |> group_by(AREA, SEASON) |> summarise(mean(sum_tow), sd(sum_tow))

wind_tows_plot <- ggplot(sumflounder_summ |> filter(AREA == "WIND")) +
  # geom_col(aes(x = as.factor(EST_YEAR), y = sum_tow, fill = str_to_title(AREA)), position = "fill") +
  geom_line(aes(x = EST_YEAR, y = sum_tow, color = str_to_title(AREA))) +
  labs(x = "Year", y = "Number of survey tows", color = "Area") +
  # geom_bar(aes(x = as.factor(EST_YEAR), y = sum_tow, fill = AREA), stat_count(sum_tow), position = "fill") +
  facet_wrap(~str_to_title(SEASON)) +
  ylim(0, NA)

all_tows_plot <- ggplot(sumflounder_summ) +
  geom_line(aes(x = EST_YEAR, y = sum_tow, color = str_to_title(AREA))) +
  labs(x = "Year", y = "Number of survey tows", color = "Area") +
  facet_wrap(~str_to_title(SEASON)) +
  ylim(0, NA)

# stratified mean abundance indices created here("retro-analysis") 
# sf_stratmu <- readRDS(here("data", "rds", "retro-analysis", "strat-mu_all.rds")) |> 
#   filter(SVSPP == 103)
# 
# 
# # mean squared relative difference of abundance indices created here("retro-analysis", "03-strat-mu-diff.R")
# sf_mudiff <- readRDS(file = here("data", "rds", "species_mean-sq-diff.rds")) |> 
#   filter(SVSPP == 103)
# 
# 
# # linear regression slope estimates for summer flounder for change in abudance indices over time for each scenario
# sf_slopes <- readRDS(here("retro-analysis", "active_strata_wts_only", "obs-stratmu-slopes.rds")) |>
#   filter(SVSPP == 103)



### save the data 
data <- list("sumflounder" = sumflounder, 
             "sumflounder_fall" = sf_fall, 
             "sumflounder_spring" = sf_spring)
plots <- list("biomass_plot" = obs_plot, 
              "wind_tows_plot" = wind_tows_plot, 
              "all_tows_plot" = all_tows_plot)

pmap(list(data, names(data)), ~saveRDS(.x, file = str_c(.y, "rds", sep = ".",), path = here("data", "sumflounder")))
pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = str_c(.y, "png", sep = "."), device = "png", path = here("outputs", "init-analysis-plots", "sumflounder"), width = 12, height = 8))
  
# saveRDS(sumflounder, here("data", "sumflounder", "sumflounder.rds"))
# saveRDS(sf_fall, here("data", "sumflounder", "sumflounder_fall.rds"))
# saveRDS(sf_spring, here("data", "sumflounder", "sumflounder_spring.rds"))
# saveRDS(sf_stratmu, here("data", "sumflounder", "sf_stratmu.rds"))
# saveRDS(sf_mudiff, here("data", "sumflounder", "sf_mudiff.rds"))
# saveRDS(sf_slopes, here("data", "sumflounder", "sf_obs-slopes.rds"))
