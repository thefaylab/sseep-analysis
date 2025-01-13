### created: 11/30/2023
### last updated: 11/11/2024

# 01 - FILTER ATLANTIC MACKEREL DATA ####


## OBJECTIVE ####
# save copies of only Atlantic mackerel data for future analyses
# creates datasets where outliers are removed and are maintained to compare best model fit in sdmtmb framework 

### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))


### LOAD DATA ####
# expanded data created here("tidy-data", "04-complete-datasets.R")
data <- readRDS(here("data", "rds", "completed_bts_data.rds")) |> filter(SVSPP == 121) |> mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))

# read in the northeast coastline
ne_coast <- sf::st_read(dsn = here("gis", "eastern_coast_WGS84.shp"))

# active strata shapefile 
strata <- readRDS(here("data", "rds", "active_strata.rds"))


# set the years that will be included in the dataset because they are full survey years 
full_survey <- c(2009:2019, 2021)


## FILTER DATA ####
# without outliers removed 
am_spring <- data |> 
  filter(SEASON == "SPRING", YEAR %in% full_survey)

# with outliers removed 
# filter raw data by season for seasonal model fitting
am_spring_no.out <- data |> 
  filter(SEASON == "SPRING", YEAR %in% full_survey, EXPCATCHWT <= quantile(EXPCATCHWT, 0.99))

## PLOT THE OBSERVATIONS ####
obs_plot <- ggplot() +
  geom_sf(data = ne_coast, fill = "#efe5c7", color = "#816c62") +
  geom_sf(data = strata, fill = NA) +
  geom_point(data = am_spring, aes(x = DECDEG_BEGLON, y = DECDEG_BEGLAT, color = EXPCATCHWT, size = EXPCATCHWT, alpha = EXPCATCHWT)) +
  scale_colour_viridis_c(guide = "legend")+
  scale_size(range=c(1,10)) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  labs(x = "Longitude", y = "Latitude", color = "Biomass\n(kg/tow)", size = "Biomass\n(kg/tow)", alpha = "Biomass\n(kg/tow)") +
  facet_grid(cols = vars(str_to_title(AREA))) +
  theme(panel.background = element_blank(), panel.grid = element_blank())

## PLOT WIND OVER TIME ####
atlmack_summ <- am_spring |> 
  group_by(EST_YEAR, AREA, SEASON) |> 
  summarise(sum_tow = length(unique(TOWID)), 
            sum_catch = sum(EXPCATCHNUM), 
            sum_bio = sum(EXPCATCHWT))
# atlmack_summ |> group_by(AREA, SEASON) |> summarise(sum(sum_tow))
# atlmack_summ |> group_by(AREA, SEASON) |> summarise(mean(sum_tow), sd(sum_tow))


wind_tows_plot <- ggplot(atlmack_summ |> filter(AREA == "WIND")) +
  # geom_col(aes(x = as.factor(EST_YEAR), y = sum_tow, fill = str_to_title(AREA)), position = "fill") +
  geom_line(aes(x = EST_YEAR, y = sum_tow, color = str_to_title(AREA))) +
  labs(x = "Year", y = "Number of survey tows", color = "Area") +
  # geom_bar(aes(x = as.factor(EST_YEAR), y = sum_tow, fill = AREA), stat_count(sum_tow), position = "fill") +
  # facet_wrap(~str_to_title(AREA)) +
  ylim(0, NA)


all_tows_plot <- ggplot(atlmack_summ) +
  geom_line(aes(x = EST_YEAR, y = sum_tow, color = str_to_title(AREA))) +
  labs(x = "Year", y = "Number of survey tows", color = "Area") +
  # facet_wrap(~str_to_title(SEASON)) +
  ylim(0, NA)

### save the data 
data <- list("atlmack_complete_data" = data, 
             "atlmackerel_spring" = am_spring, 
             "sumflounder_spring_no-out" = am_spring_no.out)
plots <- list("biomass_plot" = obs_plot, 
              "wind_tows_plot" = wind_tows_plot, 
              "all_tows_plot" = all_tows_plot)

pmap(list(data, names(data)), ~saveRDS(.x, file = str_c(.y, "rds", sep = ".",), path = here("data", "atlmackerel")))
pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = str_c(.y, "png", sep = "."), device = "png", path = here("outputs", "init-analysis-plots", "atlmackerel"), width = 12, height = 8))

# saveRDS(data, here("data", "atlmackerel", "atlmack_complete_data.rds"))
# saveRDS(am_spring, here("data", "atlmackerel", "atlmackerel_spring.rds"))
# saveRDS(am_spring_no.out, here("data", "atlmackerel", "atlmackerel_spring_no-out.rds"))

