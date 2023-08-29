### created: 08/23/2023
### last updated: 

# 03a -  Calculate True Abundance - Fall Scenarios  ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"
#source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
#set.seed(123)


### LOAD DATA ####
# base simulation data 
sim_base_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseGrid.rds"))

# increased simulation data 
sim_increase_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseGrid.rds"))

# reduced simulation data 
sim_decrease_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseGrid.rds"))


## CALCULATE TRUE ABUNDANCE ####
### Base Scenario ####
base_trueN <- sim_base_grid |>
  group_by(rep, EST_YEAR) |> 
  nest() |> 
  mutate(trueN = map(data, ~sum(.$EXPCATCHWT))) |> 
  unnest(trueN) |> 
  mutate(SCENARIO = "BASELINE")

med_base_trueN <- base_trueN |>   
  group_by(EST_YEAR)|>
  summarise(med = median(trueN)) |> 
  mutate(SCENARIO = "BASELINE")

base_trueN |>
  median_qi(trueN, .width = c(0.25, 0.5, 0.95)) |> 
  ggplot() +
  aes(EST_YEAR, y = trueN, ymin = .lower, ymax = .upper) + 
  geom_lineribbon()+ 
  scale_fill_brewer()

# ggplot(sim_base_grid) + 
#   aes(EST_YEAR, EXPCATCHWT, group = rep) + 
#   geom_line(alpha = 0.3, col = gray(0.5))
# base_trueN |>
#   select(!data) |>
#   group_by(EST_YEAR) |> 
#   summarise(med = median(trueN), 
#             lower = quantile(trueN, 0.025), 
#             upper = quantile(trueN, 0.975)) |>
# ggplot() +
#   aes(EST_YEAR, y = med) + 
#   #geom_point()+ 
#   geom_line() +
#   geom_errorbar(aes(ymin = lower, ymax = upper))



### Increased Scenario ####
inc_trueN <- sim_increase_grid |>
  group_by(rep, EST_YEAR) |> 
  nest() |> 
  mutate(trueN = map(data, ~sum(.$EXPCATCHWT))) |> 
  unnest(trueN) |> 
  mutate(SCENARIO = "ENHANCED")
  
med_inc_trueN <- inc_trueN |>   
  group_by(EST_YEAR)|>
  summarise(med = median(trueN)) |> 
  mutate(SCENARIO = "ENHANCED")

inc_trueN |>
  median_qi(trueN, .width = c(0.25, 0.5, 0.95)) |> 
  ggplot() +
  aes(EST_YEAR, y = trueN, ymin = .lower, ymax = .upper) + 
  geom_lineribbon()+ 
  scale_fill_brewer()

### Decreased Scenario ####
dec_trueN <- sim_decrease_grid |>
  group_by(rep, EST_YEAR) |> 
  nest() |> 
  mutate(trueN = map(data, ~sum(.$EXPCATCHWT))) |> 
  unnest(trueN) |> 
  mutate(SCENARIO = "REDUCED")
  
med_dec_trueN <- dec_trueN |>  
  group_by(EST_YEAR)|>
  summarise(med = median(trueN)) |> 
  mutate(SCENARIO = "REDUCED")

dec_trueN |>
  group_by(EST_YEAR) |>
  median_qi(trueN, .width = c(0.25, 0.5, 0.95)) |> 
  ggplot() +
  aes(EST_YEAR, y = trueN, ymin = .lower, ymax = .upper) + 
  geom_lineribbon()+ 
  scale_fill_brewer()


## PLOT ####
trueN <- bind_rows(base_trueN, inc_trueN, dec_trueN)

trueN |>
  group_by(EST_YEAR, SCENARIO) |>
  median_qi(trueN, .width = c(0.25, 0.5, 0.95)) |> 
  ggplot() +
  aes(EST_YEAR, y = trueN, ymin = .lower, ymax = .upper) + 
  geom_lineribbon()+ 
  scale_fill_brewer() +
  facet_wrap(~SCENARIO) + 
  labs(x = "Year", y = "Simulated Total Biomass (kg)")

ggsave("trueN_scenarios.png", plot = last_plot(), path = here("sdmtmb", "sumflounder", "plots"), width = 9, height = 5)



trueN |> 
  group_by(SCENARIO) |>
  ggplot() + 
  aes(x = EST_YEAR, y = med, color = SCENARIO)+
  scale_color_manual(values = c("#0a4c8a", "#0B6E4F", "#57B8FF"), aesthetics = c("color")) +
  #scale_shape_manual(values = c(0, 22, 24))+
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Simulated Total Biomass (kg)", color = "Scenario") + 
  theme(legend.position = "bottom")
    
ggsave("scenario_trueN.png", plot = last_plot(), path = here("sdmtmb", "sumflounder", "plots"), width = 10, height = 5)
a
