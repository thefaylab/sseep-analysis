### created: 08/23/2023
### last updated: 

# 03a -  Calculate True Abundance - Spring Scenarios  ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"
#source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
#set.seed(123)


### LOAD DATA ####
# base simulation data 
sim_base_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseGrid.rds"))

# increased simulation data 
sim_increase_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseGrid.rds"))

# reduced simulation data 
sim_decrease_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseGrid.rds"))

spring_preds <- readRDS(here("sdmtmb", "sumflounder", "data", "spring_predictions.rds"))

## CALCULATE TRUE ABUNDANCE ####
### Observed Data ####
obs_trueN <- spring_preds |> 
  filter(EST_YEAR %in% c(2009:2021)) |> 
  group_by(EST_YEAR) |> 
  summarise(total.bio = sum(exp(est))) |> 
  mutate(SCENARIO = "PREDICTED") |>#, 
  #lower = total.bio, 
  #upper = total.bio) |> 
  relocate(SCENARIO, .before = everything())


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

# base_trueN |>
#   median_qi(trueN, .width = c(0.25, 0.5, 0.95)) |> 
#   ggplot() +
#   aes(EST_YEAR, y = trueN, ymin = .lower, ymax = .upper) + 
#   geom_lineribbon()+ 
#   scale_fill_brewer() 


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

# inc_trueN |>
#   median_qi(trueN, .width = c(0.25, 0.5, 0.95)) |> 
#   ggplot() +
#   aes(EST_YEAR, y = trueN, ymin = .lower, ymax = .upper) + 
#   geom_lineribbon()+ 
#   scale_fill_brewer()

### Decrease Scenario ####
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

# dec_trueN |>
#   group_by(EST_YEAR) |>
#   median_qi(trueN, .width = c(0.25, 0.5, 0.95)) |> 
#   ggplot() +
#   aes(EST_YEAR, y = trueN, ymin = .lower, ymax = .upper) + 
#   geom_lineribbon()+ 
#   scale_fill_brewer()

## BIND #####
trueN <- bind_rows(base_trueN, inc_trueN, dec_trueN) |> 
  mutate(SEASON = "SPRING")

trueN_summary <- trueN |> 
  group_by(SCENARIO, EST_YEAR) |> 
  summarise(total.bio = median(trueN), 
            lower = quantile(trueN, 0.025), 
            upper = quantile(trueN, 0.975))

saveRDS(trueN, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_AllTrueN.rds"))

all_bio <- bind_rows(obs_trueN, trueN_summary) |> 
  mutate(SEASON = "Spring")

saveRDS(all_bio, here("sdmtmb", "sumflounder", "data", "simulations",  "SprTrueN_Obs-Sim.rds"))


## PLOT ####
base_plot <- all_bio |> 
  filter(SCENARIO %in% c("PREDICTED", "BASELINE")) |> 
  ggplot() +
  aes(x = EST_YEAR, y = total.bio, ymin = lower, ymax = upper) +
  geom_line()+
  geom_pointrange(aes(color = SCENARIO), na.rm = TRUE) +
  scale_color_manual(values = c("#57B8FF", "black"),  labels = c(str_wrap("Simulated Baseline Abundance", width = 22, exdent = 8, whitespace_only = FALSE), str_wrap("Model Expected Abundance", width = 22, exdent = 5, whitespace_only = FALSE)))+
  labs(x = "Year", y = "Total Biomass (kg)", color = "", subtitle = "BASELINE") + 
  facet_wrap(~SEASON) +
  theme(legend.position = "bottom", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), axis.text = element_text(size = 14), plot.subtitle = element_text(size = 16, margin = margin(5, 0, 10, 0), hjust = 0.5), strip.text = element_text(size = 14), legend.title = element_text(size = 14),  legend.text = element_text(size = 14))

inc_plot <- all_bio |> 
  filter(SCENARIO %in% c("PREDICTED", "ENHANCED")) |> 
  ggplot() +
  aes(x = EST_YEAR, y = total.bio, ymin = lower, ymax = upper) +
  geom_line()+
  geom_pointrange(aes(color = SCENARIO), na.rm = TRUE) +
  scale_color_manual(values = c("#57B8FF", "black"), labels = c(str_wrap("Simulated Enhanced Abundance", width = 22, exdent = 8, whitespace_only = FALSE), str_wrap("Model Expected Abundance", width = 22, exdent = 5, whitespace_only = FALSE)))+
  labs(x = "Year", y = "Total Biomass (kg)", color = "", subtitle = "ENHANCED") + 
  facet_wrap(~SEASON) +
  theme(legend.position = "bottom", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), axis.text = element_text(size = 14), plot.subtitle = element_text(size = 16, margin = margin(5, 0, 10, 0), hjust = 0.5), strip.text = element_text(size = 14), legend.title = element_text(size = 14),  legend.text = element_text(size = 14))

dec_plot <- all_bio |> 
  filter(SCENARIO %in% c("PREDICTED", "REDUCED")) |> 
  ggplot() +
  aes(x = EST_YEAR, y = total.bio, ymin = lower, ymax = upper) +
  geom_line()+
  geom_pointrange(aes(color = fct_rev(SCENARIO)), na.rm = TRUE) +
  scale_color_manual(values = c("#57B8FF", "black"), labels = c(str_wrap("Simulated Reduced Abundance", width = 25, exdent = 7, whitespace_only = FALSE), str_wrap("Model Expected Abundance", width = 22, whitespace_only = FALSE, exdent = 5)))+
  labs(x = "Year", y = "Total Biomass (kg)", color = "", subtitle = "REDUCED") +
  facet_wrap(~SEASON) +
  theme(legend.position = "bottom", axis.title.x = element_text(size = 14, margin = margin(10, 0, 5, 0)), axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 5)), axis.text = element_text(size = 14), plot.subtitle = element_text(size = 16, margin = margin(5, 0, 10, 0), hjust = 0.5), strip.text = element_text(size = 14), legend.title = element_text(size = 14),  legend.text = element_text(size = 14))


((base_plot + theme(plot.margin = unit(c(5, 5, 5, 5), "pt"))) + (inc_plot + theme(plot.margin = unit(c(5, 5, 5, 5), "pt"))) + (dec_plot+ theme(plot.margin = unit(c(5, 5, 5, 5), "pt")))) + plot_annotation(title = "Simulated scenarios compared to GLMM abundance expectations", theme = theme(plot.title = element_text(size = 16, hjust = 0.5))) & theme(legend.position = "bottom")

ggsave("trueN_spring_scenarios2.png", plot = last_plot(), path = here("sdmtmb", "sumflounder", "plots"), width = 16, height = 7)



# trueN |> 
#   group_by(SCENARIO) |>
#   ggplot() + 
#   aes(x = EST_YEAR, y = med, color = SCENARIO)+
#   scale_color_manual(values = c("#0a4c8a", "#0B6E4F", "#57B8FF"), aesthetics = c("color")) +
#   #scale_shape_manual(values = c(0, 22, 24))+
#   geom_point() +
#   geom_line() +
#   labs(x = "Year", y = "Simulated Total Biomass (kg)", color = "Scenario") + 
#   theme(legend.position = "bottom")

ggsave("spring_scenario_trueN.png", plot = last_plot(), path = here("sdmtmb", "sumflounder", "plots"), width = 10, height = 5)
a


