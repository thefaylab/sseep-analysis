### created: 03/06/2023
### last updated:

####  ####

###################
#### OBJECTIVE ####
###################
# 


####################


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
library(infer)
suppressPackageStartupMessages(library(tidyverse))


#### LOAD DATA ####
# observed stratified mean 
sf_stratmu <- readRDS(here("data", "rds", "strat-mu_all.rds")) %>% filter(SVSPP == 103)

# without wind tows 
FallSimMu_wow <- readRDS(here("sdmtmb", "data", "sfall_sim-wow-means.rds"))
SprSimMu_wow <- readRDS(here("sdmtmb", "data", "spr_sim-wow-means.rds"))

# with wind tows
FallSimMu_ww <- readRDS(here("sdmtmb", "data", "fall_sim-ww-means.rds"))
SprSimMu_ww <- readRDS(here("sdmtmb", "data", "spr_sim-ww-means.rds"))

# FIND CHANGES IN TREND (eg slopes) #
# observed data
sf_stratmu_lm <- sf_stratmu %>% 
  group_by(SEASON, TYPE) %>% 
  nest() %>%
  mutate(model = map(data, ~lm(stratmu ~ EST_YEAR, data = .x))) %>% 
  mutate(coefs = map(model, broom::tidy, conf.int = TRUE)) %>% 
  unnest(coefs) 
saveRDS(sf_stratmu_lm, file = here(sdmtmb.dir, "data", "sf_obs_lm.rds"))

# Simulated data
# with wind removed
fall_wow_lms <- FallSimMu_wow %>%
  select(nsim, stratmu) %>% 
  mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) %>%  #same code as before to run the models
  mutate(coef = map(model, broom::tidy, conf.int = TRUE)) %>% 
  unnest(coef) %>% 
  select(nsim, term, estimate, conf.low, conf.high) %>%
  filter(term == "EST_YEAR") |> 
  mutate(SEASON = "FALL", 
         TYPE = "With Wind Precluded")
saveRDS(fall_wow_lms, file = here(sdmtmb.dir, "data", "fall_wow_lm.rds"))

spr_wow_lms <- SprSimMu_wow %>%
  select(nsim, stratmu) %>% 
  mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) %>%  #same code as before to run the models
  mutate(coef = map(model, broom::tidy, conf.int = TRUE)) %>% 
  unnest(coef) %>% 
  select(nsim, term, estimate, conf.low, conf.high) %>%
  filter(term == "EST_YEAR") |> 
  mutate(SEASON = "SPRING", 
         TYPE = "With Wind Precluded")
saveRDS(spr_wow_lms, file = here(sdmtmb.dir, "data", "spr_wow_lm.rds"))

# with wind included
fall_ww_lms <- FallSimMu_ww %>%
  select(nsim, stratmu) %>% 
  mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) %>%  #same code as before to run the models
  mutate(coef = map(model, broom::tidy, conf.int = TRUE)) %>% 
  unnest(coef) %>% 
  select(nsim, term, estimate, conf.low, conf.high) %>%
  filter(term == "EST_YEAR") |> 
  mutate(SEASON = "FALL", 
         TYPE = "With Wind Included")
saveRDS(fall_ww_lms, file = here(sdmtmb.dir, "data", "fall_ww_lm.rds"))

spr_ww_lms <- SprSimMu_ww %>%
  select(nsim, stratmu) %>% 
  mutate(model = map(stratmu, ~lm(stratmu ~ EST_YEAR, data = .))) %>%  #same code as before to run the models
  mutate(coef = map(model, broom::tidy, conf.int = TRUE)) %>% 
  unnest(coef) %>% 
  select(nsim, term, estimate, conf.low, conf.high) %>%
  filter(term == "EST_YEAR") |> 
  mutate(SEASON = "SPRING", 
         TYPE = "With Wind Included")
saveRDS(spr_ww_lms, file = here(sdmtmb.dir, "data", "spr_ww_lm.rds"))

# # 
#observed data 
obs_slopes <- filter(sf_stratmu_lm, term == "EST_YEAR") %>% 
  select(SEASON, TYPE, estimate, conf.high, conf.low) %>%
  rename(slope = estimate, 
         lower = conf.low, 
         upper = conf.high) %>% 
  mutate(METHOD = "Observed") %>% 
  relocate(METHOD, .after = TYPE) %>%
  arrange(SEASON)
saveRDS(obs_slopes, file = here(sdmtmb.dir, "data", "obs_changes.rds"))
 
# with wind removed
fall_wow_slope <- fall_wow_lms %>% 
  ungroup() %>% 
  summarise(slope = median(estimate), 
            lower = quantile(estimate, 0.025), 
            upper = quantile(estimate, 0.975)) %>% 
  mutate(SEASON = "FALL", 
         TYPE = "With Wind Precluded", 
         METHOD = "Simulated") %>% 
  relocate(c(SEASON, TYPE, METHOD), .before = everything())
percentile_fwowslope <- fall_wow_slope %>% 
  select(lower, upper)
saveRDS(fall_wow_slope, file = here(sdmtmb.dir, "data", "fall_wow_slope.rds"))

spr_wow_slope <- spr_wow_lms %>% 
  ungroup() %>% 
  summarise(slope = median(estimate), 
            lower = quantile(estimate, 0.025), 
            upper = quantile(estimate, 0.975)) %>% 
  mutate(SEASON = "SPRING", 
         TYPE = "With Wind Precluded",
         METHOD = "Simulated") %>% 
  relocate(c(SEASON, TYPE, METHOD), .before = everything())
percentile_swowslope <- spr_wow_slope %>% 
  select(lower, upper)
saveRDS(spr_wow_slope, file = here(sdmtmb.dir, "data", "spr_wow_slope.rds"))

# with wind included
fall_ww_slope <- fall_ww_lms %>% 
  ungroup() %>% 
  summarise(slope = median(estimate), 
            lower = quantile(estimate, 0.025), 
            upper = quantile(estimate, 0.975)) %>% 
  mutate(SEASON = "FALL", 
         TYPE = "With Wind Included", 
         METHOD = "Simulated") %>% 
  relocate(c(SEASON, TYPE, METHOD), .before = everything())

percentile_fwwslope <- fall_ww_slope %>% 
  select(lower, upper)
saveRDS(fall_ww_slope, file = here(sdmtmb.dir, "data", "fall_ww_slope.rds"))

spr_ww_slope <- spr_ww_lms %>% 
  ungroup() %>% 
  summarise(slope = median(estimate), 
            lower = quantile(estimate, 0.025), 
            upper = quantile(estimate, 0.975)) %>% 
  mutate(SEASON = "SPRING", 
         TYPE = "With Wind Included", 
         METHOD = "Simulated") %>% 
  relocate(c(SEASON, TYPE, METHOD), .before = everything())
percentile_swwslope <- spr_ww_slope %>% 
  select(lower, upper)
saveRDS(spr_ww_slope, file = here(sdmtmb.dir, "data", "spr_ww_slope.rds"))

slopes <- bind_rows(obs_slopes, fall_wow_slope, spr_wow_slope, fall_ww_slope, spr_ww_slope) %>%
  mutate(ID = str_c(str_sub(SEASON,1,1), str_sub(TYPE, 11, 11), str_sub(METHOD, 1, 1))) %>%
  arrange(SEASON)
# 
# myLevels <- c("FIO", "FPO", "FIS", "FPS", "SIO", "SPO", "SIS", "SPS")
# slopes$ID <- factor(slopes$ID, levels = myLevels)
# 
# 
# slope_plot <- ggplot(slopes) + 
#   aes(x = factor(ID), y = slope, color = TYPE, shape = METHOD) + 
#   geom_point() + 
#   geom_errorbar(aes(ymin = lower, ymax = upper, color = TYPE, lty = TYPE), width = 0.2) +
#   #coord_flip() + 
#   labs(y = "slope", 
#        x = "Calculation Type") +
#   theme_minimal() +
#   theme(legend.position = "bottom") 
#   #facet_wrap(~SEASON)
# 
# ggsave(slope_plot, device = "png", path = here())

# PLOT DISTRIBUTION OF SLOPES ####
fp1 <- ggplot(fall_wow_lms) +
  geom_histogram(aes(x = estimate)) + 
  shade_confidence_interval(endpoints = percentile_fwowslope, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[2,4]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  labs(title = "A) Fall Slope Estimates with Wind Precluded", x = "Slope estimates", y = "Count") +
  theme_bw() +
  theme(title = element_text(size = 10))

fp2 <- ggplot(fall_ww_lms) +
  geom_histogram(aes(x = estimate)) + 
  shade_confidence_interval(endpoints = percentile_fwwslope, color = "#5dc5e9", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[2,4]), linetype = 6, linewidth = 1, color = "#0a4c8a") +
  xlim((-0.10), (-0.02)) +
  labs(title = "B) Fall Slope Estimates with Wind Included", x = "Slope estimates", y = "Count") +
  theme_bw() +
  theme(title = element_text(size = 10))

sp1 <- ggplot(spr_wow_lms) +
  geom_histogram(aes(x = estimate)) + 
  shade_confidence_interval(endpoints = percentile_swowslope, color = "#0a4c8a", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[4,4]), linetype = "dashed", linewidth = 1, color = "orange") +
  labs(title = "A) Spring Slope Estimates with Wind Precluded", x = "Slope estimates", y = "Count") +
  theme_bw() +
  theme(title = element_text(size = 10))

sp2 <- ggplot(spr_ww_lms) +
  geom_histogram(aes(x = estimate)) + 
  shade_confidence_interval(endpoints = percentile_swwslope, color = "#0a4c8a", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(obs_slopes[4,4]), linetype = "dashed", linewidth = 1, color = "orange") +
  labs(title = "B) Spring Slope Estimates with Wind Included", x = "Slope estimates", y = "Count") +
  theme_bw() +
  theme(title = element_text(size = 10))

fp1 + fp2 
sp1 + sp2

