### created: 04/18/2023
### last updated: 08/14/2023

# 03a - CV OF FALL PREDICTIONS ####

## OBJECTIVE ####
# Estimate the uncertainty in our spatiotemporal density predictions using simulations from the joint precision matrix 
# generate 100 estimates 
# calculate upper and lower confidence intervals, a standard deviation, and a coefficient of variation (CV).


### LOAD PACKAGES ####
library(patchwork)
library(here)
library(sdmTMB)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
#source()
set.seed(120)

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

### LOAD DATA ####
# read in fall grid for predicting created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "sf_fall_grid_Jun2022.rds"))

# read in fall model without REML created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_mod <- radRDS(here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

# read in fall predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R")
fall_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "fall_predictions.rds"))


## GENERATE 100 ESTIMATES ####
# replicate the grid for each year
fall_grid <- sdmTMB::replicate_df(fall_grid, "EST_YEAR", c(2009:2026)) 
# make predictions 100 times
sim <- predict(fall_mod, newdata = fall_grid, nsim = 100) 

## CALCULATE CV ####
# filter the 100 simulations for the last year
sim_last <- sim[fall_grid$EST_YEAR == max(fall_grid$EST_YEAR), ] 

# filter the predictions for the last year
pred_last <- fall_preds[fall_preds$EST_YEAR == max(fall_grid$EST_YEAR), ] 

# calculate lower confidence interval of 100 estimates
pred_last$lwr <- apply(exp(sim_last), 1, quantile, probs = 0.025)

# calculate upper confidence interval of 100 estimates
pred_last$upr <- apply(exp(sim_last), 1, quantile, probs = 0.975)

# calculate standard deviation of 100 estimates
pred_last$sd <- round(apply(exp(sim_last), 1, function(x) sd(x)), 2)

# calculate coefficient of variation of 100 estimates 
pred_last$cv <- round(apply(exp(sim_last), 1, function(x) sd(x) / mean(x)), 2)


pred_last <- pred_last |> mutate(across(c(X, Y, lwr, upr), round, digits = 2))


ggplot(pred_last, aes(X, Y, fill = cv)) +
  geom_tile(width = 10, height = 10) + 
  facet_wrap(~EST_YEAR) + 
  coord_fixed() + 
  scale_fill_viridis_c(option = "E") +
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "CV") 

saveRDS(pred_last, here("sdmtmb", "sumflounder", "data", "fall_pred_cvs.rds"))
