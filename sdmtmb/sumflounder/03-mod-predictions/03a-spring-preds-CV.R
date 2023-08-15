### created: 04/18/2023
### last updated: 08/14/2023

# 03a - CV OF SPRING PREDICTIONS ####

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

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"

### LOAD DATA ####
# read in spring grid for predicting created here("sdmtmb", "sumflounder", "03-mod-predictions", "01b-spring-forecasts.R")
spring_grid <- readRDS(here("sdmtmb", "sumflounder", "data", "sf_spring_grid_Jun2022.rds"))

# read in spring model without REML created here("sdmtmb", "sumflounder", "03-mod-predictions", "01b-spring-forecasts.R")
spring_mod <- radRDS(here("sdmtmb", "sumflounder", "data", "spring_mod.rds"))

# read in spring predictions created here("sdmtmb", "sumflounder", "03-mod-predictions", "01b-spring-forecasts.R")
spring_preds <- readRDS(file = here("sdmtmb", "sumflounder", "data", "spring_predictions.rds"))


## GENERATE 100 ESTIMATES ####
# replicate the grid for each year
spring_grid <- sdmTMB::replicate_df(spring_grid, "EST_YEAR", c(2009:2026)) 
# make predictions 100 times
sim <- predict(spring_mod, newdata = spring_grid, nsim = 100) 

## CALCULATE CV ####
# filter the 100 simulations for the last year
sim_last <- sim[spring_grid$EST_YEAR == max(spring_grid$EST_YEAR), ] 

# filter the predictions for the last year
pred_last <- spring_preds[spring_preds$EST_YEAR == max(spring_grid$EST_YEAR), ] 

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

### save the data 
saveRDS(pred_last, here("sdmtmb", "sumflounder", "data", "spring_pred_cvs.rds"))
