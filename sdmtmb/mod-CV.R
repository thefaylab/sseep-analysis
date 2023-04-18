### created: 04/18/2023
### last updated: 

#### MAKE PRETTY MAPS ####

###################
#### OBJECTIVE ####
###################
# project predictions onto map with Eastern Coast for frame of reference

####################


#### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
library(raster)
library(sdmTMB)
library(marmap)
library(oce)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())
#source()

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"


# CVs
#We can also estimate the uncertainty in our spatiotemporal density predictions using simulations from the joint precision matrix by setting nsim > 0 in the predict function. Here we generate 100 estimates and use apply() to calculate upper and lower confidence intervals, a standard deviation, and a coefficient of variation (CV).

sim <- predict(m6_fall, newdata = grid, nsim = 100)
sim_last <- sim[grid$EST_YEAR == max(grid$EST_YEAR), ] # just plot last year
pred_last <- preds[preds$EST_YEAR == max(grid$EST_YEAR), ]
pred_last$lwr <- apply(exp(sim_last), 1, quantile, probs = 0.025)
pred_last$upr <- apply(exp(sim_last), 1, quantile, probs = 0.975)
pred_last$sd <- round(apply(exp(sim_last), 1, function(x) sd(x)), 2)
pred_last$cv <- round(apply(exp(sim_last), 1, function(x) sd(x) / mean(x)), 2)
pred_last <- pred_last |> mutate(across(c(X, Y, lwr, upr), round, digits = 2))


ggplot(pred_last, aes(X, Y, fill = cv)) +
  geom_tile(width = 8, height = 8) + 
  facet_wrap(~EST_YEAR) + 
  coord_fixed() + 
  scale_fill_viridis_c(option = "E") +
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "CV") 