### created: 04/24/2023
### last updated: 01/02/2024

# 03b - SPRING MODEL RESIDUALS ####

## OBJECTIVE ####
# calculate randomized quantile, dharma, and mcmc-sampled residuals for the best fitting spring model for summer flounder data 


### LOAD PACKAGES ####
library(stringr)
library(sf)
library(patchwork)
library(here)
library(raster)
library(sdmTMB)
library(marmap)
library(oce)
library(sdmTMBextra)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())


sumflounder.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data"
spr.resids.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data/resids"
spr.resids.plots <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/plots/spring-resids"

### LOAD DATA ####
strata_utm <- readRDS(here("data", "rds", "active_strata_utm.rds")) 

# best fit model created in  here("sdmtmb", "sumflounder", "01-mod-fits", "01a-fit-spr-mods.R")
spring_mod <- readRDS(here(sumflounder.dat, "mods", "m12_spring.rds"))

### Model predictions ####
spr_mod_preds <- predict(spring_mod)

### save the predictions
saveRDS(spr_mod_preds, file = here(sumflounder.dat, "spring_mod_preds.rds"))
saveRDS(spring_mod, file = here(sumflounder.dat, "spring_mod.rds"))

## DATA WRANGLE ####
# extract data used to fit the model 
spr_moddat <- spring_mod$data

## CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
# pull residuals from best fit model
spr_moddat$resids <- residuals(spring_mod, type = "mle-laplace") 

# plot frequency of residuals to find distribution 
hist(spr_moddat$resids, main = "Histogram of residuals from the spring summer flounder model", xlab = "Residuals") 

# qqplot of residuals
qqnorm(spr_moddat$resids, main = "Normal Q-Q Plot for spring summer flounder model") 
qqline(spr_moddat$resids) # add trend line

# map 
ggplot() +
  geom_sf(data = strata_utm, fill = NA, color = "gray")+
  geom_point(data = spr_moddat, aes(X*1000, Y*1000, color = resids)) + scale_colour_gradient2() +
  facet_wrap(~EST_YEAR) +
  labs(x = "Longitude", y = "Latitude", color = "Residuals")

ggsave("sf_spring_residual_map.png", last_plot(), device = "png", here(spr.resids.plots), width = 7, height = 8)


### DHARMa RESIDUALS ####
#simulations with the parameters fixed at their Maximum Likelihood Estimate (MLE) and predictions conditional on the fitted random effects.
# simulate(spring_mod, nsim = 500) |> 
#   sdmTMBextra::dharma_residuals(spring_mod)

sim_sprmod <- simulate(spring_mod, nsim = 500)
sum(spr_moddat$EXPCATCHWT == 0) / length(spr_moddat$EXPCATCHWT)
#> [1] 0.407
sum(sim_sprmod == 0)/length(sim_sprmod)
#> [1] 0.403

# My reading of DHARMa documation is that the predicted response for the 
# residuals vs. fitted plot should ideally not include the random effects:
pred_fixed <- spring_mod$family$linkinv(predict(spring_mod)$est_non_rf) # extracts model expectations for fixed effects and transforms them out of link space 
r_spr_mod <- DHARMa::createDHARMa(
  simulatedResponse = sim_sprmod,
  observedResponse = spr_moddat$EXPCATCHWT,
  fittedPredictedResponse = pred_fixed
)

saveRDS(r_spr_mod, file = here(spr.resids.dat, "spring_dharma_obj.rds"))
# plot(r_spr_mod)

# fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
DHARMa::testQuantiles(r_spr_mod)

# tests if the simulated dispersion is equal to the observed dispersion
DHARMa::testDispersion(r_spr_mod)

# residuals against predictor depth
#DHARMa::testResiduals(r_spr_mod, form = spr_moddat$AVGDEPTH)
DHARMa::plotResiduals(r_spr_mod, form = spr_moddat$AVGDEPTH)


# residuals against predictor year
DHARMa::plotResiduals(r_spr_mod, form = spr_moddat$EST_YEAR)
# 
DHARMa::testCategorical(r_spr_mod, catPred = spr_moddat$EST_YEAR)
DHARMa::testCategorical(r_spr_mod, catPred = spr_moddat$AREA)

DHARMa::testResiduals(r_spr_mod)

# tests for spatial autocorrelation in the residuals. Can also be used with a generic distance function
DHARMa::testSpatialAutocorrelation(r_spr_mod, x = spr_moddat$X, y = spr_moddat$Y)

#DHARMa::testTemporalAutocorrelation(r_spr_mod, time = spr_moddat2$EST_YEAR)

# tests if there are more zeros in the data than expected from the simulations
DHARMa::testZeroInflation(r_spr_mod)

### CALCULATE MCMC-BASED RESIDUALS ####
sf_spr_samps <- sdmTMBextra::predict_mle_mcmc(spring_mod, mcmc_iter = 300, mcmc_warmup = 200, stan_args = list(thin = 10))

sf_spr_mcres <- residuals(spring_mod, type = "mle-mcmc", mcmc_samples = sf_spr_samps)


# qqplot
qqnorm(sf_spr_mcres, main = "Normal Q-Q plot of MCMC sampled residuals for the spring summer flounder model")
qqline(sf_spr_mcres) # add trendline

### save the data
saveRDS(sf_spr_mcres, here(spr.resids.dat, "sf_spr_mcres.rds"))

# add the MCMC residuals to the spring data
spr_moddat$mc_resids <- sf_spr_mcres

### save the data
saveRDS(spr_moddat, file = here(spr.resids.dat, "spring-moddat-resids.rds"))


### FITTED VS RESIDUALS 
ggplot(spring_moddat) +
  geom_point(aes(x = preds, y = mc_resids)) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~SEASON) +
  labs(x = "Biomass predictions in link space", y = "MCMC Residual", subtitle =  "Distribution of MCMC residuals for the spring summer flounder model")
ggsave("sf_spring_mcmc-resids-v-biomass.png", last_plot(), device = "png", here(spr.resids.plots), width = 8, height = 5)

ggplot() +
  geom_sf(data = strata_utm, fill = NA, color = "gray")+
  geom_point(data = spring_moddat, aes(X*1000, Y*1000, color = resids)) + scale_colour_gradient2() +
  labs(x = "Longitude", y = "Latitude", color = "Residuals", subtitle = "Laplace residuals for the spring summer flounder model")
ggsave("sf_spring_laplace-resids-map.png", last_plot(), device = "png", here(spr.resids.plots), width = 8, height = 5)

ggplot() +
  geom_sf(data = strata_utm, fill = NA, color = "gray")+
  geom_point(data = spring_moddat, aes(X*1000, Y*1000, color = mc_resids)) + scale_colour_gradient2() +
  labs(x = "Longitude", y = "Latitude", color = "MCMC Residuals", subtitle = "MCMC residuals for the spring summer flounder model")
ggsave("sf_spring_mcmc-resids-map.png", last_plot(), device = "png", here(spr.resids.plots), width = 8, height = 5)
