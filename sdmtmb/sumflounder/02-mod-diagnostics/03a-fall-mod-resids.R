### created: 04/24/2023
### last updated: 01/02/2024

# 03a - FALL MODEL RESIDUALS ####

## OBJECTIVE ####
# calculate randomized quantile, dharma, and mcmc-sampled residuals for the best fitting fall model for summer flounder data
# 

## LOAD PACKAGES ####
library(patchwork)
library(here)
library(sdmTMB)
library(sdmTMBextra)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

sumflounder.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data"
fall.resids.dat <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/data/resids"
fall.resids.plots <- "C:/Users/amiller7/Documents/cinar-osse/sseep-analysis/sdmtmb/sumflounder/plots/fall-resids"

## LOAD DATA ####
strata_utm <- readRDS(here("data", "rds", "active_strata_utm.rds")) 

# best fit model created here("sdmtmb", "sumflounder", "01-mod-fits", "01a-fit-fall-mods.R")
fall_mod <- readRDS(here(sumflounder.dat, "mods", "fall", "m10_fall.rds"))

### Model Predictions ####
fall_mod_preds <- predict(fall_mod)

### save the data 
saveRDS(fall_mod_preds, file = here(sumflounder.dat, "fall_mod_preds.rds"))
saveRDS(fall_mod, file = here(sumflounder.dat, "fall_mod.rds"))

## DATA WRANGLE ####
# extract data used to fit the model 
fall_moddat <- fall_mod$data

## CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
# pull residuals from best fit model
fall_moddat$resids_eb <- residuals(fall_mod, type = "mle-eb") # Fixed effects are held at their MLEs and random effects are taken as their EB estimates. 

fall_moddat$resids_mvn <- residuals(fall_mod, type = "mle-mvn") # Fixed effects are held at their MLEs and random effects are taken from a single approximate posterior sample. The "approximate" part refers to the sample being taken from the random effects' assumed MVN distribution

# plot frequency of residuals to find distribution
hist(fall_moddat$resids_eb, main = "Histogram of summer flounder fall model residuals", xlab = "Residuals")

hist(fall_moddat$resids_mvn, main = "Histogram of summer flounder fall model residuals", xlab = "Residuals")


# qplot of residuals 
qqnorm(fall_moddat$resids_eb, main = "Normal Q-Q Plot for fall summer flounder model")
qqline(fall_moddat$resids_eb) # add trend line

qqnorm(fall_moddat$resids_mvn, main = "Normal Q-Q Plot for fall summer flounder model")
qqline(fall_moddat$resids_mvn) # add trend line

# map 
# ggplot() +
#   geom_sf(data = strata_utm, fill = NA, color = "gray")+
#   geom_point(data = fall_moddat, aes(X*1000, Y*1000, color = resids)) + scale_colour_gradient2() +
#   facet_wrap(~EST_YEAR) +
#   labs(x = "Longitude", y = "Latitude", color = "Residuals")
# 
# ggsave("sf_fall_residual_map.png", last_plot(), device = "png", here(fall.resids.plots), width = 7, height = 8)

## CALCULATE DHARMa RESIDUALS ####
#simulations with the parameters fixed at their Maximum Likelihood Estimate (MLE) and predictions conditional on the fitted random effects.
simulate(fall_mod, nsim = 500) |> 
  sdmTMBextra::dharma_residuals(fall_mod)

sim_fallmod <- simulate(fall_mod, nsim = 500)
sum(fall_moddat$EXPCATCHWT == 0) / length(fall_moddat$EXPCATCHWT)
#> [1] 0.51
sum(sim_fallmod == 0)/length(sim_fallmod)
#> [1] 0.505

# My reading of DHARMa documation is that the predicted response for the 
# residuals vs. fitted plot should ideally not include the random effects:
pred_fixed <- fall_mod$family$linkinv(predict(fall_mod)$est_non_rf) # extracts model expectations for fixed effects and transforms them out of link space 
r_fall_mod <- DHARMa::createDHARMa(
  simulatedResponse = sim_fallmod,
  observedResponse = fall_moddat$EXPCATCHWT,
  fittedPredictedResponse = pred_fixed
)

saveRDS(r_fall_mod, here(fall.resids.dat, "sf_fall_dharma_obj.rds"))
# plot(r_fall_mod)

# fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
DHARMa::testQuantiles(r_fall_mod)

# tests if the simulated dispersion is equal to the observed dispersion
DHARMa::testDispersion(r_fall_mod)

# residuals against predictor depth
#DHARMa::testResiduals(r_spr_mod, form = spr_moddat$AVGDEPTH)
DHARMa::plotResiduals(r_fall_mod, form = fall_moddat$AVGDEPTH)


# residuals against predictor year
DHARMa::plotResiduals(r_fall_mod, form = fall_moddat$EST_YEAR)
# 
DHARMa::testCategorical(r_fall_mod, catPred = fall_moddat$EST_YEAR)
DHARMa::testCategorical(r_fall_mod, catPred = fall_moddat$AREA)

DHARMa::testResiduals(r_fall_mod)

# tests for spatial autocorrelation in the residuals. Can also be used with a generic distance function
DHARMa::testSpatialAutocorrelation(r_fall_mod, x = fall_moddat$X, y = fall_moddat$Y)

#DHARMa::testTemporalAutocorrelation(r_spr_mod, time = spr_moddat2$EST_YEAR)

# tests if there are more zeros in the data than expected from the simulations
DHARMa::testZeroInflation(r_fall_mod)

### CALCULATE MCMC-BASED RESIDUALS ####
sf_fall_samps <- sdmTMBextra::predict_mle_mcmc(fall_mod, mcmc_iter = 300, mcmc_warmup = 200, stan_args = list(thin = 10))

sf_fall_mcres <- residuals(fall_mod, type = "mle-mcmc", mcmc_samples = sf_fall_samps)

qqnorm(sf_fall_mcres,  main = "Normal Q-Q plot of MCMC sampled residuals for the fall summer flounder model")
qqline(sf_fall_mcres) # add trendline

### save the data
saveRDS(fall_mod_mcres, here(fall.resids.dat, "fall_mcres.rds"))

# add the MCMC residuals to the fall data
fall_moddat$mc_resids <- fall_mod_mcres

### save the data
saveRDS(fall_moddat, file = here(fall.resids.dat, "fall-moddat-resids.rds"))


## DIAGNOSTIC PLOTS ####
ggplot(fall_moddat) +
  geom_point(aes(x = exp(preds), y = mc_resids)) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~SEASON) +
  labs(x = "Biomass estimates in link space", y = "MCMC Residual", subtitle = "Distribution of MCMC residuals for the fall summer flounder model")
ggsave("sf_fall_mcmc-resids-v-biomass.png", last_plot(), device = "png", here(fall.resids.plots), width = 7, height = 5)

ggplot() +
  geom_sf(data = strata_utm, fill = NA, color = "gray")+
  geom_point(data = fall_moddat, aes(X*1000, Y*1000, color = resids)) + scale_colour_gradient2() +
  labs(x = "Longitude", y = "Latitude", color = "Residuals", subtitle =  "Laplace residuals for the fall summer flounder model")
ggsave("sf_fall_laplace-resids_map.png", last_plot(), device = "png", here(fall.resids.plots), width = 7, height = 5)

ggplot() +
  geom_sf(data = strata_utm, fill = NA, color = "gray")+
  geom_point(data = fall_moddat, aes(X*1000, Y*1000, color = mc_resids)) + scale_colour_gradient2() +
  labs(x = "Longitude", y = "Latitude", color = "MCMC Residuals", subtitle =  "MCMC residuals for the fall summer flounder model")
ggsave("sf_fall_mcmc-resids_map.png", last_plot(), device = "png", here(fall.resids.plots), width = 7, height = 5)

