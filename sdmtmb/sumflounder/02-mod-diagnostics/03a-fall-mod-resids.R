### created: 04/24/2023
### last updated: 12/13/2023

# 03a - FALL MODEL RESIDUALS ####

## OBJECTIVE ####
# calculate randomized quantile and dharma residuals for the best fitting fall model 
# 

## LOAD PACKAGES ####
library(patchwork)
library(here)
library(sdmTMB)
library(sdmTMBextra)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

## LOAD DATA ####
#strata <- readRDS(here("data", "rds", "active_strata.rds")) 

# best fit model created here("sdmtmb", "sumflounder", "01-mod-fits", "01a-fit-fall-mods.R")
fall_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "mods", "m12_fall.rds"))

### Model Predictions ####
fall_mod_preds <- predict(fall_mod)

### save the data 
saveRDS(fall_mod_preds, file = here("sdmtmb", "sumflounder", "data", "fall_mod_preds.rds"))
saveRDS(fall_mod, file = here("sdmtmb", "sumflounder", "data", "fall_mod.rds"))

## DATA WRANGLE ####
# extract data used to fit the model 
fall_moddat <- fall_mod$data

## CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
# pull residuals from best fit model
fall_moddat$resids <- residuals(fall_mod, type = "mle-laplace") 

# plot frequency of residuals to find distribution
hist(fall_moddat$resids, main = "Histogram of Summer flounder fall model residuals", xlab = "Residuals")

# qplot of residuals 
qqnorm(fall_moddat$resids, main = "Normal Q-Q Plot for fall summer flounder model")
qqline(fall_moddat$resids) # add trend line

# map 
ggplot() +
  geom_sf(data = strata_utm, fill = NA, color = "gray")+
  geom_point(data = fall_moddat, aes(X*1000, Y*1000, color = resids)) + scale_colour_gradient2() +
  facet_wrap(~EST_YEAR) +
  labs(x = "Longitude", y = "Latitude", color = "Residuals")

ggsave("sf_fall_residual_map.png", last_plot(), device = "png", here("sdmtmb", "sumflounder", "plots", "fall-resids"), width = 7, height = 8)

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
plot(r_fall_mod)

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
# fall_samps <- sdmTMBextra::predict_mle_mcmc(fall_mod, mcmc_iter = 201, mcmc_warmup = 200)
# 
# fall_mod_mcres <- residuals(fall_mod, type = "mle-mcmc", mcmc_samples = fall_samps)
# 
# # qqplot
# qqnorm(fall_mod_mcres)
# qqline(fall_mod_mcres) # add trendline
# 
# ### save the data 
# saveRDS(fall_mod_mcres, here("sdmtmb", "sumflounder", "data", "fall_mcres.rds"))
#  
# # add the MCMC residuals to the fall data 
# fall_moddat$mc_resids <-fall_mod_mcres
# 
# ### save the data 
# saveRDS(fall_moddat, file = here("sdmtmb", "sumflounder", "data", "fall_dat-resids.rds"))


## DIAGNOSTIC PLOTS ####
### FITTED VS RESIDUALS PLOT ####
# ggplot(fall_moddat) +
#   geom_point(aes(x = EXPCATCHWT, y = mc_resids)) + 
#   facet_wrap(~SEASON) + 
#   labs(x = "Biomass (kg)", y = "MCMC Residual")
# 
# ggsave("fall_fit-v-resid.png" , plot = last_plot(), device = "png", here("sdmtmb", "sumflounder", "plots"), width = 6, height = 4)
# 
# ### RESIDUALS PLOTTED BY SPACE ####
# ggplot(fall_moddat, aes(DECDEG_BEGLON, DECDEG_BEGLAT, col = mc_resids)) + scale_colour_gradient2()+#low ="#5dc5e9", high = "#0a4c8a") +
#   geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
#   labs(x = "Longitude", 
#        y = "Latitude", 
#        color = "Residuals") +
#   #theme_bw() +
#   theme(legend.position="bottom",
#         #legend.title = element_blank(), 
#         #panel.border = element_rect(fill = NA, color = "black"),
#         axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))
# 
# ggsave("fall_resid_map.png" ,plot = last_plot(), device = "png", here("sdmtmb", "sumflounder", "plots"), width = 8, height = 5)

