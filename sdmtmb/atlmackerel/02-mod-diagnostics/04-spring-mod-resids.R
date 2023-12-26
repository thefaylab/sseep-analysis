### created: 11/06/2023
### last updated: 12/26/2023

# 04 - SPRING MODEL RESIDUALS ####

## OBJECTIVE ####
# calculate randomized quantile, dharma, and mcmc-based residuals for the tweedie and delta poisson gamma model with a fourth order polynomial relationship on depth
# plot the residuals 

### LOAD PACKAGES ####
library(patchwork)
library(here)
library(sdmTMB)
library(sdmTMBextra)
suppressPackageStartupMessages(library(tidyverse))
theme_set(theme_bw())

### LOAD DATA ####
#strata <- readRDS(here("data", "rds", "active_strata.rds"))

# best fit model created in  here("sdmtmb", "atlmackerel", "01-mod-fits", "01-fit-spr-mods.R")
# spring_mod <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m12_spring.rds"))
# 
# # save so that spring mod will be called in all future scripts 
# saveRDS(spring_mod, here("sdmtmb", "atlmackerel", "data", "spring_mod.rds"))

### model fits when observations at depths greater than 200m are removed; created here("sdmtmb", "atlmackerel", "01-mod-fits", "explore-polynomial-mods.Rmd")
# tweedie model
m14_tweedie <- readRDS(file = here("sdmtmb", "atlmackerel", "data", "mods", "m14_spring_rm200.rds"))

# delta poisson gamma model
m14_dpg <- readRDS(file = here("sdmtmb", "atlmackerel", "data", "mods", "m14_spring_dpg_rm200.rds"))

## DATA WRANGLE ####
# extract data used to fit the models 
tm14_moddat <- m14_tweedie$data

dpg14_moddat <- m14_dpg$data

## TWEEDIE MODEL ####
### CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
#### LAPLACE RESIDUALS ####
# pull residuals from best fit model
tm14_moddat$resids <- residuals(m14_tweedie) 

# plot frequency of residuals to find distribution 
hist(tm14_moddat$resids) 

# qqplot of residuals
qqnorm(tm14_moddat$resids) 
qqline(tm14_moddat$resids) # add trend line


#### DHARMa RESIDUALS ####
#simulations with the parameters fixed at their Maximum Likelihood Estimate (MLE) and predictions conditional on the fitted random effects.
# simulate(m14_tweedie, nsim = 500) |> 
#   sdmTMBextra::dharma_residuals(spring_mod)

sim_tm14 <- simulate(m14_tweedie, nsim = 500)
# check that the model is simulating the same proportion of zeroes as the data for model fit
sum(tm14_moddat$EXPCATCHWT == 0) / length(tm14_moddat$EXPCATCHWT)
#> [1] 0.667
sum(sim_tm14 == 0)/length(sim_tm14)
#> [1] 0.699


# residuals vs. fitted plot should ideally not include the random effects:
tm_preds_fixed <- m14_tweedie$family$linkinv(predict(m14_tweedie)$est_non_rf) # extracts model expectations for fixed effects and transforms them out of link space 
r_tm_mod <- DHARMa::createDHARMa(
  simulatedResponse = sim_tm14,
  observedResponse = tm14_moddat$EXPCATCHWT,
  fittedPredictedResponse = tm_preds_fixed
)
plot(r_tm_mod)

# fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
DHARMa::testQuantiles(r_tm_mod)

# tests if the simulated dispersion is equal to the observed dispersion
DHARMa::testDispersion(r_tm_mod)

# residuals against predictor depth
#DHARMa::testResiduals(r_spr_mod, form = spr_moddat$AVGDEPTH)
DHARMa::plotResiduals(r_tm_mod, form = tm14_moddat$AVGDEPTH)


# residuals against predictor year
DHARMa::plotResiduals(r_tm_mod, form = tm14_moddat$EST_YEAR)
# 
DHARMa::testCategorical(r_tm_mod, catPred = tm14_moddat$EST_YEAR)
#DHARMa::testCategorical(r__mod, catPred = spr_moddat$AREA)

DHARMa::testResiduals(r_tm_mod)

# tests for spatial autocorrelation in the residuals. Can also be used with a generic distance function
DHARMa::testSpatialAutocorrelation(r_tm_mod, x = tm14_moddat$X, y = tm14_moddat$Y)

#DHARMa::testTemporalAutocorrelation(r_spr_mod, time = spr_moddat2$EST_YEAR)

# tests if there are more zeros in the data than expected from the simulations
DHARMa::testZeroInflation(r_tm_mod)


#### MCMC-BASED RESIDUALS ####
#### For full dataset 
# spr_samps <- sdmTMBextra::predict_mle_mcmc(spring_mod, mcmc_iter = 201, mcmc_warmup = 200)
# # add thin argument, do burn in 200 and samps 10, thin = 10 = 300 samps 
# # only interested in big ones - compute residuals for those large fitted values 
# # read vignette Dharma residuals to work through 
# 
# spr_mod_mcres <- residuals(spring_mod, type = "mle-mcmc", mcmc_samples = spr_samps)
# 
# # qqplot
# qqnorm(spr_mod_mcres) 
# qqline(spr_mod_mcres) # add trendline
# 
# ### save the data
# saveRDS(spr_mod_mcres, here("sdmtmb", "atlmackerel", "data", "spr_mcres.rds"))
# 
# ### FITTED VS RESIDUALS 
# ggplot(spr_moddat) +
#   geom_point(aes(x = EXPCATCHWT, y = mc_resids)) + 
#   facet_wrap(~SEASON) + 
#   labs(x = "Biomass (kg)", y = "MCMC Residual")
# 
# ggsave("spring_fit-v-resid.png", plot = last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 4)

### RESIDUAL PLOTTED BY SPACE ####
ggplot(tm14_moddat, aes(DECDEG_BEGLON, DECDEG_BEGLAT, col = resids)) + scale_colour_gradient2()+#low ="#5dc5e9", high = "#0a4c8a") +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Residuals") +
  #theme_bw() +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        #panel.border = element_rect(fill = NA, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))


# ggsave("spring_resid_map.png", plot = last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 5)

## DELTA MODEL ####
### CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
#### LAPLACE RESIDUALS ####
# pull residuals from best fit model
dpg14_moddat$resids <- residuals(m14_dpg) 
dpg14_moddat$resids1 <- residuals(m14_dpg, model = 1) 
dpg14_moddat$resids2 <- residuals(m14_dpg, model = 2) 

# plot frequency of residuals to find distribution
hist(dpg14_moddat$resids)
hist(dpg14_moddat$resids1)
hist(dpg14_moddat$resids2)

# qqplot of residuals
qqnorm(dpg14_moddat$resids)
qqline(dpg14_moddat$resids)

# qqplot of residuals
qqnorm(dpg14_moddat$resids1)
qqline(dpg14_moddat$resids1) # add trend line

qqnorm(dpg14_moddat$resids2)
qqline(dpg14_moddat$resids2) # add trend line

#### DHARMa RESIDUALS ####
#simulations with the parameters fixed at their Maximum Likelihood Estimate (MLE) and predictions conditional on the fitted random effects.
# simulate(m14_dpg, nsim = 500) |> 
#   sdmTMBextra::dharma_residuals(m14_dpg)

sim_dpg <- simulate(m14_dpg, nsim = 500)
sum(dpg14_moddat$EXPCATCHWT == 0) / length(dpg14_moddat$EXPCATCHWT)
#> [1] 0.667
sum(sim_dpg == 0)/length(sim_dpg)
#> [1] 0.541


# residuals vs. fitted plot should ideally not include the random effects:
preds1_fixed <- m14_dpg$family[[1]]$linkinv(predict(m14_dpg)$est_non_rf1) # extracts model expectations for fixed effects and transforms them out of link space 

preds2_fixed <- m14_dpg$family[[2]]$linkinv(predict(m14_dpg)$est_non_rf2)

r1_dpg <- DHARMa::createDHARMa(
  simulatedResponse = sim_dpg,
  observedResponse = dpg14_moddat$EXPCATCHWT,
  fittedPredictedResponse = preds1_fixed
)
plot(r1_dpg)

r2_dpg <- DHARMa::createDHARMa(
  simulatedResponse = sim_dpg,
  observedResponse = dpg14_moddat$EXPCATCHWT,
  fittedPredictedResponse = preds2_fixed
)
plot(r2_dpg)

# fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
DHARMa::testQuantiles(r1_dpg)
DHARMa::testQuantiles(r2_dpg)

# tests if the simulated dispersion is equal to the observed dispersion
DHARMa::testDispersion(r1_dpg)
DHARMa::testDispersion(r2_dpg)

# residuals against predictor depth
#DHARMa::testResiduals(r_spr_mod, form = spr_moddat$AVGDEPTH)
DHARMa::plotResiduals(r1_dpg, form = dpg14_moddat$AVGDEPTH)
DHARMa::plotResiduals(r2_dpg, form = dpg14_moddat$AVGDEPTH)

# residuals against predictor year
DHARMa::plotResiduals(r1_dpg, form = dpg14_moddat$EST_YEAR)
DHARMa::plotResiduals(r2_dpg, form = dpg14_moddat$EST_YEAR)
# 
DHARMa::testCategorical(r1_dpg, catPred = dpg14_moddat$EST_YEAR)
DHARMa::testCategorical(r2_dpg, catPred = dpg14_moddat$EST_YEAR)

DHARMa::testResiduals(r1_dpg)
DHARMa::testResiduals(r2_dpg)

# tests for spatial autocorrelation in the residuals. Can also be used with a generic distance function
DHARMa::testSpatialAutocorrelation(r1_dpg, x = dpg14_moddat$X, y = dpg14_moddat$Y)
DHARMa::testSpatialAutocorrelation(r2_dpg, x = dpg14_moddat$X, y = dpg14_moddat$Y)

#DHARMa::testTemporalAutocorrelation(r_spr_mod, time = spr_moddat2$EST_YEAR)

# tests if there are more zeros in the data than expected from the simulations
DHARMa::testZeroInflation(r1_dpg)

DHARMa::testZeroInflation(r2_dpg)


#### MCMC-BASED RESIDUALS ####
#### For full dataset 
# spr_samps <- sdmTMBextra::predict_mle_mcmc(spring_mod, mcmc_iter = 201, mcmc_warmup = 200)
# # add thin argument, do burn in 200 and samps 10, thin = 10 = 300 samps 
# # only interested in big ones - compute residuals for those large fitted values 
# # read vignette Dharma residuals to work through 
# 
# spr_mod_mcres <- residuals(spring_mod, type = "mle-mcmc", mcmc_samples = spr_samps)
# 
# # qqplot
# qqnorm(spr_mod_mcres) 
# qqline(spr_mod_mcres) # add trendline
# 
# ### save the data
# saveRDS(spr_mod_mcres, here("sdmtmb", "atlmackerel", "data", "spr_mcres.rds"))
# 
# ### FITTED VS RESIDUALS 
# ggplot(spr_moddat) +
#   geom_point(aes(x = EXPCATCHWT, y = mc_resids)) + 
#   facet_wrap(~SEASON) + 
#   labs(x = "Biomass (kg)", y = "MCMC Residual")
# 
# ggsave("spring_fit-v-resid.png", plot = last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 4)

### RESIDUAL PLOTTED BY SPACE ####
ggplot(dpg14_moddat, aes(DECDEG_BEGLON, DECDEG_BEGLAT, col = resids)) + scale_colour_gradient2()+#low ="#5dc5e9", high = "#0a4c8a") +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Residuals") +
  #theme_bw() +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        #panel.border = element_rect(fill = NA, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

# 
# ggsave("spring_resid_map.png", plot = last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 5)
