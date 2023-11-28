### created: 11/06/2023
### last updated: 11/21/2023

# 03 - SPRING MODEL RESIDUALS ####

## OBJECTIVE ####
# calculate randomized quantile and mcmc-based residuals for the best fitting spring model 
# plot the mcmc-based residuals against the fitted data and spatially to note any potential spatial autocorrelation


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

### LOAD DATA ####
strata <- readRDS(here("data", "rds", "active_strata.rds"))

# best fit model created in  here("sdmtmb", "atlmackerel", "01-mod-fits", "01-fit-spr-mods.R")
spring_mod <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m12_spring.rds"))

# save so that spring mod will be called in all future scripts 
saveRDS(spring_mod, here("sdmtmb", "atlmackerel", "data", "spring_mod.rds"))

## DATA WRANGLE ####
# extract data used to fit the model 
spr_moddat <- spring_mod$data
quantile(spr_moddat$EXPCATCHWT, 0.95) #36.478


## CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
### LAPLACE RESIDUALS ####
# pull residuals from best fit model
spr_moddat$resids <- residuals(spring_mod) 

# plot frequency of residuals to find distribution 
hist(spr_moddat$resids) 

# qqplot of residuals
qqnorm(spr_moddat$resids) 
qqline(spr_moddat$resids) # add trend line


### DHARMa RESIDUALS ####
#simulations with the parameters fixed at their Maximum Likelihood Estimate (MLE) and predictions conditional on the fitted random effects.
simulate(spring_mod, nsim = 500) |> 
  sdmTMBextra::dharma_residuals(spring_mod)

sim_sprmod <- simulate(spring_mod, nsim = 500)
sum(spr_moddat$EXPCATCHWT == 0) / length(spr_moddat$EXPCATCHWT)
#> [1] 0.535
sum(sim_sprmod == 0)/length(sim_sprmod)
#> [1] 0.561

# My reading of DHARMa documation is that the predicted response for the 
# residuals vs. fitted plot should ideally not include the random effects:
pred_fixed <- spring_mod$family$linkinv(predict(spring_mod)$est_non_rf) # extracts model expectations for fixed effects and transforms them out of link space 
r_spr_mod <- DHARMa::createDHARMa(
  simulatedResponse = sim_sprmod,
  observedResponse = spr_moddat$EXPCATCHWT,
  fittedPredictedResponse = pred_fixed
)
plot(r_spr_mod)

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


### MCMC-BASED RESIDUALS ####
#### For full dataset ####
spr_samps <- sdmTMBextra::predict_mle_mcmc(spring_mod, mcmc_iter = 201, mcmc_warmup = 200)
# add thin argument, do burn in 200 and samps 10, thin = 10 = 300 samps 
# only interested in big ones - compute residuals for those large fitted values 
# read vignette Dharma residuals to work through 

spr_mod_mcres <- residuals(spring_mod, type = "mle-mcmc", mcmc_samples = spr_samps)

# qqplot
qqnorm(spr_mod_mcres) 
qqline(spr_mod_mcres) # add trendline

### save the data
saveRDS(spr_mod_mcres, here("sdmtmb", "atlmackerel", "data", "spr_mcres.rds"))

### FITTED VS RESIDUALS 
ggplot(spr_moddat) +
  geom_point(aes(x = EXPCATCHWT, y = mc_resids)) + 
  facet_wrap(~SEASON) + 
  labs(x = "Biomass (kg)", y = "MCMC Residual")

ggsave("spring_fit-v-resid.png", plot = last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 4)

### RESIDUAL PLOTTED BY SPACE 
ggplot(spr_moddat, aes(DECDEG_BEGLON, DECDEG_BEGLAT, col = resids)) + scale_colour_gradient2()+#low ="#5dc5e9", high = "#0a4c8a") +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Residuals") +
  #theme_bw() +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        #panel.border = element_rect(fill = NA, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))


ggsave("spring_resid_map.png", plot = last_plot(), device = "png", here("sdmtmb", "atlmackerel", "plots"), width = 6, height = 5)
