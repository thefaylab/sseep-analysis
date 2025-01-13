### created: 04/24/2024
### last updated: 11/10/2024

# 05 - DIAGNOSTICS: Residuals ####

## OBJECTIVE ####
# Calculate randomized quantile residuals for best model fits 
# fixed effects are held at their MLEs and random effects are takje either: 
## from a single approximate posterior sampled assuming a multivariate normal distribution (MVN), or 
## from their emprirical bayes (EB) estimates 
# see ?sdmTMB::residuals.sdmTMB for more information

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
# library(sf) 
library(sdmTMB)
# library(kableExtra)
set.seed(123)
theme_set(theme_bw())

### Environment Set Up ####
# season 
season <- "fall"

# species
species <- "sumflounder"

species_name <- "summer flounder"

### File locations ####
dat.files <- here("data", "rds", "sdmtmb",  species)
plot.files <- here("outputs", "sdmtmb",  species, "plots", "resids")


### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, "mods", season, str_c("m10_", season, ".rds", sep = ""))) ## FIXME as needed 

### Extract data ####
moddat <- mod$data

### Model predictions ####
mod_preds <- predict(mod)

## CALCULATE AND PLOT RANDOMIZED QUANTILE RESIDUALS ####
### From Empirical Bayes Estimates ####
# Fixed effects are held at their MLEs and random effects are taken as their EB estimates.
mod_preds$resids_eb <- residuals(mod, type = "mle-eb") 

### For delta gamma models 
# mod_preds$resids_eb1 <- residuals(mod, type = "mle-eb", model = 1)
# mod_preds$resids_eb2 <- residuals(mod, type = "mle-eb", model = 2)

#### Plots ####
# plot frequency of residuals to find distribution
eb_resids_hist <- ggplot(mod_preds) + geom_histogram(aes(x = resids_eb), bins = 12, fill = "grey", color = "black") + labs(x = "Empirical Bayes Residuals", y = "Frequency", subtitle = str_c("Histogram of", season, species_name, "model residuals", sep = " "))

# qqplot of residuals 
eb_resids_qqplot <- ggplot(mod_preds, aes(sample = resids_eb)) + stat_qq(shape = 21) + stat_qq_line(linetype = 2) + labs(x = "Empirical Bayes Residuals", y = "Frequency", subtitle = str_c("Normal Q-Q Plot of empirical bayes residuals for", season, species_name, "model", sep = " "))


### From MVN Posterior ####
# Fixed effects are held at their MLEs and random effects are taken from a single approximate posterior sample. The "approximate" part refers to the sample being taken from the random effects' assumed MVN distribution
mod_preds$resids_mvn <- residuals(mod, type = "mle-mvn") 

### For delta gamma models 
# mod_preds$resids_mvn1 <- residuals(mod, type = "mle-mvn", model = 1)
# mod_preds$resids_mvn2 <- residuals(mod, type = "mle-mvn", model = 2)

#### Plots ####
# plot frequency of residuals to find distribution
mvn_resids_hist <- ggplot(mod_preds) + geom_histogram(aes(x = resids_mvn), bins = 12, fill = "grey", color = "black") + labs(x = "MVN Posterior Residuals", y = "Frequency", subtitle = str_c("Histogram of", season, species_name, "model residuals", sep = " "))

# qqplot of residuals
mvn_resids_qqplot <- ggplot(mod_preds, aes(sample = resids_mvn)) + stat_qq(shape = 21) + stat_qq_line(linetype = 2) + labs(x = "Theoretical Quantiles", y = "Sample Quantiles", subtitle = str_c("Normal Q-Q Plot of MVN posterior residuals for", season, species_name, "model", sep = " "))

#### Save the data ###
# saveRDS(mod_preds, file = here(dat.files, "resids", str_c(season, species, "resid-dat.rds", sep = "_"))) ## FIXME when repo is reorganized

### From MCMC Chains ####
# Fixed effects are held at their MLEs and random effects are taken from a single posterior sample obtained with MCMC. These are an excellent option since they make no assumption about the distribution of the random effects 
tic()
samps <- sdmTMBextra::predict_mle_mcmc(mod, mcmc_iter = 800, mcmc_warmup = 400, nsim = 10)

# create sublists of samples for iteration 
samps_iter <- samps |> as.data.frame() |> # create dataframe from matrix of samples to pivot 
  pivot_longer(cols = everything(), cols_vary = "slowest", names_to = "rep", names_prefix = "V", values_to = "samp") |> # consolidate replicated samples into one column by pivoting longer 
  group_by(rep) |> # group by replicate
  group_split() # create list item of length(moddat) by each replicate

# sample residuals over each iteration 
mcres <- map(samps_iter, ~pluck(.,"samp")) |> # select posterior samples from each replicate
        map(~residuals(mod, type = "mle-mcmc", mcmc_samples = .x)) |> # supply each vector of samples to the residuals function 
  map2(samps_iter, ~tibble(mcmc_resids = .x) |> bind_cols(.y) |> relocate(mcmc_resids, .after = everything())) # bind the calculated residuals back to the posterior samples list to identify by replicate 

# add the MCMC residuals to the data
modpreds_full <- map2_dfr(rep(list(mod_preds), length(mcres)), mcres, ~bind_cols(.x, .y))
toc()

### for delta models 
modpreds1 <- map2(rep(list(mod_preds), length(mcres1)), mcres1, ~bind_cols(.x, .y))
modpreds_full <- map(mcres2, ~select(., mcmc_resids) |> rename(mcmc_resids2 = mcmc_resids)) |> map2_dfr(modpreds1, ~bind_cols(.y, .x))


#### Plots ####
# histogram
mcmc_resids_hist <- ggplot(modpreds_full) + geom_histogram(aes(x = mcmc_resids), fill = "grey", color = "black") + labs(x = "MCMC-sampled Posterior Residuals", y = "Frequency", subtitle = str_c("Histogram of", season, species_name, "model residuals", sep = " "))

# qqplot of residuals
mcmc_resids_qqplot <- ggplot(modpreds_full, aes(sample = mcmc_resids)) + stat_qq(shape = 21) + stat_qq_line(linetype = 2) + labs(x = "Theoretical Quantiles", y = "Sample Quantiles", subtitle = str_c("Normal Q-Q Plot of MCMC-sampled posterior residuals for", season, species_name, "model", sep = " "))

# predicted(fitted) vs residual plot 
fit.mcres_plot <- ggplot(modpreds_full) + geom_point(aes(x = est, y = mcmc_resids)) + geom_hline(yintercept = 0) + labs(x = "Biomass estimates in link space", y = "MCMC-sampled Posterior Residual", subtitle = str_c(str_to_sentence(season), species_name, "model residuals versus estimates of biomass", sep = " "))

# predictor vs residual plot 
depth.mcres_plot <- ggplot(modpreds_full) + geom_point(aes(x = AVGDEPTH, y = mcmc_resids)) + geom_hline(yintercept = 0) + labs(x = "Average Depth", y = "MCMC-sampled Posterior Residual", subtitle = str_c(str_to_sentence(season), species_name, "model residuals versus depth", sep = " "))

year.mcres_plot <- ggplot(modpreds_full) + geom_point(aes(x = EST_YEAR, y = mcmc_resids)) + geom_hline(yintercept = 0) + labs(x = "Year", y = "MCMC-sampled Posterior Residual", subtitle = str_c(str_to_sentence(season), species_name, "model residuals", sep = " "))



### Save the data ####
## FIXME when repo is reorganized 
data <- list("resid-dat.rds" = mod_preds, 
             "mcmc-samps.rds" = samps, 
             "mcmc-samps_iter_list.rds" = samps_iter, 
             "mcresids.rds" = mcres, 
             "resid-dat-full.rds" = modpreds_full) 

plots <- list("eb-resids_hist.png" = eb_resids_hist, 
              "eb-resids_qqplot.png" = eb_resids_qqplot, 
              "mvn-resids_hist.png" = mvn_resids_hist, 
              "mvn-resids_qqplot.png" =  mvn_resids_qqplot, 
              "mcmc-resids_hist.png" = mcmc_resids_hist, 
              "mcmc-resids_qqplot.png" = mcmc_resids_qqplot, 
              "fit-mcres_plot.png" = fit.mcres_plot,
              "depth-mcres_plot.png" = depth.mcres_plot, 
              "year-mcress_plot.png" = year.mcres_plot) 

pmap(list(data, names(data)), ~saveRDS(.x, file = here(dat.files, species, "resids", str_c(season, species, .y, sep = "_"))))

pmap(list(plots, names(plots)), ~ggsave(plot = .x, filename = str_c(season, .y, sep = "_"), device = "png", path = here(plot.files), width = 9, height = 5))

# saveRDS(samps, file = here(dat.files, "resids", str_c(season, species, "mcmc-samps.rds", sep = "_")))
# saveRDS(samps_iter, file = here(dat.files, "resids", str_c(season, species, "mcmc-samps_iter-list.rds", sep = "_")))
# saveRDS(mcres, file = here(dat.files, "resids", str_c(season, species, "mcmc-resids.rds", sep = "_"))) 
# saveRDS(modpreds_full, file = here(dat.files, "resids", str_c(season, species, "resid-dat.rds", sep = "_")))


## CALCULATE DHARMa RESIDUALS ####
# simulations with the parameters fixed at their Maximum Likelihood Estimate (MLE) and predictions conditional on the fitted random effects
sim_mod <- simulate(mod, nsim = 500, type = "mle-mvn")
# saveRDS(sim_mod, here(dat.files, "resids", str_c(season, "mod_sim_resids.rds", sep = "_")))

dim(sim_mod) # row = nrow(moddat), cols = sim draws 

# compare observed number of zeros to model simulated number of zeros
sum(moddat$EXPCATCHWT == 0) / length(moddat$EXPCATCHWT)

sum(sim_mod == 0)/length(sim_mod)


# plot DHARMa residuals 
dharma_residuals(sim_mod, mod)

# create a DHARMa object for use in other tools 
# r_fall_mod <- DHARMa::createDHARMa(
#   simulatedResponse = sim_mod,
#   observedResponse = moddat$EXPCATCHWT,
#   fittedPredictedResponse = pred_fixed
# )
r_mod <- sdmTMB::dharma_residuals(sim_mod, mod, return_DHARMa = TRUE)
saveRDS(r_mod, here(dat.files, "resids", str_c(season, "dharma_resids.rds", sep = "_")))

# plot residuals 
plot(r_mod) # plot in right hand panel represents simulated residuals against the prediction without the random effects
# the significant quantile regression will be highlighted as red, and a warning will be displayed in the plot.


DHARMa::testResiduals(r_mod)

# tests for spatial autocorrelation in the residuals. Can also be used with a generic distance function
DHARMa::testSpatialAutocorrelation(r_mod, x = moddat$X, y = moddat$Y)

# tests if there are more zeros in the data than expected from the simulations
DHARMa::testZeroInflation(r_mod)

# tests if the simulated dispersion is equal to the observed dispersion
DHARMa::testDispersion(r_mod)

# plot residuals against predictor depth
DHARMa::plotResiduals(r_mod, form = moddat$AVGDEPTH)
# the significant quantile regression will be highlighted as red, and a warning will be displayed in the plot.

# plot residuals against predictor year
DHARMa::plotResiduals(r_mod, form = moddat$EST_YEAR)
# under H0 (perfect model), we would expect those boxes to range homogenously from 0.25-0.75.

DHARMa::testCategorical(r_mod, catPred = moddat$EST_YEAR)





