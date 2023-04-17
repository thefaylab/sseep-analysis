### created: 12/10/2022
### last updated: 04/11/2023

#### 04a - FALL MODEL VISUALIZATION ####

###################
#### OBJECTIVE ####
###################
# residual checking of best and worst models fit to historical summer flounder data  

####################

#### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(ggeffects)
#remotes::install_github("pbs-assess/sdmTMBextra")
library(sdmTMBextra)
# library(marmap)
# library(raster)

here()

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

#### LOAD DATA ####
sf_fall <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds"))
sf_fall$scaled_depth <- (sf_fall$AVGDEPTH - mean(sf_fall$AVGDEPTH)) / sd(sf_fall$AVGDEPTH)
sf_fall$scaled_year <- (sf_fall$EST_YEAR - mean(sf_fall$EST_YEAR)) / sd(sf_fall$EST_YEAR)
sf_fall$scaled_area <- (sf_fall$AREA_CODE - mean(sf_fall$AREA_CODE)) / sd(sf_fall$AREA_CODE)

fall_mesh <- readRDS(here(sdmtmb.dir, "data", "fall_mesh.rds"))

# worst fit generated from `02a-fit-models.R` here("sdmtmb")
# m1 <- readRDS(file = here("sdmtmb", "model-outputs", "m1.rds"))

# best fit generated from `02a-fit-fall-mods.R` here("sdmtmb")
m10_fall <- readRDS(file = here(sdmtmb.dir, "model-outputs", "m10_fall.rds"))
m10_data <- m10_fall$data
m10fall_2020 <- filter(m10_data, EST_YEAR == 2020)

m8_fall <- readRDS(file = here(sdmtmb.dir, "model-outputs", "m8_fall2.rds"))
m8_data <- m8_fall$data
m8fall_2020 <- filter(m8_data, EST_YEAR == 2020)

m11_fall <- readRDS(file = here(sdmtmb.dir, "model-outputs", "m11_fall2.rds"))
m11_data <- m11_fall$data
m11fall_2020 <- filter(m11_data, EST_YEAR == 2020)


# fall summer flounder data prepared in `01-prepare-data.R` here("sdmtmb")
#sf_fall <- readRDS(here("sdmtmb", "data", "sumflounder_fall.rds"))
m8_sf <- bind_rows(sf_fall, m8fall_2020)
m10_sf <- bind_rows(sf_fall, m10fall_2020)
m11_sf <- bind_rows(sf_fall, m11fall_2020)


#### PREPARE RESIDUALS #### 

##### Best Fit Model ####
### m10 model 
m10_sf$m10_res <- residuals(m10_fall) # pull residuals from best fit model

hist(m10_sf$m10_res) # plot frequency of residuals to find distribution 
qqnorm(m10_sf$m10_res) # qplot 
qqline(m10_sf$m10_res) # add trend line


fit10_ml <- update(m10_fall, reml = FALSE) #refit your model with `reml = FALSE` to use MCMC-MLE residuals.
saveRDS(fit10_ml, here("sdmtmb", "data", "m10fall_reml-off.rds"))

samps10 <- sdmTMBextra::predict_mle_mcmc(fit10_ml, mcmc_iter = 201, mcmc_warmup = 200)

m10fall_mcres <- residuals(m10_fall, type = "mle-mcmc", mcmc_samples = samps10)
qqnorm(m10fall_mcres)
qqline(m10fall_mcres)

saveRDS(m10fall_mcres, here("sdmtmb", "data", "m10fall_mcres.rds"))

m10_sf$m10_mcres <- m10fall_mcres

# save the data 
saveRDS(m10_sf, file = here("sdmtmb", "data", "m10sf_fall-resids.rds"))

### m8 model 
m8_sf$m8_res <- residuals(m8_fall) # pull residuals from best fit model

hist(m8_sf$m8_res) # plot frequency of residuals to find distribution 
qqnorm(m8_sf$m8_res) # qplot 
qqline(m8_sf$m8_res) # add trend line


fit8_ml <- update(m8_fall, reml = FALSE) #refit your model with `reml = FALSE` to use MCMC-MLE residuals.
saveRDS(fit8_ml, here("sdmtmb", "data", "m8fall_reml-off.rds"))

samps8 <- sdmTMBextra::predict_mle_mcmc(fit8_ml, mcmc_iter = 201, mcmc_warmup = 200)
#Warning messages:
# 1: There were 1 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 2: Examine the pairs() plot to diagnose sampling problems

m8fall_mcres <- residuals(m8_fall, type = "mle-mcmc", mcmc_samples = samps8)
qqnorm(m8fall_mcres)
qqline(m8fall_mcres)

saveRDS(m8fall_mcres, here("sdmtmb", "data", "m8fall_mcres.rds"))

m8_sf$m8_mcres <- m8fall_mcres
# save the data 
saveRDS(m8_sf, file = here("sdmtmb", "data", "m8sf_fall-resids.rds"))

### m11 model 
m11_sf$m11_res <- residuals(m11_fall) # pull residuals from best fit model

hist(m11_sf$m11_res) # plot frequency of residuals to find distribution 
qqnorm(m11_sf$m11_res) # qplot 
qqline(m11_sf$m11_res) # add trend line

fit11_ml <- update(m11_fall, reml = FALSE) #refit your model with `reml = FALSE` to use MCMC-MLE residuals.
saveRDS(fit11_ml, here("sdmtmb", "data", "m11fall_reml-off.rds"))

samps11 <- sdmTMBextra::predict_mle_mcmc(fit11_ml, mcmc_iter = 201, mcmc_warmup = 200)

m11fall_mcres <- residuals(m11_fall, type = "mle-mcmc", mcmc_samples = samps11)
hist(m11fall_mcres)
qqnorm(m11fall_mcres)
qqline(m11fall_mcres)

saveRDS(m11fall_mcres, here(sdmtmb.dir, "data", "m11fall_mcres.rds"))

m11_sf$m11_mcres <- m11fall_mcres


# save the data 
saveRDS(m11_sf, file = here(sdmtmb.dir, "data", "m11sf_fall-resids.rds"))



sf_fall <- readRDS(here("sdmtmb", "data", "sf_fall-resids.rds")) 

#### VISUALISATIONS ####

g <- ggeffect(m6_fall, "AVGDEPTH [0:400 by=1]")
plot(g)


##### Best fit model ####
# plotting residuals of best fit model


qqnorm(m10_sf$m10_mcres) # qplot 
abline(a = 0, b = 1) # add trend line

# plotting residuals of best fit model


qqnorm(m8_sf$m8_mcres) # qplot 
abline(a = 0, b = 1) # add trend line


# from the mcmc residuals 
hist(m8_sf$m8_mcres, 
     main = "Fall MCMC Residuals for Summer Flounder", 
     xlab = "Residuals")


# qqnorm(sf_fall$m6_mcres)
# abline(a = 0, b = 1) # add trend line
#ggsave(filename = "sf_fall-m6qqplot.png", device = "png", path = here("sdmtmb", "plots"), width = 8, height = 8)


# residuals plotted by year
ggplot(m11_sf, aes(DECDEG_BEGLON, DECDEG_BEGLAT, col = m11_mcres)) + scale_colour_gradient2()+#low ="#5dc5e9", high = "#0a4c8a") +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Residuals") +
  #theme_bw() +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        #panel.border = element_rect(fill = NA, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

ggsave(filename = "sf_fall-m6-mcresids.png", device = "png", path = here("sdmtmb", "plots"), width = 8, height = 8)

# subset years to get a closer look 
m11_sf %>% 
  filter(EST_YEAR %in% c(2018, 2019, 2020, 2021)) %>% 
  ggplot(aes(X, Y, col = m11_mcres)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(title = "Fall AR1 Spatiotemporal Model Residuals ") +
  theme_bw() +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) #+ 
  #scale_color_manual(values = pal)

ggsave(filename = "sf_fall-m6-mcresids2.png", device = "png", path = here("sdmtmb", "plots"), width = 10, height = 5)

# marginal residual plot in link space
#visreg::visreg(m1, xvar = "AVGDEPTH", xlim = c(30, 350), xlab = "Depth")


# marginal residual plot of response variables
visreg::visreg(m6_fall, xvar = "AVGDEPTH", scale = "response", nn = 200, xlab = "Depth", ylab = "Biomass (kg)")



# predicted probability of biomass versus depth; plot predictions against covariates 
pred_df <- sf_fall
pred_df$pred <- predict(m6_fall)$est
#a nonlinear pattern here that shows a trend that we probably want to include. 
ggplot(pred_df, aes(AVGDEPTH, pred)) + 
  geom_point(size=0.3, alpha=0.5) + 
  geom_smooth(se = FALSE) + 
  xlab("sumflounder$AVGDEPTH") + 
  ylab("predict(m6_fall)$est")

ggsave(filename = "sf_fall-pred-depth.png", device = "png", path = here("sdmtmb", "plots"), width = 12, height = 10)

##### Worst fit model ####
# plotting residuals of worst fit model
# hist(sumflounder$m1_res) # plot frequency of residuals to find distribution 
# labs(x = "Residuals", title = "M1 Residuals for Summer Flounder")
# 
# qqnorm(sumflounder$m1_res) # qplot 
# abline(a = 0, b = 1) # add trend line


# from the mcmc residuals 
#hist(sumflounder$m1_mcres)

# qqnorm(sumflounder$m1_mcres)
# abline(a = 0, b = 1) # add trend line



# residuals plotted by year
# ggplot(sumflounder, aes(X, Y, col = m1_res)) + scale_colour_gradient2() +
#   geom_point() + facet_wrap(~year) + coord_fixed() +
#   labs(title = "m1 Residuals") +
#   theme(legend.position="bottom",
#         #legend.title = element_blank(), 
#         axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))
# 
# ggsave(filename = "sumflounder-m1res.png", device = "png", path = here("sdmtmb", "plots"), width = 8, height = 8)
# 
# # subset years to get a closer look 
# sumflounder %>% 
#   filter(year %in% c(2018, 2019, 2021)) %>% 
#   ggplot(aes(X, Y, col = m1_res)) + scale_colour_gradient2() +
#   geom_point() + facet_wrap(~year) + coord_fixed() +
#   labs(title = "m1 Residuals") +
#   theme(legend.position="bottom",
#         #legend.title = element_blank(), 
#         axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))
# 
# ggsave(filename = "sumflounder-m1res2.png", device = "png", path = here("sdmtmb", "plots"), width = 10, height = 5)

