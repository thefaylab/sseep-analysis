### created: 11/22/2023
### last updated: 12/09/2023

# 02a - FIT FALL MODELS ####

## OBJECTIVE ####
# fit various forms of models using prepared scup data for the fall season
# include predictor for index standardization 
# estimate a coefficient for each year 
# include inside/outside wind area designation as part of the predictor

## LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)

here()


## LOAD DATA ####
# scup data 
scup_fall <- readRDS(here("sdmtmb", "scup", "data", "scup_fall.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# mesh 
fall_mesh <- readRDS(here("sdmtmb", "scup", "data", "fall_mesh.rds"))

## MODEL FITS ####

### No random effect models ####
#### M1 ####
m1_fall_tw <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
             data = scup_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
             silent = FALSE)

sanity(m1_fall_tw)

tidy(m1_fall_tw)
tidy(m1_fall_tw, effects = "ran_pars")



#### M2 ####
m2_fall_tw <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = scup_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"), 
                  spatial = "off", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m2_fall_tw)
tidy(m2_fall_tw)
tidy(m2_fall_tw, effects = "ran_pars")



### Spatial Only Models ####
# logistic regression of scup biomass (weight) in tows as a function of AVGDEPTH with spatial random effects and index standardization to estimate a separate intercept for each EST_YEAR
# 
#### M3 ####
m3_fall_tw <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
             data = scup_fall, 
             mesh = fall_mesh,
             family = tweedie(link = "log"),  
             spatial = "on", # spatial covariance with depth
             control = sdmTMBcontrol(newton_loops = 1), 
             silent = FALSE)

sanity(m3_fall_tw) 
tidy(m3_fall_tw)
tidy(m3_fall_tw, effects = "ran_pars")

#### M4 ####
m4_fall_tw <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1,
                  data = scup_fall, 
                  mesh = fall_mesh,
                  family = tweedie(link = "log"),  
                  spatial = "on", 
                  control = sdmTMBcontrol(newton_loops = 1), 
                  silent = FALSE)

sanity(m4_fall_tw) 
tidy(m4_fall_tw)
tidy(m4_fall_tw, effects = "ran_pars")


### Spatiotemporal Only Models ####
#logistic regression of scup biomass in tows as a function of AVGDEPTH with spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 

#### M5 ####
m5_fall_tw <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
             data = scup_fall,
             mesh = fall_mesh,
             family = tweedie(link = "log"), 
             spatial = "off", 
             time = "EST_YEAR",
             spatiotemporal = "IID", 
             silent = FALSE)

sanity(m5_fall_tw)
tidy(m5_fall_tw)
tidy(m5_fall_tw, effects = "ran_pars")

#### M6 ####
m6_fall_tw <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1, 
                     data = scup_fall,
                     mesh = fall_mesh,
                     family = tweedie(link = "log"), 
                     spatial = "off", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     silent = FALSE)

sanity(m6_fall_tw)
tidy(m6_fall_tw)
tidy(m6_fall_tw, effects = "ran_pars")

### Spatiotemporal + Spatial Models ####

#### M7 ####
#logistic regression of scup biomass in tows as a function of AVGDEPTH with spatial random effects and spatiotemporal random fields estimated by EST_YEAR and with a separate intercept for each. 
m7_fall_tw <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1, 
                     data = scup_fall,
                     mesh = fall_mesh,
                     family = tweedie(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     silent = FALSE)

sanity(m7_fall_tw)
tidy(m7_fall_tw)
tidy(m7_fall_tw, effects = "ran_pars")



#### M8 #### 
m8_fall_tw <- sdmTMB(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1, 
                     data = scup_fall,
                     mesh = fall_mesh,
                     family = tweedie(link = "log"), 
                     spatial = "on", 
                     time = "EST_YEAR",
                     spatiotemporal = "IID", 
                     silent = FALSE)

sanity(m8_fall_tw)
tidy(m8_fall_tw)
tidy(m8_fall_tw, effects = "ran_pars")


AIC(m1_fall_tw,m2_fall_tw,m3_fall_tw,m4_fall_tw,m5_fall_tw,m6_fall_tw,m7_fall_tw,m8_fall_tw)

### save the data
saveRDS(m1_fall_tw, file = here("sdmtmb", "scup", "data", "mods", "m1_fall_tw.rds"))
saveRDS(m2_fall_tw, file = here("sdmtmb", "scup", "data", "mods", "m2_fall_tw.rds"))
saveRDS(m3_fall_tw, file = here("sdmtmb", "scup", "data", "mods", "m3_fall_tw.rds"))
saveRDS(m4_fall_tw, file = here("sdmtmb", "scup", "data", "mods", "m4_fall_tw.rds"))
saveRDS(m5_fall_tw, file = here("sdmtmb", "scup", "data", "mods", "m5_fall_tw.rds"))
saveRDS(m6_fall_tw, file = here("sdmtmb", "scup", "data", "mods", "m6_fall_tw.rds"))
saveRDS(m7_fall_tw, file = here("sdmtmb", "scup", "data", "mods", "m7_fall_tw.rds"))
saveRDS(m8_fall_tw, file = here("sdmtmb", "scup", "data", "mods", "m8_fall_tw.rds"))



scup_fall$resids <- residuals(m7_fall_tw) # randomized quantile residuals
qqnorm(scup_fall$resids)
qqline(scup_fall$resids)

ggplot(scup_fall, aes(X, Y, col = resids)) +
  scale_colour_gradient2() +
  geom_point() +
  facet_wrap(~EST_YEAR) +
  coord_fixed()




pred_fall <- predict(m7_fall_tw, se_fit = TRUE, re_form = NA)
ggplot(pred_fall, aes(AVGDEPTH, exp(est), 
              ymin = exp(est - 1.96 * est_se), 
              ymax = exp(est + 1.96 * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4) + facet_wrap(~YEAR)


library(ggeffects)
ggpredict(m7_fall_tw,  "AVGDEPTH [all]") %>% plot()
ggeffects::ggpredict(m7_fall_tw, "AVGDEPTH [all") |> 
  plot()
