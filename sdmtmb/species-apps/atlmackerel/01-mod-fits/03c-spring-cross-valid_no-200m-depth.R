### created: 04/08/2024
### last updated: 05/28/2024

# 03a - CROSS VALIDATE SPRING MODELS: NO DEPTH OUTLIERS ####

## OBJECTIVE ####
# cross validate models fit to historical Atlantic mackerel data  
# runs approx 15 hours

### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(kableExtra)
library(tictoc)
set.seed(120)

here()

atlmack.dat <- here("sdmtmb", "atlmackerel", "data")
tweedie.cvs <- here("sdmtmb", "atlmackerel", "data", "cross-valid", "tweedie", "no-dep-out")
dpg.cvs <- here("sdmtmb", "atlmackerel", "data", "cross-valid", "dpg", "no-dep-out")

### LOAD DATA ####
# Atlantic mackerel data created in `01-prepare-data.R` 
atlmack <- readRDS(here(atlmack.dat, "atlmackerel_no-200-obs.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# spring mesh created in `01-prepare-data.R` 
spring_mesh <- readRDS(here(atlmack.dat, "spring_mesh_no-200-obs.rds"))

# clust <- sample(1:2, size = nrow(atlmack), replace = T, prob = c(0.1, 0.9))
# saveRDS(clust, here(atlmack.dat, "cross-valid", "atlmack-no.200dep-clust.rds"))


## TWEEDIE CROSS VALIDATIONS ####
#### M1 ####
m1.cv <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                    data = atlmack,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE, 
                    # fold_ids = clust,
                    k_folds = 10)

saveRDS(m1.cv, file = here(tweedie.cvs, "m1-cv.rds"))

#### M2 ####
# second order polynomial cross validation with random effects
m2.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                    data = atlmack,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE, 
                    # fold_ids = clust,
                    k_folds = 10)


saveRDS(m2.cv, file = here(tweedie.cvs, "m2-cv.rds"))


#### M3 ####
# third order polynomial cross validation with random effects
m3.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 3) + EST_YEAR - 1,
                    data = atlmack,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE, 
                    # fold_ids = clust,
                    k_folds = 10)


saveRDS(m3.cv, file = here(tweedie.cvs, "m3-cv.rds"))


#### M4 ####
# forth order polynomial cross validation with random effects
m4.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                    data = atlmack,
                    mesh = spring_mesh,
                    family = tweedie(link = "log"), 
                    spatial = "on", 
                    time = "EST_YEAR",
                    spatiotemporal = "IID",
                    control = sdmTMBcontrol(newton_loops = 1), 
                    silent = FALSE, 
                    # fold_ids = clust,
                    k_folds = 10)


saveRDS(m4.cv, file = here(tweedie.cvs, "m4-cv.rds"))

#### M5 ####
# forth order polynomial cross validation with random effects and independent ranges
m5.cv <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                   data = atlmack,
                   mesh = spring_mesh,
                   family = tweedie(link = "log"), 
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID",
                   share_range = FALSE,
                   control = sdmTMBcontrol(newton_loops = 1), 
                   silent = FALSE, 
                   # fold_ids = clust,
                   k_folds = 10)


saveRDS(m5.cv, file = here(tweedie.cvs, "m5-cv.rds"))



## DELTA/POISSON/GAMMA CROSS VALIDATIONS ####
#### M1 ####
m1.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE, 
                        # fold_ids = clust,
                        k_folds = 10)


saveRDS(m1.cv.dpg, file = here(dpg.cvs, "m1-cv.rds"))


#### M2 ####
# second order polynomial cross validation with random effects
m2.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE, 
                        # fold_ids = clust,
                        k_folds = 10)


saveRDS(m2.cv.dpg, file = here(dpg.cvs, "m2-cv.rds"))


#### M3 ####
# third order polynomial cross validation with random effects
m3.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 3) + EST_YEAR - 1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE, 
                        # fold_ids = clust,
                        k_folds = 10)


saveRDS(m3.cv.dpg, file = here(dpg.cvs, "m3-cv.rds"))


#### M4 ####
# forth order polynomial cross validation with random effects
m4.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                        data = atlmack,
                        mesh = spring_mesh,
                        family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                        spatial = "on", 
                        time = "EST_YEAR",
                        spatiotemporal = "IID",
                        control = sdmTMBcontrol(newton_loops = 1), 
                        silent = FALSE, 
                        # fold_ids = clust,
                        k_folds = 10)


saveRDS(m4.cv.dpg, file = here(dpg.cvs, "m4-cv.rds"))

#### M5 ####
# forth order polynomial cross validation with random effects and independent ranges
m5.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       share_range = list(FALSE, FALSE),
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       k_folds = 10)

saveRDS(m5.cv.dpg, file = here(dpg.cvs, "m5-cv.rds"))

#### M6 ####
# forth order polynomial cross validation with random effects and independent ranges in model 2
m6.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       share_range = list(TRUE, FALSE),
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       k_folds = 10)


saveRDS(m6.cv.dpg, file = here(dpg.cvs, "m6-cv.rds"))

#### M7 ####
# forth order polynomial cross validation with random effects and independent ranges in model 1
m7.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                       spatial = "on", 
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       share_range = list(FALSE, TRUE),
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       k_folds = 10)

saveRDS(m7.cv.dpg, file = here(dpg.cvs, "m7-cv.rds"))

#### M8 ####
# forth order polynomial cross validation with random effects and independent ranges in model 1, but no spatial random effects in model 2
m8.cv.dpg <- sdmTMB_cv(EXPCATCHWT ~ poly(AVGDEPTH, 4) + EST_YEAR - 1,
                       data = atlmack,
                       mesh = spring_mesh,
                       family = delta_gamma(link1 = "log", link2 = "log", type = "poisson-link"), 
                       spatial = list("on", "off"),
                       time = "EST_YEAR",
                       spatiotemporal = "IID",
                       share_range = list(FALSE, TRUE),
                       control = sdmTMBcontrol(newton_loops = 1), 
                       silent = FALSE, 
                       k_folds = 10)

saveRDS(m8.cv.dpg, file = here(dpg.cvs, "m8-cv.rds"))


