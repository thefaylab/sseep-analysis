##EXAMPLE
## OBJECTIVE ####
# prepare data for sdmTMB model fits and predicting


## LOAD PACKAGES ####
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(patchwork)
library(ggplot2)
library(gridExtra)
library(stringr)
library(readr)  
library(dplyr)
# library(marmap)
# library(raster)

here()

## LOAD DATA ####
sseep.sim.dir <- "C:/Users/croman1/Desktop/UMassD/sseep-sim"
mods.data.dir <- file.path(sseep.sim.dir, "data", "rds", "surv-prods", "mods_data","scup")

# Example: Load population 1, sim 1
pop <- 1
sim <- 1

file_name <- sprintf("precl_pop%03d_sim%02d.rds", pop, sim)
file_path <- file.path(mods.data.dir, file_name)

# Read the file
dat_1_1 <- readRDS(file_path)

strata <- readRDS(here("data", "rds", "active_strata.rds")) %>% 
  select(STRATUM, Region)


mab_strata <- c(3450, 1050, 1610, 1090, 3410, 3380, 3020, 3460 ,3050, 3440, 3260, 3350, 8510, 1010,
                1060, 3080, 3230, 3320, 3290, 8500, 1650, 1690, 7520, 1100, 3110, 3140, 3170, 1020,1740,1700,1730,3200, 
                1660,1620,1110,1070,1030,1750,1710,1670,1630,8520,1120,1080,1040,1760,1720,1680,1640,8530)

# sf_fall_strata <- c(3450, 3410, 3380, 3020, 3460 ,3050, 3440, 3260, 3350, 3080, 3230, 3320, 3290, 3110, 3140, 3170, 3200,
#                1610, 1650, 1690, 1730, 1010, 1050, 1090, 1200, 1190,
#                1620, 1660, 1700, 1740, 1020, 1060, 1100, 1130, 1160)




### filter specific strata ####

dat_1_1 <- dat_1_1 |> 
  filter(STRATUM %in% mab_strata,
  AVGDEPTH < 75,
  YEAR > 5) |>
   mutate(AREA = as.factor(AREA_CODE), 
         EST_YEAR = as.factor(YEAR))


#Catch 
ggplot(dat_1_1) +
  geom_point(aes(x = AVGDEPTH, y = N)) +
  labs(x = "Depth (m)", y = "Catch (Number) per tow", subtitle = "Fall")


ggplot(dat_1_1) +
  geom_point(aes(x = AVGDEPTH, y = N)) +
  facet_wrap(~EST_YEAR) +
  labs(x = "Depth (m)", y = "Catch (Number) per tow", subtitle = "Fall")



gam1 <- gam( N~ s(AVGDEPTH) , data = dat_1_1,
           method = "REML")

plot(gam1, all.terms = TRUE)

# save data 
#saveRDS(sat_1_1, here("sdmtmb", "scup", "data", "scup_fall.rds"))


## CONSTRUCT MESH #### 
mesh_1_1 <- make_mesh(dat_1_1, xy_cols = c("X", "Y"), cutoff = 10)
#cutoff defines the minimum allowed distance between points in the units of X and Y (km)

#mesh$mesh$n 
plot(mesh_1_1)

# # save mesh
# saveRDS(dat_1_1, here("sdmtmb", "scup", "model-based-est", "sdat_1_1.rds"))
# saveRDS(mesh_1_1, here("sdmtmb", "scup", "data", "mesh_1_1.rds"))


glimpse(dat_1_1)


#M1 smooth no rree
m1 <- sdmTMB(N ~ s(AVGDEPTH) + EST_YEAR - 1, 
                data = dat_1_1,
                mesh = mesh_1_1,
                family = tweedie(link = "log"), 
                spatial = "off", 
                time = "EST_YEAR",
                control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
                #spatiotemporal = "IID", 
                silent = FALSE)

sanity(m1)
tidy(m1)
tidy(m1, effects = "ran_pars")

m1$resids <- residuals(m1, type = "mle-eb") 
hist(m1$resids)

# qplot of residuals 
qqnorm(m1$resids)
qqline(m1$resids) # add trend line

#m2 smooth spline + rree
m2 <- sdmTMB(N ~ s(AVGDEPTH) + EST_YEAR - 1, 
             data = dat_1_1,
             mesh = mesh_1_1,
             family = tweedie(link = "log"), 
             spatial = "on", 
             time = "EST_YEAR",
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
             spatiotemporal = "IID", 
             silent = FALSE)

sanity(m2)
tidy(m2)
tidy(m2, effects = "ran_pars")

m2$resids <- residuals(m2, type = "mle-eb") 
hist(m1$resids)

# qplot of residuals 
qqnorm(m2$resids)
qqline(m2$resids) # add trend line


#plynomial function depth, no rree
m3 <- sdmTMB(N ~ poly(AVGDEPTH,2) + EST_YEAR - 1, 
            data = dat_1_1,
            mesh = mesh_1_1,
            family = tweedie(link = "log"), 
            spatial = "off", 
            time = "EST_YEAR",
            control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
            #spatiotemporal = "IID", 
            silent = FALSE)

sanity(m3)
tidy(m3)
tidy(m3, effects = "ran_pars")

m3$resids <- residuals(m3, type = "mle-eb") 
hist(m3$resids)

# qplot of residuals 
qqnorm(m3$resids)
qqline(m3$resids) # add trend line



#poly 2, rree
m4 <- sdmTMB(N ~ poly(AVGDEPTH,4) + EST_YEAR - 1, 
             data = dat_1_1,
             mesh = mesh_1_1,
             family = tweedie(link = "log"), 
             spatial = "on", 
             time = "EST_YEAR",
             control = sdmTMBcontrol(newton_loops = 1), #extra optimization to help with convergence
             spatiotemporal = "IID", 
             silent = FALSE)

sanity(m4)
tidy(m4)
tidy(m4, effects = "ran_pars")

m4$resids <- residuals(m4, type = "mle-eb") 
hist(m4$resids)

# qplot of residuals 
qqnorm(m4$resids)
qqline(m4$resids) # add trend line



AIC(m1,m2,m3,m4) 


# library(DHARMa)
# 
# # Simulate predictions (your current version returns a matrix directly)
# sim_vals <- predict(m2, nsim = 500)  # [n_obs x nsim] matrix already
# 
# # Predicted mean response
# fitted_vals <- predict(m2)$est
# 
# # Construct DHARMa residuals
# sim_res <- createDHARMa(
#   simulatedResponse = sim_vals,            # use directly if [nsim, n_obs]
#   observedResponse = dat_1_1$N,
#   fittedPredictedResponse = fitted_vals
# )
# 
# # Plot residuals
# plot(sim_res)
# 

#proceed with model 2

dat_1_1$resids <- residuals(m2, type = "mle-eb") # randomized quantile residuals

ggplot(dat_1_1, aes(X, Y, col = resids)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed()


grid <- readRDS(here("sdmtmb", "survey_grid_all_122024.rds")) |>
  select(X,Y,cell,mean_1,mean_2,STRATUM,AREA_CODE,AREA)|> 
  rename(AVGDEPTH = mean_1)


grid_yrs <- replicate_df(grid, "EST_YEAR", unique(dat_1_1$EST_YEAR))


predictions <- predict(m2, newdata = grid_yrs, return_tmb_object = TRUE)

plot_map <- function(dat, column, width = 10, height = 10) {
  ggplot(dat, aes(X, Y, fill = {{ column }})) +
    geom_tile(width = width, height = height) +
    facet_wrap(~EST_YEAR) +
    coord_equal()
}




plot_map(predictions$data, exp(est)) +
  scale_fill_viridis_c(trans = "sqrt") +
  ggtitle("Prediction (fixed effects + all random effects)")


plot_map(predictions$data, exp(est_non_rf)) +
  ggtitle("Prediction (fixed effects only)") +
  scale_fill_viridis_c(trans = "sqrt")


plot_map(predictions$data, omega_s) +
  ggtitle("Spatial random effects only") +
  scale_fill_gradient2()


plot_map(predictions$data, epsilon_st) +
  ggtitle("Spatiotemporal random effects only") +
  scale_fill_gradient2()


index <- get_index(predictions, area = 86, bias_correct = TRUE)

ggplot(index, aes(x = EST_YEAR, y = est, group = 1)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab("Year") +
  ylab("Biomass estimate")


mutate(index, cv = sqrt(exp(se^2) - 1)) %>% 
  select(-log_est, -se) %>%
  knitr::kable(format = "pandoc", digits = c(0, 0, 0, 0, 2))


##############################################################################
#############################################################################


