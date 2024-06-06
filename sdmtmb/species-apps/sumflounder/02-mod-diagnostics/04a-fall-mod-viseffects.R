### created: 11/28/2023 
### updated: 04/17/2024

# 04 - VISUALIZE MARGINAL AND CONDITIONAL EFFECTS ####

## OBJECTIVE ####
# visualize marginal and conditional effects of predictor variables on the response variable from Atlantic mackerel model fit.


### LOAD PACKAGES ####
library(sdmTMB)
library(visreg)
#install.packages("ggeffects")
library(ggeffects)
library(ggplot2)
theme_set(theme_bw())

cond.dat <- here("sdmtmb", "sumflounder", "data", "cond-effects")
cond.plots <- here("sdmtmb", "sumflounder", "plots", "fall-cond-effects")
marg.dat <- here("sdmtmb", "sumflounder", "data", "marg-effects")
marg.plots <- here("sdmtmb", "sumflounder", "plots", "fall-marg-effects")


### DATA #### 
# load best fitting m12 model created in 01-mod-fits.R and resaved as modsafter diagnostics 
mods<- readRDS(here("sdmtmb", "sumflounder", "data", "mods", "fall", "m10_fall.rds"))


## CONDITIONAL EFFECTS ####
# visualize the fit of the fall model with visreg
# conditional on the other predictors being set to certain values.

### DEPTH ####
# effect of depth in link space and the change in biomass on the y-axis; all other variables held constant 
# includes randomized quantile residuals
depth_reg <- visreg(mod, xvar = "AVGDEPTH", plot = FALSE)
depth_plot <- ggplot(depth_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) + 
  labs(x = "Average Depth", y = "Change in biomass", subtitle = "Conditional effect of average depth on fall summer flounder biomass")
  
depth_plot + geom_point(aes(y = visregRes), data = depth_reg$res, size = 1, alpha = 0.4)

ggsave("fall_depth-cond-effects.png", device = "png", here(cond.plots), width = 6, height = 4, plot = last_plot())
saveRDS(depth_reg, here(cond.dat, "fall_depth-cond-effects.rds"))

#### IN RESPONSE SPACE ####
depth_resp_plot <- visreg(mod, xvar = "AVGDEPTH", scale = "response", gg = TRUE)
saveRDS(depth_resp_plot, here(cond.dat, "fall_depth-resp_cond-effects.rds"))

depth_resp_plot + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = "Conditional effect of average depth on fall summer flounder biomass")

ggsave("fall_depth-resp_cond-effects.png", device = "png", here(cond.plots), width = 8, height = 4, plot = last_plot())


#### & YEAR ####
depth_yr_reg <- visreg(mod, xvar = "AVGDEPTH", by = "EST_YEAR", plot = FALSE)

depth_year_plot <- ggplot(depth_yr_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) + 
  labs(x = "Average Depth", y = "Change in biomass", subtitle = "Conditional effect of average depth on annual fall summer flounder biomass") +
  facet_wrap(~EST_YEAR)

depth_year_plot + geom_point(aes(y = visregRes), data = depth_yr_reg$res, size = 1, alpha = 0.4) 

ggsave("fall_depth-yr_cond-effects.png", device = "png", here(cond.plots), width = 8, height = 6, plot = last_plot())
saveRDS(depth_yr_reg, here(cond.dat, "fall_depth-year_cond-effects.rds"))

##### IN RESPONSE SPACE ####
depth_yr_resp <- visreg(mod, xvar = "AVGDEPTH", by = "EST_YEAR", scale = "response", gg = TRUE)
depth_resp_plot + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = "Conditional effect of average depth on annual fall summer flounder biomass")

ggsave("fall_dep-yr_resp-cond-effects.png", device = "png", here(cond.plots), width = 8, height = 4, plot = last_plot())
saveRDS(depth_yr_resp, here(cond.dat, "fall_depth-year_resp-cond-effects.rds"))

##### 2D PLOT ####
dep.yr_2dplot <- visreg2d(mod, xvar = "EST_YEAR", yvar = "AVGDEPTH", plot.type = "gg")
dep.yr_2dplot + labs(x = "Year", y = "Depth (m)", fill = "Change in depth")
ggsave("fall_dep-yr_cond-effects2.png", device = "png", here(cond.plots), width = 6, height = 4, plot = last_plot())
# saveRDS(depth_yr_2dplot, here("sdmtmb", "sumflounder", "data", "depth-year_2Deffects-plot.rds"))

#### & AREA ####
depth_area_reg <- visreg(mod, xvar = "AVGDEPTH", by = "AREA", plot = FALSE)
depth_area_plot <- ggplot(depth_area_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) + 
  labs(x = "Average Depth", y = "Change in biomass", subtitle = "Conditional effect of average depth on fall summer flounder biomass by area") +
  facet_wrap(~AREA)
depth_area_plot + geom_point(aes(y = visregRes), data = depth_area_reg$res, size = 1, alpha = 0.4) 

ggsave("fall_dep_area-cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "fall-cond-effects"), width = 6, height = 4, plot = last_plot())
saveRDS(depth_area_reg, here("sdmtmb", "sumflounder", "data", "fall-depth-area_condeffects-dat.rds"))

##### IN RESPONSE SPACE ####
depth_area_resp <- visreg(mod, xvar = "AVGDEPTH", by = "AREA", scale = "response", gg = TRUE)

depth_area_resp + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = "Conditional effect of average depth on fall summer flounder biomass by area")

ggsave("fall-dep_area-response-cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "fall-cond-effects"), width = 8, height = 4, plot = last_plot())
saveRDS(depth_area_resp, here("sdmtmb", "sumflounder", "data", "fall-depth-area-resp_condeffects-dat.rds"))

# 2D plot
depth_area2d <- visreg2d(mod, xvar = "AREA", yvar = "AVGDEPTH", plot.type = "gg")
depth_area2d + labs(x= "Area", y = "Average Depth", subtitle = "Conditional effect of average depth and area on changes in fall summer flounder biomass") 

ggsave("fall-dep_area-cond-effects2d.png", device = "png", here("sdmtmb", "sumflounder", "plots", "fall-cond-effects"), width = 6, height = 4, plot = last_plot())
saveRDS(depth_area2d, here("sdmtmb", "sumflounder", "data", "fall-depth-area_2Deffects-plot.rds"))

### YEAR ####
#year_reg <- visreg(mod, xvar = "EST_YEAR", plot = FALSE)

# year_plot <- ggplot(year_reg$fit, aes(x = EST_YEAR, y = visregFit)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) + 
#   labs(x = "Year", y = "f(Year)")
# 
# year_plot + geom_point(aes(y = visregRes), data = year_reg$res, size = 1, alpha = 0.4)

year_eff <- visreg(mod, xvar = "EST_YEAR", gg = TRUE)
year_eff + labs(x= "Year", y = "Change in biomass in link space", subtitle = "Conditional effect of year on changes in fall summer flounder biomass") 

ggsave("fall_year_cond-effects.png", device = "png", here(cond.plots), width = 8, height = 4, plot = last_plot())
saveRDS(year_eff, here(cond.dat, "fall_year_cond-effects.rds"))

#### & AREA ####
year_area <- visreg(mod, xvar = "EST_YEAR", by = "AREA", gg = TRUE)
year_area + labs(x= "Year", y = "Change in biomass in link space", subtitle = "Conditional effect of year on changes in fall summer flounder biomass") 

 ggsave("fall-year-area_cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "fall-cond-effects"), width = 7, height = 4, plot = last_plot())
saveRDS(year_area, here("sdmtmb", "sumflounder", "data", "fall-year-area_condeffects-dat.rds"))

### AREA ####
area_plot <- visreg(mod, xvar = "AREA", gg = TRUE)
area_plot + labs(x= "Year", y = "Change in biomass in link space", subtitle = "Conditional effect of year on changes in fall summer flounder biomass") 

  # area_plot <- ggplot(area_plot$fit, aes(x = AREA, y = visregFit)) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) +
  #   labs(x = "Area", y = "f(Area)")

# area_plot + geom_point(aes(y = visregRes), data = area_reg$res, size = 1, alpha = 0.4)

ggsave("fall-area-cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "fall-cond-effects"), width = 7, height = 4, plot = last_plot())
saveRDS(area_plot, here("sdmtmb", "sumflounder", "data", "fall-area_condeffects-dat.rds"))


## MARGINAL EFFECTS ####
#  plot marginal effects of given predictor variables
# calculates marginal effects with the effects package using the parameter covariance matrix.
# does not work with smoothers
# effects are “marginalized” or “averaged” over the other fixed effects

### DEPTH ####
g <- ggpredict(mod, "AVGDEPTH [all]") |> plot()
plot(g) + labs(x = "Average Depth", y = "Biomass (kg)", title="Predicted values of fall summer flounder biomass")
saveRDS(g, here(marg.dat, "fall_depth_marg-eff.rds"))
ggsave("fall_depth_marg-effects.png", device = "png", here(marg.plots), width = 8, height = 5, plot = last_plot())

### YEAR ####
g2 <- ggpredict(mod, "EST_YEAR [all]") |> plot()
saveRDS(g2, here(marg.dat, "fall_year_marg-eff.rds"))

plot(g2) + labs(x = "Year", y = "Biomass (kg)", title="Predicted values of fall summer flounder biomass")
ggsave("fall_year_marg-effects.png", device = "png", here(marg.plots), width = 8, height = 5, plot = last_plot())


### AREA ####
g3 <- ggpredict(mod, "AREA [all]") |> plot()
saveRDS(g3, here(marg.dat, "fall_area_marg-eff.rds"))

plot(g3) + labs(x = "Area", y = "Biomass (kg)", title="Predicted values of fall summer flounder biomass")
ggsave("fall_area_marg-effects.png", device = "png", here(marg.plots), width = 6, height = 4, plot = last_plot())

### DEPTH & YEAR ####
g4 <- ggpredict(mod, terms = c("AVGDEPTH [all]", "EST_YEAR [all]")) #|> plot()
saveRDS(g4, here(marg.dat, "fall_m10_depth-yr_marg-eff.rds"))
#plot(g4, facet = TRUE) + scale_color_viridis_c()

ggplot(g4, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", color = "Year", subtitle = "Predicted values of fall summer flounder biomass") + xlim(0, NA)
ggsave("fall_dep-yr_marg-effects1.png", device = "png", here(marg.plots), width = 7, height = 5, plot = last_plot())

ggplot(g4, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", color = "Year", subtitle = "Predicted values of fall summer flounder biomass") + xlim(0, NA) +
  facet_wrap(~group)
ggsave("fall_dep-yr_marg-effects2.png", device = "png", here(marg.plots), width = 6, height = 4, plot = last_plot())

### DEPTH & AREA ####
g5 <- ggpredict(mod, terms = c("AVGDEPTH [all]", "AREA [all]"))
saveRDS(g5, here(marg.dat, "fall_dep-area_marg-eff-dat.rds"))

plot(g5, facet = TRUE) + 
  labs(x = "Average Depth", y = "Biomass (kg)", title = "Predicted values of fall summer flounder biomass")
ggsave("fall_depth-area_marg-effects.png", device = "png", here(marg.plots), width = 6, height = 4, plot = last_plot())

### YEAR & AREA ####
g6 <- ggpredict(mod, terms = c("EST_YEAR [all]", "AREA [all]"))
saveRDS(g6, here(marg.dat, "fall_area-yr_marg-eff-dat.rds"))

plot(g6, facet = TRUE) + 
  labs(x = "Year", y = "Biomass (kg)", title = "Predicted values of fall summer flounder biomass")
ggsave("fall_year-area_marg-effects.png", device = "png", here(marg.plots), width = 10, height = 5, plot = last_plot())

