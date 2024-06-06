### created: 11/28/2023 
### updated: 

# 04 - VISUALIZE MARGINAL AND CONDITIONAL EFFECTS ####

## OBJECTIVE ####
# visualize marginal and conditional effects of predictor variables on the response variable from summer flounder model fit.


### LOAD PACKAGES ####
library(sdmTMB)
library(visreg)
install.packages("ggeffects")
library(ggeffects)
theme_set(theme_bw())

### DATA #### 
# load best fitting m12 model created in 01-mod-fits.R and resaved as spring_mod after diagnostics 
spring_mod <- readRDS(here("sdmtmb", "sumflounder", "data", "mods", "m12_spring.rds"))


## CONDITIONAL EFFECTS ####
# visualize the fit of the spring model with visreg
# conditional on the other predictors being set to certain values.

### DEPTH ####
# effect of depth in link space and the change in biomass on the y-axis; all other variables held constant 
# includes randomized quantile residuals
depth_reg <- visreg(spring_mod, xvar = "AVGDEPTH", plot = FALSE)
depth_plot <- ggplot(depth_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) + 
  labs(x = "Average Depth", y = "Change in biomass in link space", subtitle = "Conditional effects of average depth on changes in spring summer flounder biomass")
  
depth_plot + geom_point(aes(y = visregRes), data = depth_reg$res, size = 1, alpha = 0.4)

ggsave("spring-depth-cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 8, height = 4, plot = last_plot())
saveRDS(depth_reg, here("sdmtmb", "sumflounder", "data", "depth_cond_effects_dat.rds"))

#### IN RESPONSE SPACE ####
depth_resp_plot <- visreg(spring_mod, xvar = "AVGDEPTH", scale = "response", gg = TRUE)
depth_resp_plot + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = "Conditional effects of average depth on changes in spring summer flounder biomass")
ggsave("spring-depth-response-cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 6, height = 4, plot = last_plot())saveRDS(depth_resp_plot, here("sdmtmb", "sumflounder", "data", "spring-depth_response_plot.rds"))

#### & YEAR ####
depth_yr_reg <- visreg(spring_mod, xvar = "AVGDEPTH", by = "EST_YEAR", plot = FALSE)
depth_year_plot <- ggplot(depth_yr_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) + 
  labs(x = "Average Depth", y = "f(Average Depth)") +
  facet_wrap(~EST_YEAR)
depth_year_plot + geom_point(aes(y = visregRes), data = depth_yr_reg$res, size = 1, alpha = 0.4) 

ggsave("spring-dep_yr-cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 6, height = 4, plot = last_plot())
saveRDS(depth_yr_reg, here("sdmtmb", "sumflounder", "data", "spring-depth-year_condeffects-dat.rds"))

##### IN RESPONSE SPACE ####
depth_yr_resp <- visreg(spring_mod, xvar = "AVGDEPTH", by = "EST_YEAR", scale = "response", gg = TRUE)
depth_yr_resp + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = "Conditional effects of average depth on changes in annual spring summer flounder biomass") + facet_wrap(~EST_YEAR, nrow = 2)

ggsave("spring_dep-yr-resp_cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 8, height = 5, plot = last_plot())
saveRDS(depth_yr_reg, here("sdmtmb", "sumflounder", "data", "spring_dep-yr_resp-condeffects-dat.rds"))

##### 2D PLOT ####
dep_yr_2dplot <- visreg2d(spring_mod, xvar = "EST_YEAR", yvar = "AVGDEPTH", plot.type = "gg")
dep_yr_2dplot + labs(x = "Year", y = "Average Depth", subtitle = "Conditional effects of average depth and year on changes in spring summer flounder biomass")
ggsave("spring_dep-yr_cond-effects2.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 7, height = 4, plot = last_plot())
saveRDS(dep_yr_2dplot, here("sdmtmb", "sumflounder", "data", "spring-depth-year_2Deffects-plot.rds"))

#### & AREA ####
depth_area_reg <- visreg(spring_mod, xvar = "AVGDEPTH", by = "AREA", plot = FALSE)
depth_area_plot <- ggplot(depth_area_reg$fit, aes(x = AVGDEPTH, y = visregFit)) +
  geom_line() +
  geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) + 
  labs(x = "Average Depth", y = "Change in biomass in link space", subtitle = "Conditional effects of average depth on changes in spring summer flounder biomass by area") +
  facet_wrap(~AREA)
depth_area_plot + geom_point(aes(y = visregRes), data = depth_area_reg$res, size = 1, alpha = 0.4) 

ggsave("spring-dep_area-cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 7, height = 4, plot = last_plot())
saveRDS(depth_area_reg, here("sdmtmb", "sumflounder", "data", "spring-depth-area_condeffects-dat.rds"))

##### IN RESPONSE SPACE ####
dep_area_resp <- visreg(spring_mod, xvar = "AVGDEPTH", by = "AREA", scale = "response", gg = TRUE)
dep_area_resp + labs(x = "Average Depth", y = "Biomass (kg)", subtitle = "Conditional effects of average depth on spring summer flounder biomass by area")
ggsave("spring_dep-area-response_cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 7, height = 4, plot = last_plot())
saveRDS(dep_area_resp, here("sdmtmb", "sumflounder", "data", "spring-depth-area-resp_condeffects-dat.rds"))

# 2D plot
dep_area_2dplot <- visreg2d(spring_mod, xvar = "AREA", yvar = "AVGDEPTH", plot.type = "gg")
dep_area_2dplot + labs(x = "Year", y = "Average Depth", subtitle = "Conditional effects of average depth and year on changes in spring summer flounder biomass")

ggsave("spring-dep_area_condeffects2.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 8, height = 4, plot = last_plot())
saveRDS(dep_area_2dplot, here("sdmtmb", "sumflounder", "data", "spring_depth-area_2Deffects-plot.rds"))

### YEAR ####
#year_reg <- visreg(spring_mod, xvar = "EST_YEAR", plot = FALSE)

# year_plot <- ggplot(year_reg$fit, aes(x = EST_YEAR, y = visregFit)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = visregLwr, ymax = visregUpr), alpha = 0.5) + 
#   labs(x = "Year", y = "f(Year)")
# 
# year_plot + geom_point(aes(y = visregRes), data = year_reg$res, size = 1, alpha = 0.4)

year_plot <- visreg(spring_mod, xvar = "EST_YEAR", gg = TRUE)

year_plot + labs(x = "Year", y = "Change in biomass in link space", subtitle = "Conditional effects of year on changes in annual spring summer flounder biomass")


ggsave("spring-year-cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 7, height = 4, plot = last_plot())
saveRDS(year_plot, here("sdmtmb", "sumflounder", "data", "spring_year_condeffects-dat.rds"))

#### & AREA ####
area_year_plot <- visreg(spring_mod, xvar = "EST_YEAR", by = "AREA", gg = TRUE)

area_year_plot + labs(x = "Year", y = "Change in biomass in link space", subtitle = "Conditional effects of year on changes in annual spring summer flounder biomass")

ggsave("spring_year-area_cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 8, height = 5, plot = last_plot())
saveRDS(area_year_plot, here("sdmtmb", "sumflounder", "data", "spring_year-area_condeffects-dat.rds"))

### AREA ####
area_plot <- visreg(spring_mod, xvar = "AREA", gg = TRUE)

area_plot + labs(x = "Area", y = "Change in biomass in link space", subtitle = "Conditional effects of area on changes in spring summer flounder biomass")

ggsave("spring_area_cond-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-cond-effects"), width = 6, height = 4, plot = last_plot())
saveRDS(area_plot, here("sdmtmb", "sumflounder", "data", "spring_area_condeffects-dat.rds"))


## MARGINAL EFFECTS ####
#  plot marginal effects of given predictor variables
# calculates marginal effects with the effects package using the parameter covariance matrix.
# does not work with smoothers
# effects are “marginalized” or “averaged” over the other fixed effects

### DEPTH ####
g <- ggpredict(spring_mod, "AVGDEPTH [all]") |> plot()
plot(g) + labs(x = "Average Depth", y = "Biomass (kg)", title="Predicted values of spring summer flounder biomass")
ggsave("sf-spring_depth_marg-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-marg-effects"), width = 6, height = 5, plot = last_plot())
saveRDS(g, here("sdmtmb", "sumflounder", "data", "spring_depth_margeffects-dat.rds"))

### YEAR ####
g2 <- ggpredict(spring_mod, "EST_YEAR [all]") |> plot()
plot(g2) + labs(x = "Year", y = "Biomass (kg)", title="Predicted values of spring summer flounder biomass")
ggsave("sf-spring_year_marg-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-marg-effects"), width = 7, height = 4, plot = last_plot())
saveRDS(g2, here("sdmtmb", "sumflounder", "data", "spring_year_margeffects-dat.rds"))

### AREA ####
g3 <- ggpredict(spring_mod, "AREA [all]") |> plot()
plot(g3) + labs(x = "Area", y = "Biomass (kg)", title="Predicted values of spring summer flounder biomass")
ggsave("sf-spring_area_marg-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-marg-effects"), width = 6, height = 4, plot = last_plot())
saveRDS(g3, here("sdmtmb", "sumflounder", "data", "spring_area_margeffects-dat.rds"))

### DEPTH & YEAR ####
g4 <- ggpredict(spring_mod, terms = c("AVGDEPTH [all]", "EST_YEAR [all]")) 
saveRDS(g4, here("sdmtmb", "sumflounder", "data", "spring_dep-yr_margeffects-dat.rds"))
#plot(g4, facet = TRUE)

ggplot(g4, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", color = "Year", subtitle = "Predicted values of spring summer flounder biomass")
ggsave("sf-spring_depth-yr_marg-effects1.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-marg-effects"), width = 6, height = 4, plot = last_plot())

ggplot(g4, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Biomass (kg)", color = "Year", subtitle = "Predicted values of spring summer flounder biomass") + 
  facet_wrap(~group)
ggsave("sf-spring_depth-yr_marg-effects2.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-marg-effects"), width = 6, height = 4, plot = last_plot())

### DEPTH & AREA ####
g5 <- ggpredict(spring_mod, terms = c("AVGDEPTH [all]", "AREA [all]")) #|>
plot(g5, facet = TRUE) + 
  labs(x = "Average Depth", y = "Biomass (kg)", title = "Predicted values of spring summer flounder biomass")
ggsave("sf-spring_depth-area_marg-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-marg-effects"), width = 7, height = 5, plot = last_plot())
saveRDS(g5, here("sdmtmb", "sumflounder", "data", "spring_dep-area_margeffects-dat.rds"))

### YEAR & AREA ####
g6 <- ggpredict(spring_mod, terms = c("EST_YEAR [all]", "AREA [all]")) #|> plot()
plot(g6, facet = TRUE) + 
  labs(x = "Year", y = "Biomass (kg)", title = "Predicted values of spring summer flounder biomass")
ggsave("sf-spring_year-area_marg-effects.png", device = "png", here("sdmtmb", "sumflounder", "plots", "spring-marg-effects"), width = 10, height = 4, plot = last_plot())
saveRDS(g6, here("sdmtmb", "sumflounder", "data", "spring_year-area_margeffects-dat.rds"))

