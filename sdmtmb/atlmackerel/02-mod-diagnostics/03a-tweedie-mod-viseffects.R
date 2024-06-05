### created: 12/13/2023 
### updated: 

# 04a - VISUALIZE MARGINAL AND CONDITIONAL EFFECTS: TWEEDIE MODELS ####

## OBJECTIVE ####
# visualize marginal and conditional effects of predictor variables on the response variable from Atlantic mackerel model fit.

### LOAD PACKAGES #### 
library(ggeffects)
library(sdmTMB)

tweedie.mods <- here("sdmtmb", "atlmackerel", "data", "mods", "tweedie")
dpg.mods <- here("sdmtmb", "atlmackerel", "data", "mods", "dpg", "no-dep-out")
tweedie.plots <-  here("sdmtmb", "atlmackerel", "plots", "marg-effects", "tweedie")

### LOAD DATA ####
# tm13_all.dat <- readRDS(here(tweedie.mods, "all-dat", "m13_spring.rds"))
# tm14_all.dat <- readRDS(here(tweedie.mods, "all-dat", "m14_spring.rds"))
# 
# tm13_no.bio <- readRDS(here(tweedie.mods, "no-bio-out", "m13_spring.rds"))
# tm14_no.bio <- readRDS(here(tweedie.mods, "no-bio-out", "m14_spring.rds"))

tm4_no.dep <- readRDS(here(tweedie.mods, "no-dep-out", "m4_spring.rds"))
dpg4_no.dep <- readRDS(here(dpg.mods, "m4_spring.rds"))
# tweedie_m12.no_out <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m12_spring_no.out.rds"))

# am_spring_no.out <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring_no-outliers.rds")) |> 
#   mutate(EST_YEAR = as.factor(EST_YEAR), 
#          AREA = as.factor(AREA))
# am_spring <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring.rds")) |>
#   mutate(EST_YEAR = as.factor(EST_YEAR),
#          AREA = as.factor(AREA))

atlmack.dat <- here("sdmtmb", "atlmackerel", "data")
# tweedie.mods <- here("sdmtmb", "atlmackerel", "data", "mods", "tweedie", "no-dep-out")
# 

### LOAD DATA ####
# Atlantic mackerel data created in `01-prepare-data.R` 
am_spring <- readRDS(here(atlmack.dat, "atlmackerel_no-200-obs.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# CONDITIONAL EFFECTS 
# visreg(tweedie_m11, xvar = "AVGDEPTH", gg = TRUE)
# #visreg(tweedie_m11, xvar = "AVGDEPTH", model = 2, gg = TRUE)
# visreg(tweedie_m11.no_out, xvar = "AVGDEPTH", gg = TRUE)
#visreg(tweedie_m11.no_out, xvar = "AVGDEPTH", model = 2, gg = TRUE)

## MARGINAL EFFECTS ####
#  plot marginal effects of given predictor variables
# calculates marginal effects with the effects package using the parameter covariance matrix.
# does not work with smoothers
# effects are “marginalized” or “averaged” over the other fixed effects


### DEPTH ####

g1_tm <- ggeffect(tm4_no.dep, "AVGDEPTH [all]")
g1_dpg <- ggpredict(dpg4_no.dep, "AVGDEPTH [all]")
# saveRDS(g1_tm12, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m12-tweedie-preds.rds"))
# g1_tm12_no.out <- ggeffect(tweedie_m12.no_out, "AVGDEPTH [all]")
# saveRDS(g1_tm12_no.out, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m12-tweedie-preds-no.out.rds"))

#g1_dpgm12 <- ggeffect(dpg_m12, "AVGDEPTH [0:400 by=1]")
#g1_dpgm12_no.out <- ggeffect(dpg_m12, "AVGDEPTH [0:400 by=1]")


plot(g1_tm) + labs(x = "Average Depth", y = "Biomass (kg)", title="Predicted values of biomass", subtitle="Marginal effects of a fourth-order polynomial relationship with depth on Atlantic mackerel biomass predictions \nusing a tweedie distribution")
ggsave("tweedie-mod_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 8, height = 4, plot = last_plot())

# plot(g1_tm12_no.out) + labs(x = "Average Depth", y = "Biomass (kg)", title="Predicted values of biomass", subtitle="Marginal effects of a parabolic relationship with depth on Atlantic mackerel biomass predictions \nwhen outliers are removed and using a tweedie distribution")
# ggsave("atlmack_tweedie-m12no-out_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 8, height = 4, plot = last_plot())

### YEAR ####
g2_tm <- ggeffect(tm4_no.dep, "EST_YEAR")
plot(g2_tm) + labs(x = "Year", y = "Biomass (kg)", title="Predicted values of biomass", subtitle="Marginal effects of independent years on Atlantic mackerel biomass predictions \nusing a tweedie distribution")
ggsave("tweedie-mod_year-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 6, height = 4, plot = last_plot())

### AREA ####
# g3 <- ggeffect(spring_mod, "AREA")
# plot(g3) + labs(x = "Area", y = "Biomass (kg)", title="Predicted values of biomass")
# ggsave("area-marg-effects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 6, height = 4, plot = last_plot())

### DEPTH & YEAR ####
g4 <- ggeffect(tm4_no.dep, terms = c("AVGDEPTH [all]", "EST_YEAR"))
#plot(g4, facet = TRUE)

ggplot(g4, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Predicted Biomass (kg)", color = "Year")
ggsave("depth_yr-marg-effects1.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 6, height = 4, plot = last_plot())

ggplot(g4, aes(x, predicted, colour = group)) +
  geom_line() + 
  labs(x = "Average Depth", y = "Predicted Biomass (kg)", color = "Year") + 
  facet_wrap(~group)
ggsave("depth_yr-marg-effects2.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 6, height = 4, plot = last_plot())

### DEPTH & AREA ####
# g5 <- ggeffect(spring_mod, terms = c("AVGDEPTH [0:400 by=1]", "AREA"))
# plot(g5, facet = TRUE) + 
#   labs(x = "Average Depth", y = "Biomass (kg)", title = "Predicted values of biomass")
# ggsave("depth_area-marg-effects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 6, height = 4, plot = last_plot())

### YEAR & AREA ####
# g6 <- ggeffect(spring_mod, terms = c("EST_YEAR", "AREA"))
# plot(g6, facet = TRUE) + 
#   labs(x = "Year", y = "Biomass (kg)", title = "Predicted values of biomass")
# ggsave("year_area-marg-effects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 8, height = 4, plot = last_plot())