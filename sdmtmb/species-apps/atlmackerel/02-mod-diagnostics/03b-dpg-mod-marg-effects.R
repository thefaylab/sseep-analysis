### created: 12/19/2023 
### updated: 

# 04b - VISUALIZE MARGINAL AND CONDITIONAL EFFECTS: DELTA POISSON MODELS  ####

## OBJECTIVE ####
# visualize marginal and conditional effects of predictor variables on the response variable from Atlantic mackerel models fit using a spline on depth or a delta poisson gamma distribution family

### LOAD PACKAGES ####
library(ggeffects)
library(sdmTMB)
theme_set(theme_bw())


### LOAD DATA #####
# model fitting data with outliers of bionass removed
am_spring_no.out <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring_no-outliers.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# model fitting data 
am_spring <- readRDS(here("sdmtmb", "atlmackerel", "data", "atlmackerel_spring.rds")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), 
         AREA = as.factor(AREA))

# tweedie models
tweedie_m11.no_out <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m11_spring_no.out.rds"))
tweedie_m11 <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m11_spring.rds"))

# delta models 
dpg_m11 <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m11_spring_dpg.rds"))
dpg_m11.no_out <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m11_spring_no.out.dpg.rds"))
dpg_m12 <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m12_spring_dpg.rds"))
dpg_m12.no_out <- readRDS(here("sdmtmb", "atlmackerel", "data", "mods", "m12_spring_no.out.dpg.rds"))

# create new data frame for predictions
nd <- data.frame(AVGDEPTH = seq(
  min(am_spring$AVGDEPTH), 
  max(am_spring$AVGDEPTH), length.out = 100))

nd <- replicate_df(nd, "EST_YEAR", c(2009:2019, 2021)) |> 
  replicate_df("AREA", c("OUTSIDE", "WIND")) |> 
  mutate(EST_YEAR = as.factor(EST_YEAR), AREA = as.factor(AREA))
saveRDS(nd, here("sdmtmb", "atlmackerel", "data", "marg-effects", "new-data.rds"))


# TWEEDIE MODELS ####
## m11 ####
### with outliers ####
# make predictions
p_tm11 <- predict(tweedie_m11, newdata = nd, se_fit = TRUE, re_form = NA, model = NA)
saveRDS(p_tm11, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11-tweedie-preds.rds"))

# plot predictions
ggplot(p_tm11 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                                              ymin = exp(est - 2 * est_se), 
                                              ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y") + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a smoother on depth in 2021 on Atlantic mackerel biomass predictions \nusing a tweedie distribution")
ggsave("atlmack_tweedie-m11_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 8, height = 5, plot = last_plot())

ggplot(p_tm11, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)
ggsave("atlmack_tweedie-m11_dep-yr-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 8, height = 5, plot = last_plot())


### without outliers #### 
# make predictions
p_tm11_no.out <- predict(tweedie_m11.no_out, newdata = nd, se_fit = TRUE, re_form = NA, model = NA)
saveRDS(p_tm11_no.out, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11-tweedie-preds-no.out.rds"))

# plot predictions
ggplot(p_tm11_no.out |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                                                     ymin = exp(est - 2 * est_se), 
                                                     ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y") + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a smoother on depth in 2021 on Atlantic mackerel biomass predictions when \noutliers are removed from model fit and using a tweedie distribution")
ggsave("atlmack_tweedie-m11no-out_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 8, height = 5, plot = last_plot())

ggplot(p_tm11_no.out, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)
ggsave("atlmack_tweedie-m11no-out_dep-yr-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 8, height = 5, plot = last_plot())

 
## DELTA MODELS ####
## m11 ####
### with outliers ####
#### combined model ####
p_m11 <- predict(dpg_m11, newdata = nd, se_fit = TRUE, re_form = NA, model = NA)
saveRDS(p_m11, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11-dpg-preds.rds"))


ggplot(p_m11 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, xp(est), 
              ymin = exp(est - 2 * est_se), 
              ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y") + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a smoother on depth in 2021 on Atlantic mackerel biomass predictions \nusing the combined models of a delta poisson gamma distribution")
ggsave("atlmack_dpg-m11_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m11, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)
ggsave("atlmack_dpg-m11_dep-yr-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

# test if predicting at same loctions gives you same shape 
# test <- predict(dpg_m11, se_fit = TRUE, re_form = NA, model = NA)
# ggplot(test |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est),
#                                              ymin = exp(est - 2 * est_se),
#                                              ymax = exp(est + 2 * est_se))) +
#   geom_line() +
#   geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y")
# 
# # same test but with outliers removed 
# test2 <- predict(dpg_m11.no_out, se_fit = TRUE, re_form = NA, model = NA)
# ggplot(test2 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est),
#                                              ymin = exp(est - 2 * est_se),
#                                              ymax = exp(est + 2 * est_se))) +
#   geom_line() +
#   geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y")

#### model 1 ####
p_m11.1 <- predict(dpg_m11, newdata = nd, se_fit = TRUE, re_form = NA, model = 1)
saveRDS(p_m11.1, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11.1-dpg-preds.rds"))


ggplot(p_m11.1 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, plogis(est), 
                  ymin = plogis(est - 2 * est_se), 
                  ymax = plogis(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a smoother on depth in 2021 on Atlantic mackerel biomass predictions \nusing MODEL 1 from the delta poisson gamma distribution") +
  facet_wrap(~AREA, scales = "free_y") #+ facet_grid(cols = vars(AREA), rows = vars(EST_YEAR), scales = "free_y") 
ggsave("atlmack_dpg-m11.1_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m11.1, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)

#### model 2 ####
p_m11.2 <- predict(dpg_m11, newdata = nd, se_fit = TRUE, re_form = NA, model = 2)
saveRDS(p_m11.2, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11.2-dpg-preds.rds"))

ggplot(p_m11.2 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                  ymin = exp(est - 2 * est_se), 
                  ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a smoother on depth in 2021 on Atlantic mackerel biomass predictions \nusing MODEL 2 from the delta poisson gamma distribution") +
  facet_wrap(~AREA, scales = "free_y")
ggsave("atlmack_dpg-m11.2_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m11.2, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)



### without outliers ####
#### combined ####
p_m11_no.out <- predict(dpg_m11.no_out, newdata = nd, se_fit = TRUE, re_form = NA, model = NA)
saveRDS(p_m11_no.out, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11-dpg-preds-no.out.rds"))

ggplot(p_m11_no.out |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                  ymin = exp(est - 2 * est_se), 
                  ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y") + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a smoother on depth 2021 on Atlantic mackerel biomass predictions \nusing the combined models of a delta poisson gamma distribution and removing outliers")
ggsave("atlmack_dpg-m11no-out_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m11_no.out, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)
ggsave("atlmack_dpg-m11no-out_dep-yr-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

##### EXAMPLE PLOTS TO EXAMINE MODEL COMPONENTS - SDMTMB VIGNETTE ####
#nd2010 <- dplyr::filter(nd, EST_YEAR == 2010)
#p_2010 <- predict(dpg_m11.no_out, newdata = nd2010, re_form = NA)
p_m11_comps <- predict(dpg_m11.no_out, newdata = nd, re_form = NA)
saveRDS(p_m11_comps, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11-dpg-comps-no.out.rds"))
n <- exp(p_m11_comps$est1)
w <- exp(p_m11_comps$est2)
p <- 1 - exp(-n)
r <- (n * w) / p
lims <- c(0, max(p * r))
plot(n * w, p * r, xlim = lims, ylim = lims)
abline(0, 1)

g1 <- ggplot(p_m11_comps , aes(AVGDEPTH, n, color = EST_YEAR)) +
  geom_line() +
  ggtitle("Expected group density")
g2 <- ggplot(p_m11_comps , aes(AVGDEPTH, w, color = EST_YEAR)) +
  geom_line() +
  ggtitle("Expected weight per group")
g3 <- ggplot(p_m11_comps , aes(AVGDEPTH, p, color = EST_YEAR)) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("Expected encounter probability")
g4 <- ggplot(p_m11_comps , aes(AVGDEPTH, r, color = EST_YEAR)) +
  geom_line() +
  ggtitle("Expected catch given encounter")
g5 <- ggplot(p_m11_comps , aes(AVGDEPTH, n * w, color = EST_YEAR)) +
  geom_line() +
  ggtitle("Expected catch")
g6 <- ggplot(p_m11_comps , aes(AVGDEPTH, p * r, color = EST_YEAR)) +
  geom_line() +
  ggtitle("Expected catch")

pgrid <- cowplot::plot_grid(g1 + theme(legend.position="none"), 
                   g2 + theme(legend.position="none"), 
                   g3 + theme(legend.position="none"), 
                   g4 + theme(legend.position="none"), 
                   g5 + theme(legend.position="none"), 
                   g6 + theme(legend.position="none"), ncol = 2) 
p_leg <- cowplot::get_legend(
  # create some space to the left of the legend
  g1 + guides(color = guide_legend(nrow = 1)) + 
    theme(legend.position = "bottom")
)
cowplot::plot_grid(pgrid, p_leg, ncol = 1, rel_heights = c(1, .1))
ggsave("atlmack_dpg-m11no-out_cowplot.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 8, height = 10, plot = last_plot())





#### model 1 ####
p_m11.1_no.out <- predict(dpg_m11.no_out, newdata = nd, se_fit = TRUE, re_form = NA, model = 1)
saveRDS(p_m11.1_no.out, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11.1-dpg-preds-no.out.rds"))

ggplot(p_m11.1_no.out |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                    ymin = exp(est - 2 * est_se), 
                    ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a smoother on depth 2021 on Atlantic mackerel biomass predictions \nusing MODEL 1 from the delta poisson gamma distribution and removing outliers") +
  facet_wrap(~AREA, scales = "free_y")
ggsave("atlmack_dpg-m11.1no-out_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m11, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)


#### model 2 ####
p_m11.2_no.out <- predict(dpg_m11.no_out, newdata = nd, se_fit = TRUE, re_form = NA, model = 2)
saveRDS(p_m11.2_no.out, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m11.2-dpg-preds-no.out.rds"))

ggplot(p_m11.2_no.out |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                                                      ymin = exp(est - 2 * est_se), 
                                                      ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a smoother on depth 2021 on Atlantic mackerel biomass predictions \nusing MODEL 2 from the delta poisson gamma distribution and removing outliers") +
  facet_wrap(~AREA, scales = "free_y")
ggsave("atlmack_dpg-m11.2no-out_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())



## m12 ####
# visreg_delta(dpg_m12, xvar = "AVGDEPTH", model = 1, gg = TRUE)
# visreg_delta(dpg_m12, xvar = "AVGDEPTH", model = 2, gg = TRUE)
# visreg_delta(dpg_m12.no_out, xvar = "AVGDEPTH", model = 1, gg = TRUE)
# visreg_delta(dpg_m12.no_out, xvar = "AVGDEPTH", model = 2, gg = TRUE)

### with outliers ####
#### combined ####
p_m12 <- predict(dpg_m12, newdata = nd, se_fit = TRUE, re_form = NA, model = NA)
saveRDS(p_m12, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m12-dpg-preds.rds"))

ggplot(p_m12 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                                              ymin = exp(est - 2 * est_se), 
                                              ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y") + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a parabolic relationship with depth in 2021 on Atlantic mackerel biomass predictions \nusing the combined models from a delta poisson gamma distribution")
ggsave("atlmack_dpg-m12_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m12, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)
ggsave("atlmack_dpg-m12_dep-yr-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

#### model 1 ####
p_m12.1 <- predict(dpg_m12, newdata = nd, se_fit = TRUE, re_form = NA, model = 1)
saveRDS(p_m12.1, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m12.1-dpg-preds.rds"))

ggplot(p_m12.1 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, plogis(est), 
                                                ymin = plogis(est - 2 * est_se), 
                                                ymax = plogis(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a parabolic relationship with depth in 2021 on Atlantic mackerel biomass predictions \nusing MODEL 1 of a delta poisson gamma distribution") +
  facet_wrap(~AREA, scales = "free_y") #+ facet_grid(cols = vars(AREA), rows = vars(EST_YEAR), scales = "free_y") 
ggsave("atlmack_dpg-m12.1_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m12.1, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)


#### model 2 ####
p_m12.2 <- predict(dpg_m12, newdata = nd, se_fit = TRUE, re_form = NA, model = 2)
saveRDS(p_m12.2, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m12.2-dpg-preds.rds"))

ggplot(p_m12.2  |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                    ymin = exp(est - 2 * est_se), 
                    ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a parabolic relationship with depth in 2021 on Atlantic mackerel biomass predictions \nusing MODEL 2 of a delta poisson gamma distribution") +
  facet_wrap(~AREA, scales = "free_y")
ggsave("atlmack_dpg-m12.2_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m12.2, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)

### without outliers ####
#### combined ####
p_m12_no.out <- predict(dpg_m12.no_out, newdata = nd, se_fit = TRUE, re_form = NA, model = NA)
saveRDS(p_m12_no.out, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m12-dpg-preds-no.out.rds"))

ggplot(p_m12_no.out |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est), 
                                                     ymin = exp(est - 2 * est_se), 
                                                     ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y") + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of a parabolic relationship with depth in 2021 on Atlantic mackerel biomass predictions \nwhen outliers are removed and using the combined models of a delta poisson gamma distribution")
ggsave("atlmack_dpg-m12no-out_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

ggplot(p_m12_no.out, aes(AVGDEPTH, est, colour = EST_YEAR)) +
  geom_line() + facet_wrap(~AREA)
ggsave("atlmack_dpg-m12no-out_dep-yr-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

#### model 1 ####
p_m12.1_no.out <- predict(dpg_m12.no_out, newdata = nd, se_fit = TRUE, re_form = NA, model = 1)
saveRDS(p_m12.1_no.out, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m12.1-dpg-preds-no.out.rds"))

ggplot(p_m12.1_no.out |> filter(EST_YEAR == 2021), aes(AVGDEPTH, plogis(est), 
                                                ymin = plogis(est - 2 * est_se), 
                                                ymax = plogis(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) + 
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of area and a parabolic relationship with depth in 2021 on Atlantic mackerel biomass predictions \nwhen outliers are removed and using MODEL 1 of a delta poisson gamma distribution") +
  facet_wrap(~AREA, scales = "free_y") #+ facet_grid(cols = vars(AREA), rows = vars(EST_YEAR), scales = "free_y") 
ggsave("atlmack_dpg-m12.1no-out_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())

#### model 2 ####
p_m12.2_no.out <- predict(dpg_m12.no_out, newdata = nd, se_fit = TRUE, re_form = NA, model = 2)
saveRDS(p_m12.2_no.out, here("sdmtmb", "atlmackerel", "data", "marg-effects", "m12.2-dpg-preds-no.out.rds"))

ggplot(p_m12.2_no.out |> filter(EST_YEAR == 2021) , aes(AVGDEPTH, exp(est), 
                    ymin = exp(est - 2 * est_se), 
                    ymax = exp(est + 2 * est_se))) + 
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  labs(x = "Depth", y = "Biomass (kg)", subtitle = "Marginal effects of area and a parabolic relationship with depth in 2021 on Atlantic mackerel biomass predictions \nwhen outliers are removed and using MODEL 2 of a delta poisson gamma distribution") +
  facet_wrap(~AREA, scales = "free_y")
ggsave("atlmack_dpg-m12.2no-out_depth-meffects.png", device = "png", here("sdmtmb", "atlmackerel", "plots", "marg-effects"), width = 10, height = 6, plot = last_plot())


# test if predicting at same loctions gives you same shape 
# test3 <- predict(dpg_m12, se_fit = TRUE, re_form = NA, model = NA)
# ggplot(test3 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est),
#                                               ymin = exp(est - 2 * est_se),
#                                               ymax = exp(est + 2 * est_se))) +
#   geom_line() +
#   geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y")
# 
# # same test but without outliers 
# test4 <- predict(dpg_m12.no_out, se_fit = TRUE, re_form = NA, model = NA)
# ggplot(test4 |> filter(EST_YEAR == 2021), aes(AVGDEPTH, exp(est),
#                                               ymin = exp(est - 2 * est_se),
#                                               ymax = exp(est + 2 * est_se))) +
#   geom_line() +
#   geom_ribbon(alpha = 0.5) + facet_wrap(~AREA, scales = "free_y")
