### created: 12/10/2022
### last updated: 04/11/2023

#### 04b - SPRING MODEL VISUALIZATION ####

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
library(visreg)
# library(marmap)
# library(raster)
theme_set(theme_bw())

here()

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"

#### LOAD DATA ####
# worst fit generated from `02a-fit-models.R` here("sdmtmb")
#m1 <- readRDS(file = here("sdmtmb", "model-outputs", "m1.rds"))

# best fit generated from `02a-fit-models.R` here("sdmtmb")
m7_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m7_spring.rds"))
m8_spring <- readRDS(file = here(sdmtmb.dir, "model-outputs", "m8_spring2.rds"))

# summer flounder data prepared in `01-prepare-data.R` here("sdmtmb")
sf_spring <- readRDS(here(sdmtmb.dir, "data", "sumflounder_spring.rds"))

# 
spring_mesh <- readRDS(here("sdmtmb", "data", "spring_mesh.rds"))

#### PREPARE RESIDUALS #### 

##### Best Fit Model ####
sf_spring$m7_res <- residuals(m7_spring) # pull residuals from best fit model
qqnorm(sf_spring$m7_res)
qqline(sf_spring$m7_res)

spr_fit7_ml <- update(m7_spring, reml = FALSE) #refit your model with `reml = FALSE` to use MCMC-MLE residuals.
# Warning message:
#   In doTryCatch(return(expr), name, parentenv, handler) :
#   display list redraw incomplete
saveRDS(spr_fit7_ml, here("sdmtmb", "data", "m7spring_reml-off.rds"))
sanity(spr_fit7_ml)

samps7 <- sdmTMBextra::predict_mle_mcmc(spr_fit7_ml, mcmc_iter = 201, mcmc_warmup = 200)

m7spr_mcres <- residuals(m7_spring, type = "mle-mcmc", mcmc_samples = samps7)
qqnorm(m7spr_mcres)
qqline(m7spr_mcres)

saveRDS(m7spr_mcres, here("sdmtmb", "data", "m7spr_mcres.rds"))

sf_spring$m7_mcres <- m7spr_mcres



sf_spring$m8_res <- residuals(m8_spring) # pull residuals from best fit model

qqnorm(sf_spring$m8_res)
qqline(sf_spring$m8_res)


spr_fit8_ml <- update(m8_spring, reml = FALSE) #refit your model with `reml = FALSE` to use MCMC-MLE residuals.
saveRDS(spr_fit8_ml, here("sdmtmb", "data", "m8spring_reml-off.rds"))

samps8 <- sdmTMBextra::predict_mle_mcmc(spr_fit8_ml, mcmc_iter = 201, mcmc_warmup = 200)

m8spr_mcres <- residuals(m8_spring, type = "mle-mcmc", mcmc_samples = samps8)
hist(m8spr_mcres)
qqnorm(m8spr_mcres)
qqline(m8spr_mcres)

saveRDS(m8spring_mcres, here("sdmtmb", "data", "m8spring_mcres.rds"))

sf_spring$m8_mcres <- m8spr_mcres

# save the data 
saveRDS(sf_spring, file = here("sdmtmb", "data", "sf_spring-resids.rds"))


#### VISUALISATIONS ####

g <- ggeffect(m6_spring, "AVGDEPTH")
plot(g)

#the value of depth on the x-axis and the change in response on the y-axis, holding all other variables constant.
visreg(fit, xvar = "depth_scaled")


##### Best fit model ####
# plotting residuals of best fit model
# hist(sf_spring$m6_res) # plot frequency of residuals to find distribution 
# 
# qqnorm(sf_spring$m6_res) # qplot 
# abline(a = 0, b = 1) # add trend line
# 
# 
# 
# # from the mcmc residuals 
# hist(sf_spring$m6_mcres, 
#      main = "Spring MCMC Residuals for Summer Flounder", 
#      xlab = "Residuals")
# 
# 
# qqnorm(sf_spring$m6_mcres)
# abline(a = 0, b = 1) # add trend line
#ggsave(filename = "sf_spring-m6qqplot.png", device = "png", path = here("sdmtmb", "plots"), width = 8, height = 8)


# residuals plotted by year
ggplot(sf_spring, aes(DECDEG_BEGLON, DECDEG_BEGLAT, color = m8_mcres)) +
  scale_color_gradient2()+#low = "#3f7f00", high = "#0a4c8a") +
  geom_point() + 
  facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Residuals") +
  #guides(color = "none", scales = "none") + 
  theme(legend.position= c(0.8, 0.1),
        legend.direction = "horizontal",
        #legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) 

ggsave(filename = "sf_spring-m6mcresids.png", device = "png", path = here("sdmtmb", "plots"), width = 8, height = 8)

# subset years to get a closer look 
sf_spring %>% 
  filter(EST_YEAR %in% c(2018, 2019, 2020, 2021)) %>% 
  ggplot(aes(X, Y, col = m8_mcres)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(title = "Spring IID Spatiotemporal Model Residuals") +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) #+ 
  #scale_color_manual(values = pal)

ggsave(filename = "sf_spring-m6mcresids2.png", device = "png", path = here("sdmtmb", "plots"), width = 10, height = 5)

# marginal residual plot in link space
#visreg::visreg(m1, xvar = "depth", xlim = c(30, 350), xlab = "Depth")


# marginal residual plot of response variables
visreg::visreg(m6_spring, xvar = "AVGDEPTH", scale = "response", nn = 200, xlab = "Depth", ylab = "Biomass (kg)")



# predicted probability of biomass versus depth; plot predictions against covariates 
pred_df <- sf_spring
pred_df$pred <- predict(m6_spring)$est
#a nonlinear pattern here that shows a trend that we probably want to include. 
ggplot(pred_df, aes(AVGDEPTH, pred)) + 
  geom_point(size=0.3, alpha=0.5) + 
  geom_smooth(se = FALSE) + 
  xlab("sf_spring$AVGDEPTH") + 
  ylab("predict(m6_spring)$est")

ggsave(filename = "sf_spring-pred-depth.png", device = "png", path = here("sdmtmb", "plots"), width = 12, height = 10)

##### Worst fit model ####
# plotting residuals of worst fit model
# hist(sumflounder$m1_res) # plot frequency of residuals to find distribution 
# labs(x = "Residuals", title = "M1 Residuals for Summer Flounder")
# 
# qqnorm(sumflounder$m1_res) # qplot 
# abline(a = 0, b = 1) # add trend line
# 
# 
# # from the mcmc residuals 
# #hist(sumflounder$m1_mcres)
# 
# # qqnorm(sumflounder$m1_mcres)
# # abline(a = 0, b = 1) # add trend line
# 
# 
# 
# # residuals plotted by year
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
# 
