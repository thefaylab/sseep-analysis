### created: 12/10/2022
### last updated: 03/05/2023

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
# library(marmap)
# library(raster)

here()

#### LOAD DATA ####
# worst fit generated from `02a-fit-models.R` here("sdmtmb")
#m1 <- readRDS(file = here("sdmtmb", "model-outputs", "m1.rds"))

# best fit generated from `02a-fit-models.R` here("sdmtmb")
m6_spring <- readRDS(file = here("sdmtmb", "model-outputs", "m6_spring.rds"))

# summer flounder data prepared in `01-prepare-data.R` here("sdmtmb")
sf_spring <- readRDS(here("sdmtmb", "data", "sumflounder_spring.rds"))


#### PREPARE RESIDUALS #### 

##### Best Fit Model ####
sf_spring$m6_res <- residuals(m6_spring) # pull residuals from best fit model
m6spr_mcres <- residuals(m6_spring, type = "mle-mcmc", mcmc_iter = 201, mcmc_warmup = 200)
saveRDS(m6spr_mcres, here("sdmtmb", "data", "m6spr_mcres.rds"))

sf_spring$m6_mcres <- m6spr_mcres


##### Worst Fit Model ####
#sumflounder$m1_res <- residuals(m1) # pull residuals from best fit model
#m1_mcres <- residuals(m1, type = "mle-mcmc", mcmc_iter = 201, mcmc_warmup = 200)
#sumflounder$m1_mcres <- m1_mcres


# save the data 
saveRDS(sf_spring, file = here("sdmtmb", "data", "sf_spring-resids.rds"))


#### VISUALISATIONS ####

g <- ggeffect(m6_spring, "AVGDEPTH")
plot(g)


##### Best fit model ####
# plotting residuals of best fit model
hist(sf_spring$m6_res) # plot frequency of residuals to find distribution 

qqnorm(sf_spring$m6_res) # qplot 
abline(a = 0, b = 1) # add trend line



# from the mcmc residuals 
hist(sf_spring$m6_mcres, 
     main = "Spring MCMC Residuals for Summer Flounder", 
     xlab = "Residuals")


qqnorm(sf_spring$m6_mcres)
abline(a = 0, b = 1) # add trend line
#ggsave(filename = "sf_spring-m6qqplot.png", device = "png", path = here("sdmtmb", "plots"), width = 8, height = 8)


# residuals plotted by year
ggplot(sf_spring, aes(DECDEG_BEGLON, DECDEG_BEGLAT, color = m6_mcres)) +
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
  filter(EST_YEAR %in% c(2018, 2019, 2021)) %>% 
  ggplot(aes(X, Y, col = m6_mcres)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~EST_YEAR) + coord_fixed() +
  labs(title = "m6 MCMC Residuals") +
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
