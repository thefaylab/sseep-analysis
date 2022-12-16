### created: 12/10/2022
### last updated: 

#### 02c - MODEL VISUALIZATION ####

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
m1 <- readRDS(file = here("sdmtmb", "model-outputs", "m1.rds"))

# best fit generated from `02a-fit-models.R` here("sdmtmb")
m6 <- readRDS(file = here("sdmtmb", "model-outputs", "m6.rds"))

# summer flounder data prepared in `01-prepare-data.R` here("sdmtmb")
sumflounder <- readRDS(here("sdmtmb", "data", "sumflounder.rds"))


#### PREPARE RESIDUALS #### 

##### Best Fit Model ####
sumflounder$m6_res <- residuals(m6) # pull residuals from best fit model
m6_mcres <- residuals(m6, type = "mle-mcmc", mcmc_iter = 201, mcmc_warmup = 200)

sumflounder$m6_mcres <- m6_mcres


##### Worst Fit Model ####
sumflounder$m1_res <- residuals(m1) # pull residuals from best fit model
#m1_mcres <- residuals(m1, type = "mle-mcmc", mcmc_iter = 201, mcmc_warmup = 200)
#sumflounder$m1_mcres <- m1_mcres


# save the data 
saveRDS(sumflounder, file = here("sdmtmb", "data", "sumflounder-res.rds"))


#### VISUALISATIONS ####

g <- ggeffect(m5, "depth [0:400 by=1]")
plot(g)


##### Best fit model ####
# plotting residuals of best fit model
hist(sumflounder$m6_res) # plot frequency of residuals to find distribution 

qqnorm(sumflounder$m6_res) # qplot 
abline(a = 0, b = 1) # add trend line



# from the mcmc residuals 
hist(sumflounder$m6_mcres, 
     main = "M6 MCMC Residuals for Summer Flounder", 
     xlab = "Residuals")


qqnorm(sumflounder$m6_mcres)
abline(a = 0, b = 1) # add trend line



# residuals plotted by year
ggplot(sumflounder, aes(X, Y, col = m6_mcres)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~year) + coord_fixed() +
  labs(title = "m6 MCMC Residuals") +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

ggsave(filename = "sumflounder-m6res.png", device = "png", path = here("sdmtmb", "plots"), width = 8, height = 8)

# subset years to get a closer look 
sumflounder %>% 
  filter(year %in% c(2018, 2019, 2021)) %>% 
  ggplot(aes(X, Y, col = m6_mcres)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~year) + coord_fixed() +
  labs(title = "m6 MCMC Residuals") +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) #+ 
  #scale_color_manual(values = pal)

ggsave(filename = "sumflounder-m6res2.png", device = "png", path = here("sdmtmb", "plots"), width = 10, height = 5)

# marginal residual plot in link space
visreg::visreg(m1, xvar = "depth", xlim = c(30, 350), xlab = "Depth")


# marginal residual plot of response variables
visreg::visreg(m6, xvar = "depth", scale = "response", xlim = c(30, 350), nn = 200, xlab = "Depth", ylab = "Biomass (kg)")



# predicted probability of biomass versus depth; plot predictions against covariates 
pred_df <- sumflounder
pred_df$pred <- predict(m6)$est
#a nonlinear pattern here that shows a trend that we probably want to include. 
ggplot(pred_df, aes(depth, pred)) + 
  geom_point(size=0.3, alpha=0.5) + 
  geom_smooth(se = FALSE) + 
  xlab("sumflounder$depth") + 
  ylab("predict(m6)$est")

ggsave(filename = "sumflounder-pred-depth.png", device = "png", path = here("sdmtmb", "plots"), width = 12, height = 10)

##### Worst fit model ####
# plotting residuals of worst fit model
hist(sumflounder$m1_res) # plot frequency of residuals to find distribution 
labs(x = "Residuals", title = "M1 Residuals for Summer Flounder")

qqnorm(sumflounder$m1_res) # qplot 
abline(a = 0, b = 1) # add trend line


# from the mcmc residuals 
#hist(sumflounder$m1_mcres)

# qqnorm(sumflounder$m1_mcres)
# abline(a = 0, b = 1) # add trend line



# residuals plotted by year
ggplot(sumflounder, aes(X, Y, col = m1_res)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~year) + coord_fixed() +
  labs(title = "m1 Residuals") +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

ggsave(filename = "sumflounder-m1res.png", device = "png", path = here("sdmtmb", "plots"), width = 8, height = 8)

# subset years to get a closer look 
sumflounder %>% 
  filter(year %in% c(2018, 2019, 2021)) %>% 
  ggplot(aes(X, Y, col = m1_res)) + scale_colour_gradient2() +
  geom_point() + facet_wrap(~year) + coord_fixed() +
  labs(title = "m1 Residuals") +
  theme(legend.position="bottom",
        #legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))

ggsave(filename = "sumflounder-m1res2.png", device = "png", path = here("sdmtmb", "plots"), width = 10, height = 5)

