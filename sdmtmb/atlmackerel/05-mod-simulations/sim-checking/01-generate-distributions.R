### created: 01/09/2024
### last updated: 

# 01 - GENERATE DISTRIBUTIONS  ####

## OBJECTIVE ####
# generate a tweedie distribution of random values using the mean, phi, and tweedie param  
# compare the differences 
# change the scale of observations and underlying mean and compare the differences


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 
library(tweedie)

atlmackerel.dat <- here("sdmtmb", "atlmackerel", "data")
sim.check <- here("sdmtmb", "atlmackerel", "data", "simulations", "sim-checking")
sim.check.plots <- here("sdmtmb", "atlmackerel", "plots", "simulations", "sim-checking")

### LOAD DATA ####
# read in spring model
spring_mod <- readRDS(here(atlmackerel.dat, "spring_mod.rds"))
spring_ran_pars <- tidy(spring_mod, "ran_pars")

# read in spring predictions
# spring_preds <- readRDS(here(sumflounder.dat, "spring_mod_preds.rds"))



## SPRING DISTRIBUTIONS ####
# generate random numbers using spring model parameters 
set.seed(987654)

# mean 1 data 
rnum1 <- data.frame(val = rtweedie(100, power = spring_ran_pars$estimate[5], mu = 1, phi=spring_ran_pars$estimate[2]), 
                    type = "Control")
rnum1.1 <- data.frame(val = rtweedie(100,  power = spring_ran_pars$estimate[5], mu = 1 + log(2), phi=spring_ran_pars$estimate[2]), 
                      type = "Increase")
rnum1.2 <- data.frame(val = rtweedie(100,  power = spring_ran_pars$estimate[5], mu = 1 - log(2), phi=spring_ran_pars$estimate[2]), 
                      type = "Decrease")
rnum1_df <- bind_rows(rnum1, rnum1.1, rnum1.2) |> 
  mutate(mean_val = 1)

# rnum1_plot <- ggplot(rnum1_df) + 
#   geom_histogram(aes(x = val, fill = type)) + 
#   facet_grid(cols = vars(type), rows = vars(mean_val)) + 
#   theme_bw()

# mean 10 data 
rnum10 <- data.frame(val = rtweedie(100, power = spring_ran_pars$estimate[5], mu = 10, phi = spring_ran_pars$estimate[2]), 
                     type = "Control")
rnum10.1 <- data.frame(val = rtweedie(100, power = spring_ran_pars$estimate[5], mu = 10 + log(2), phi=spring_ran_pars$estimate[2]), 
                       type = "Increase")
rnum10.2 <- data.frame(val = rtweedie(100, power = spring_ran_pars$estimate[5], mu = 10 - log(2), phi=spring_ran_pars$estimate[2]), 
                       type = "Decrease")
rnum10_df <- bind_rows(rnum10, rnum10.1, rnum10.2) |> 
  mutate(mean_val = 10)

# rnum10_plot <- ggplot(rnum10_df) + 
#   geom_histogram(aes(x = val, fill = type)) + 
#   facet_grid(cols = vars(type), rows = vars(mean_val)) + 
#   theme_bw()

# mean 100 data 
rnum100 <- data.frame(val = rtweedie(100, power = spring_ran_pars$estimate[5], mu = 100, phi = spring_ran_pars$estimate[2]), 
                      type = "Control")
rnum100.1 <- data.frame(val = rtweedie(100, power = spring_ran_pars$estimate[5], mu = 100 + log(2), phi=spring_ran_pars$estimate[2]), 
                        type = "Increase")
rnum100.2 <- data.frame(val = rtweedie(100, power = spring_ran_pars$estimate[5], mu = 100 - log(2), phi=spring_ran_pars$estimate[2]), 
                        type = "Decrease")
rnum100_df <- bind_rows(rnum100, rnum100.1, rnum100.2) |> 
  mutate(mean_val = 100)

# rnum100_plot <- ggplot(rnum100_df) + 
#   geom_histogram(aes(x = val, fill = type)) + 
#   facet_grid(cols = vars(type), rows = vars(mean_val)) + 
#   theme_bw()

# rnum0.5 <- data.frame(val = rtweedie(100, power = 1.34, mu = 0.5, phi=1.54), 
#                     type = "base")
# rnum05.1 <- data.frame(val = rtweedie(100, power = 1.34, mu = 0.5 + log(2), phi=1.54), 
#                       type = "increase")
# rnum05.2 <- data.frame(val = rtweedie(100, power = 1.34, mu = 0.5 - log(2), phi=1.54), 
#                       type = "decrease")
# rnum05_df <- bind_rows(rnum0.5, rnum05.1, rnum05.2) |> 
#   mutate(mean_val = 0.5)
# 
# rnum05_plot <- ggplot(rnum05_df) + 
#   geom_histogram(aes(x = val, fill = type)) + 
#   facet_grid(cols = vars(type), rows = vars(mean_val)) + 
#   theme_bw()


rnum_test <- bind_rows(rnum1_df, rnum10_df, rnum100_df)

ggplot(rnum_test) + 
  geom_histogram(aes(x = val, fill = type)) + 
  facet_grid(cols = vars(type), rows = vars(mean_val)) + 
  theme_bw() + 
  labs(x = "Random number generated", y = "Frequency of occurrence", fill = "Treatment", title = "Spring Distributions", subtitle = "Values in the faceted rows represent the starting mean value") + 
  theme(legend.position = "none")

ggsave("atlmackerel_ran-spring-num-dist.png", device = "png", last_plot(), here(sim.check.plots), width = 8, height = 5)


