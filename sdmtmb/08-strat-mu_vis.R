### created: 11/25/2022
### last updated: 12/8/2022

#### 07 - PLOTTING STRATIFIED MEANS ####

###################
#### OBJECTIVE ####
###################
# plot the stratified means calculated for summer flounder across time and data generation source. 

# compare plots to identify potential impacts to abundance indexes when wind tows are removed and to compare simulated data from models fit to historical data. 

####################


#### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

#install.packages("devtools")
#devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)


#### LOAD DATA ####
# dataset created from `02-bind-stratmeans.R` here("retro-analysis"). Contains stratified means and variances for each species and unique tow over time.
stratmeans <- readRDS(here("data", "rds", "stratmu_all.rds"))


# calculate the logistic standard deviation and logistic normal distribution 
stratmeans <- stratmeans %>% 
  group_by(year) %>% 
  mutate(sdlog1 = sqrt(log(1+(sqrt(stratvar1)/stratmu1)^2)), #logistic standard deviation
         lower1 = qlnorm(0.025, log(stratmu1), sdlog1), # lower quantile of the logistic normal distribution
         upper1 = qlnorm(0.975, log(stratmu1), sdlog1),  # upper quantile of the logistic normal distribution
         sdlog2 = sqrt(log(1+(sqrt(stratvar2)/stratmu2)^2)),
         lower2 = qlnorm(0.025, log(stratmu2), sdlog2),
         upper2 = qlnorm(0.975, log(stratmu2), sdlog2))


# create universal aesthetics for ggplot
names(park_palettes)
pal <- park_palette("SmokyMountains", 5)



#### GGPLOTS ####

# plot historical data vs first set of simulated data
ggplot(stratmeans) +
  aes(x = as.factor(SOURCE), y = stratmu1, color = TYPE, shape = SOURCE) +
  geom_pointrange(aes(ymin=lower1, ymax = upper1), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(year), scales = "free_y") +
  labs(x = "Data Source", y = "Stratified Mean (kg/tow)", title = "Stratified Mean Biomass for Summer Flounder", subtitle = "Stratified mean biomasses are compared using the historical data set versus the first set of simulated data from the worst and best fit sdmTMB models",  TYPE = "") +
  ylim(0,NA) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + 
  scale_color_manual(values = pal)

#save the plot
ggsave(filename = "sumflounder-v-sim1.png", device = "png", path = here("sdmtmb", "plots"), width = 12, height = 10)


ggplot(stratmeans) +
  aes(x = as.factor(year), y = stratmu1, color = SOURCE, lty = TYPE) +
  geom_pointrange(aes(ymin=lower1, ymax = upper1), position =  position_dodge2(width=0.4)) +
  # facet_wrap(vars(TYPE), scales = "free_y") +
  labs(x = "Data Source", y = "Stratified Mean (kg/tow)", title = "Stratified Mean Biomass for Summer Flounder", subtitle = "Stratified mean biomasses are compared using the historical data set versus the first set of simulated data from the worst and best fit sdmTMB models",  TYPE = "") +
  ylim(0,NA) +
  theme_minimal() + 
  theme(legend.position="bottom",
        legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + 
  scale_color_manual(values = pal)

ggsave(filename = "sumflounder-v-sim1-v2.png", device = "png", path = here("sdmtmb", "plots"), width = 12, height = 10)



# plot historical data vs second set of simulated data
ggplot(stratmeans) +
  aes(x = as.factor(SOURCE), y = stratmu2, color = TYPE, shape = SOURCE) +
  geom_pointrange(aes(ymin=lower2, ymax = upper2), position =  position_dodge2(width=0.4)) +
  facet_wrap(vars(year), scales = "free_y") + 
  labs(x = "Data Source", y = "Stratified Mean (kg/tow)", title = "Stratified Mean Biomass for Summer Flounder", subtitle = "Stratified mean biomasses are compared using the historical data set versus the second set of simulated data from the worst and best fit sdmTMB models", TYPE = "") +
  ylim(0,NA) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + 
  scale_color_manual(values = pal)

# save the plot
ggsave(filename = "sumflounder-v-sim2.png", device = "png", path = here("sdmtmb", "plots"), width = 12, height = 10)

ggplot(stratmeans) +
  aes(x = as.factor(year), y = stratmu2, color = SOURCE, lty = TYPE) +
  geom_pointrange(aes(ymin=lower2, ymax = upper2), position =  position_dodge2(width=0.4)) +
  #facet_wrap(vars(year), scales = "free_y") + 
  labs(x = "Data Source", y = "Stratified Mean (kg/tow)", title = "Stratified Mean Biomass for Summer Flounder", subtitle = "Stratified mean biomasses are compared using the historical data set versus the second set of simulated data from the worst and best fit sdmTMB models", TYPE = "") +
  ylim(0,NA) +
  theme_minimal() + 
  theme(legend.position="bottom",
        legend.title = element_blank(), 
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + 
  scale_color_manual(values = pal)

# save the plot
ggsave(filename = "sumflounder-v-sim2.png", device = "png", path = here("sdmtmb", "plots"), width = 12, height = 10)


# ggplot(stratmeans) +
#   geom_pointrange(aes(x = as.factor(SOURCE), y = stratmu1, color = TYPE, shape = SOURCE, ymin=lower1, ymax = upper1), position =  position_dodge2(width=0.4)) +
#   geom_pointrange(aes(x = as.factor(SOURCE), y = stratmu2, color = TYPE, shape = SOURCE, ymin=lower2, ymax = upper2), position =  position_dodge2(width=0.4)) +
#   facet_wrap(vars(year), scales = "free_y") + 
#   labs(x = "Data Source", y = "Stratified Mean (kg/tow)", title = "Stratified Mean Biomass for Summer Flounder", subtitle = "Stratified mean biomasses are compared using the historical data set versus the second set of simulated data from the worst and best fit sdmTMB models", TYPE = "") +
#   ylim(0,NA) +
#   theme_bw() + 
#   theme(legend.position="bottom",
#         legend.title = element_blank(), 
#         axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"))) + 
#   scale_color_manual(values = pal)
# 







