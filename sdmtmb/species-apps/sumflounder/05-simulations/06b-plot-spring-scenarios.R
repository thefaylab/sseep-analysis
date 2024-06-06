### created: 08/17/2023
### last updated: 

# 04 -  DATA ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"
source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
theme_set(theme_bw())
#set.seed(123)

### LOAD DATA ####
# created here("sdmtmb", "sumflounder", "05-simulations", "03-fit-fall-lin-regs"))
base_slopes <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "base_slopes.rds"))



## STRATMU ####
base_strat_plot <- base_all.stratmu |> 
  mutate(SCENARIO = "BASELINE") |> 
  group_by(EST_YEAR, TYPE) |> 
  summarise(med = median(stratmu), 
            lower = quantile(lower, 0.025), 
            upper = quantile(upper, 0.975)) |>
  ggplot()+
  aes(EST_YEAR, med, color = TYPE) +
  scale_color_manual(values = c("#3f7f00", "orange")) +
  geom_point() + 
  geom_line() + 
  labs(x = "", y = "Stratified Mean (kg/tow)", subtitle = "Baseline")+ 
  theme(plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")
  #geom_pointrange(aes(ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) 

inc_strat_plot <- inc_all.stratmu |> 
  mutate(SCENARIO = "ENHANCED") |> 
  group_by(EST_YEAR, TYPE) |> 
  summarise(med = median(stratmu), 
            lower = quantile(lower, 0.025), 
            upper = quantile(upper, 0.975)) |>
  ggplot()+
  aes(EST_YEAR, med, color = TYPE) +
  scale_color_manual(values = c("#3f7f00", "orange")) +
  geom_point() + 
  geom_line() + 
  labs(x = "", y = "Stratified Mean (kg/tow)", subtitle = "Enhanced")+ 
  theme(plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

dec_strat_plot <- dec_all.stratmu |> 
  mutate(SCENARIO = "REDUCED") |> 
  group_by(EST_YEAR, TYPE) |> 
  summarise(med = median(stratmu), 
            lower = quantile(lower, 0.025), 
            upper = quantile(upper, 0.975)) |>
  ggplot()+
  aes(EST_YEAR, med, color = TYPE) +
  scale_color_manual(values = c("#3f7f00", "orange")) +
  geom_point() + 
  geom_line() + 
  labs(x = "Year", y = "Stratified Mean (kg/tow)", subtitle = "Reduced")+ 
  theme(plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom")

(base_strat_plot + inc_strat_plot + dec_strat_plot) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


## PROJECTIONS ####
ggplot(sim_base_grid)+ 
  aes(EST_YEAR, EXPCATCHWT, group = rep)+
  geom_line(alpha = 0.3, col = gray(0.5))

sim_base_grid |> 
  group_by(EST_YEAR) |>
  median_qi(EXPCATCHWT, .width = c(0.25, 0.5, 0.95)) |> 
  ggplot()+ 
  aes(EST_YEAR, EXPCATCHWT, ymin = .lower, ymax = .upper) + 
  geom_lineribbon()+
  scale_fill_brewer()

# base_all.stratmu |> 
#   group_by(EST_YEAR, TYPE) |>
#   median_qi(stratmu, .width = c(0.25, 0.5, 0.95)) |> 
#   ggplot()+ 
#   aes(EST_YEAR, stratmu, ymin = .lower, ymax = .upper) + 
#   geom_lineribbon()+
#   scale_fill_brewer() + 
#   facet_wrap(~TYPE)

# base_all.stratmu|> 
#   group_by(EST_YEAR, TYPE) |>
#   summarise(med = median(stratmu), 
#             lower = quantile(lower, 0.025), 
#             upper = quantile(upper, 0.975)) |> 
#   ggplot() +
#   aes(EST_YEAR, med, color = TYPE) + 
#   geom_point(position = position_dodge2(width = 0.4)) +
#   geom_errorbar(aes(ymin = lower, ymax = upper),width= 0.4, position = position_dodge2(width = 0.4))

# inc_all.stratmu |> 
#   group_by(EST_YEAR, TYPE) |>
#   median_qi(stratmu, .width = c(0.25, 0.5, 0.95)) |> 
#   ggplot()+ 
#   aes(EST_YEAR, stratmu, ymin = .lower, ymax = .upper) + 
#   geom_lineribbon()+
#   scale_fill_brewer() + 
#   facet_wrap(~TYPE)
# 
# dec_all.stratmu |> 
#   group_by(EST_YEAR, TYPE) |>
#   median_qi(stratmu, .width = c(0.25, 0.5, 0.95)) |> 
#   ggplot()+ 
#   aes(EST_YEAR, stratmu, ymin = .lower, ymax = .upper) + 
#   geom_lineribbon()+
#   scale_fill_brewer() + 
#   facet_wrap(~TYPE)


## MEAN SQUARED DIFFERENCE ####
all_mudiff <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_AllMudiff.rds"))

ggplot(all_mudiff) + 
  aes(x = str_to_sentence(SCENARIO), y = mudiff, color = SCENARIO) + 
  scale_color_manual(values = c("#0a4c8a", "#0B6E4F", "#57B8FF")) +
  geom_boxplot(show.legend = NULL) + 
  labs(y = "Relative Percent Difference", x = "Scenario") + 
  facet_wrap(~SEASON) + 
  theme_bw() + 
  theme(axis.title.x = element_text(margin = margin(10, 0, 5, 0, "pt"), size = 12), axis.title.y = element_text(margin = margin(0, 10, 0, 5, "pt"), size = 12), axis.text.y = element_text(size = 10), 
        axis.text.x = element_text(size = 10))


ggsave("spring_scenario_mudiff.png", plot = last_plot(), path = here("sdmtmb", "sumflounder", "plots"), width = 10, height = 5)



## PLOT DISTRIBUTIONS ####
all_slopes <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_AllSlopes.rds"))
### HISTOGRAM ####
# base_hist <- ggplot(base_slopes)+ 
#   geom_histogram(aes(estimate, fill = TYPE)) + 
#   facet_wrap(~TYPE)
# 
# inc_hist <- ggplot(inc_slopes)+ 
#   geom_histogram(aes(estimate, fill = TYPE)) + 
#   facet_wrap(~TYPE)
# 
# dec_hist <- ggplot(dec_slopes)+ 
#   geom_histogram(aes(estimate, fill = TYPE)) + 
#   facet_wrap(~TYPE)
# 
# (base_hist / inc_hist / dec_hist) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

all_slope_dist <- ggplot(all_slopes)+ 
  geom_histogram(aes(estimate, fill = TYPE)) + 
  scale_fill_manual(values = c("#3f7f00", "orange"))+
  #facet_wrap(~SCENARIO) +
  facet_grid(rows = vars(TYPE), cols = vars(SCENARIO), scales = "free") +
  labs(x = "Linear regression slope estimate", y = "Number of linear regression slope estimates", title = "Distribution of linear regression slope estimates of simulated spring scenarios") +
  theme_bw()+
  theme(legend.position = "bottom")

ggsave("all_spring_slope_dist.png", plot = all_slope_dist, path = here("sdmtmb", "sumflounder", "plots"), width = 10, height = 6)

### BOXPLOT ####
# ggplot(all_slopes)+ 
#   geom_boxplot(aes(SCENARIO, estimate, color = TYPE)) + 
#   scale_fill_manual(values = c("#3f7f00", "orange"))+
#   #facet_wrap(~SCENARIO) +
#   #facet_grid(rows = vars(SCENARIO), cols = vars(TYPE), scales = "free") +
#   labs(x = "Linear regression slope estimate", y = "Number of linear regression slope estimates", title = "Distribution of Linear Regression Slope Estimates of Simulated Scenarios") +
#   theme_bw()+
#   theme(legend.position = "bottom")

### POINT RANGE ####
all_slopes_summary <- all_slopes |>
  group_by(SCENARIO, TYPE) |>
  summarize(med = median(estimate),
            lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))|> 
  mutate(SEASON = "SPRING")

ggplot(all_slopes_summary) +
  aes(x = as.factor(SCENARIO), y = med, color = TYPE) +
  scale_color_manual(values = c("#3f7f00", "orange")) +
  geom_point(position = position_dodge2(width = 0.2)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width= 0.2, position = position_dodge2(width = 0.2)) + #add the bootstrap confidence interval
  #coord_flip() +
  #theme_bw() +
  facet_wrap(~SEASON) +
  labs(y = "Linear regression slope estimate",
       x = "Scenario")#, +
       #subtitle = "Changes in spring abundance indices over time in response to survey effort and productivity scenarios") + 
  theme(legend.position = "bottom", axis.title.x = element_text(vjust = -1))

ggsave("all_spring_slope_pointrange.png", plot = last_plot(), path = here("sdmtmb", "sumflounder", "plots"), width = 10, height = 6)

# base_incl.stratmu <- base_stratmu |> 
#   select(rep, incl.stratmu) |> 
#   unnest(incl.stratmu) |> 
#   mutate(TYPE = "With Wind Included") 
# 
# base_precl.stratmu <- base_stratmu |> 
#   select(rep, precl.stratmu) |> 
#   unnest(precl.stratmu) |> 
#   mutate(TYPE = "With Wind Precluded") 

# ggplot(base_precl.stratmu) +
#   aes(x = EST_YEAR, y = stratmu, group = rep, color = TYPE) +
#   geom_line(alpha = 0.3, col = gray(0.5)) +
#   theme_bw()
# 
# ggplot(all_slopes) + 
#   aes(x = SCENARIO, y = estimate, color = TYPE) + 
#   geom_boxplot()
# 
# 

# all_slopes |>
#   group_by(TYPE, SCENARIO) |>
#   median_qi(estimate, .width = c(0.25, 0.5, 0.95)) |> 
#   ggplot() +
#   aes(SCENARIO, y = estimate, ymin = .lower, ymax = .upper, color = TYPE) + 
#   geom_pointrange(position = position_dodge(width = 0.2))
#   # geom_lineribbon()+ 
#   # scale_fill_brewer() +
#   # facet_grid(rows = vars(SCENARIO))# + 
#   labs(x = "Year", y = "Simulated Total Biomass (kg)")



