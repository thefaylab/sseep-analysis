### created: 08/23/2023
### last updated: 

# 03a -  Calculate Mu Diff Fall Scenarios ####


## OBJECTIVE ####
# 


### LOAD PACKAGES ####
# library(stringr)
# library(sf)
# library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 

# sdmtmb.dir <- "../sseep-analysis/sdmtmb"
# sseep.dir <- "../sseep-analysis"
source(here("R", "StratMeanFXs_v2.R"))
#set.seed(123)


### LOAD DATA ####
# base 
base_all.stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "0828-fall", "FallSimFuture_BaseStratmuBinded.rds"))

#increase 
inc_all.stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "0828-fall", "FallSimFuture_IncreaseStratmuBinded.rds"))

#decrease
dec_all.stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "0828-fall", "FallSimFuture_DecreaseStratmuBinded.rds"))


# base 
base_mudiff <- base_all.stratmu %>%
  # filter(EST_YEAR %in% c(2016:2019, 2021)) %>% #filter for recent 5 years, skipping 2020
  group_by(rep, EST_YEAR) %>% #, GEO_AREA) %>%
  summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") %>% # calculate the relative differences and square them; drop the groups for further analysis
  group_by(rep) %>%
  summarize(mudiff = mean(sq_diff), .groups = "drop") %>% # calculate the average; drop the grouping factor 
  mutate(mudiff = sqrt(mudiff)*100, 
         SCENARIO = "BASELINE")
  #arrange(desc(mudiff))# %>% # arrange highest to lowest 
 # left_join(specieslookup, by = "SVSPP") 
saveRDS(base_mudiff, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_BaseMudiff.rds"))
  
# increase 
inc_mudiff <- inc_all.stratmu %>%
  # filter(EST_YEAR %in% c(2016:2019, 2021)) %>% #filter for recent 5 years, skipping 2020
  group_by(rep, EST_YEAR) %>% #, GEO_AREA) %>%
  summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") %>% # calculate the relative differences and square them; drop the groups for further analysis
  group_by(rep) %>%
  summarize(mudiff = mean(sq_diff), .groups = "drop") %>% # calculate the average; drop the grouping factor 
  mutate(mudiff = sqrt(mudiff)*100, 
         SCENARIO = "ENHANCED")
  #arrange(desc(mudiff))# %>% # arrange highest to lowest 
  # left_join(specieslookup, by = "SVSPP") 
saveRDS(inc_mudiff, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_IncreaseMudiff.rds"))
  
# decrease
dec_mudiff <- dec_all.stratmu %>%
    # filter(EST_YEAR %in% c(2016:2019, 2021)) %>% #filter for recent 5 years, skipping 2020
    group_by(rep, EST_YEAR) %>% #, GEO_AREA) %>%
    summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") %>% # calculate the relative differences and square them; drop the groups for further analysis
    group_by(rep) %>%
    summarize(mudiff = mean(sq_diff), .groups = "drop") %>% # calculate the average; drop the grouping factor 
    mutate(mudiff = sqrt(mudiff)*100, 
           SCENARIO = "REDUCED")
  #arrange(desc(mudiff))# %>% # arrange highest to lowest 
  # left_join(specieslookup, by = "SVSPP") 
saveRDS(dec_mudiff, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_DecreaseMudiff.rds"))

all_mudiff <- bind_rows(base_mudiff, inc_mudiff, dec_mudiff) |> 
  mutate(SEASON = "FALL")

all_mudiff_summary <- all_mudiff |> 
  group_by(SCENARIO) |> 
  summarise(med = median(mudiff), 
            lower = quantile(mudiff, 0.025), 
            upper = quantile(mudiff, 0.975))

saveRDS(all_mudiff, here("sdmtmb", "sumflounder", "data", "simulations", "FallSimFuture_AllMudiff.rds"))

# ggplot(all_mudiff) +
#   aes(x = SCENARIO, y = mudiff) +
#   geom_boxplot()


# BETWEEN SCENARIOS
base_all.stratmu$SCENARIO <- "BASELINE"
inc_all.stratmu$SCENARIO <- "ENHANCED"
dec_all.stratmu$SCENARIO <- "REDUCED"

base.inc_mudiff <- base_all.stratmu |>
  bind_rows(inc_all.stratmu) |> 
  group_by(rep, EST_YEAR, TYPE) |>
  summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") %>% # calculate the relative differences and square them; drop the groups for further analysis
  group_by(rep, TYPE) %>%
  summarize(mudiff = mean(sq_diff), .groups = "drop") %>% # calculate the average; drop the grouping factor 
  mutate(mudiff = sqrt(mudiff)*100, 
         COMPARE = "BASELINE-ENHANCED")

base.dec_mudiff <- base_all.stratmu |>
  bind_rows(dec_all.stratmu) |> 
  group_by(rep, EST_YEAR, TYPE) |>
  summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") %>% # calculate the relative differences and square them; drop the groups for further analysis
  group_by(rep, TYPE) %>%
  summarize(mudiff = mean(sq_diff), .groups = "drop") %>% # calculate the average; drop the grouping factor 
  mutate(mudiff = sqrt(mudiff)*100, 
         COMPARE = "BASELINE-REDUCED")
