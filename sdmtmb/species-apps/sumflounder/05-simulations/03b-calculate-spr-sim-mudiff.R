### created: 08/23/2023
### last updated: 

# 03b -  Calculate Mu Diff Spring Scenarios ####


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
base_all.stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseStratmuBinded.rds"))

#increase 
inc_all.stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseStratmuBinded.rds"))

#decrease
dec_all.stratmu <- readRDS(here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseStratmuBinded.rds"))


# base 
base_mudiff <- base_all.stratmu |>
  # filter(EST_YEAR %in% c(2016:2019, 2021)) |> #filter for recent 5 years, skipping 2020
  group_by(rep, EST_YEAR) |> #, GEO_AREA) |>
  summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") |> # calculate the relative differences and square them; drop the groups for further analysis
  group_by(rep) |>
  summarize(mudiff = mean(sq_diff), .groups = "drop") |> # calculate the average; drop the grouping factor 
  mutate(mudiff = sqrt(mudiff)*100, 
         SCENARIO = "BASELINE")
  #arrange(desc(mudiff))# |> # arrange highest to lowest 
 # left_join(specieslookup, by = "SVSPP") 
saveRDS(base_mudiff, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_BaseMudiff.rds"))
  
# increase 
inc_mudiff <- inc_all.stratmu |>
    # filter(EST_YEAR %in% c(2016:2019, 2021)) |> #filter for recent 5 years, skipping 2020
    group_by(rep, EST_YEAR) |> #, GEO_AREA) |>
    summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") |> # calculate the relative differences and square them; drop the groups for further analysis
    group_by(rep) |>
    summarize(mudiff = mean(sq_diff), .groups = "drop") |> # calculate the average; drop the grouping factor 
    mutate(mudiff = sqrt(mudiff)*100, 
           SCENARIO = "ENHANCED")
  #arrange(desc(mudiff))# |> # arrange highest to lowest 
  # left_join(specieslookup, by = "SVSPP") 
saveRDS(inc_mudiff, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_IncreaseMudiff.rds"))
  
# decrease
dec_mudiff <- dec_all.stratmu |>
    # filter(EST_YEAR %in% c(2016:2019, 2021)) |> #filter for recent 5 years, skipping 2020
    group_by(rep, EST_YEAR) |> #, GEO_AREA) |>
    summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") |> # calculate the relative differences and square them; drop the groups for further analysis
    group_by(rep) |>
    summarize(mudiff = mean(sq_diff), .groups = "drop") |> # calculate the average; drop the grouping factor 
    mutate(mudiff = sqrt(mudiff)*100, 
           SCENARIO = "REDUCED")
  #arrange(desc(mudiff))# |> # arrange highest to lowest 
  # left_join(specieslookup, by = "SVSPP") 
saveRDS(dec_mudiff, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_DecreaseMudiff.rds"))

all_mudiff <- bind_rows(base_mudiff, inc_mudiff, dec_mudiff) |> 
  mutate(SEASON = "SPRING")
saveRDS(all_mudiff, here("sdmtmb", "sumflounder", "data", "simulations", "SprSimFuture_AllMudiff.rds"))

# ggplot(all_mudiff) + 
#   aes(x = SCENARIO, y = mudiff) + 
#   geom_boxplot()
