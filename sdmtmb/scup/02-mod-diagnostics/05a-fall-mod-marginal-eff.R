
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(ggeffects)
here()

## LOAD DATA ####
# scup data 
scup_fall <- readRDS(here("sdmtmb", "scup", "data", "scup_fall.rds")) |> 
  mutate(AREA = as.factor(AREA), 
         EST_YEAR = as.factor(EST_YEAR))

# best models
m_fall_tw <- readRDS(here("sdmtmb", "scup", "data","mods","comps", "m7_fall_tw.rds"))


ggpredict(m_fall_tw, "AVGDEPTH [0:150 by=1]") %>% plot()
ggpredict(m_fall_tw, "EST_YEAR") %>% plot()

depcov_tw <- ggpredict(m_fall_tw, terms = c("AVGDEPTH [0:150 by=1]", "EST_YEAR")) 
ggplot(depcov_tw, aes(x, predicted, colour = group)) +
  geom_line()

