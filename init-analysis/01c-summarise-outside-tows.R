### created:      
### last update:  07/11/2023
###


### LOAD PACKAGES ###
library(stringr)
library(patchwork)
library(here)
library(tidyverse)

here()

### LOAD DATA ###
data <- readRDS(here("data", "rds", "merged_data_complete.rds")) |>
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT), 
         CODE = str_c(STRATUM, CRUISE6, STATION)) # fills expcatch wt values with 0 if missing

wind <- filter(data, AREA == "WIND")

outside <- filter(data, AREA == "OUTSIDE")


