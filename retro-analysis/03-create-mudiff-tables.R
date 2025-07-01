### created: 06/06/2024
### last updated: 04/25/2025

# 03 - CREATE RETROSPECTIVE ANALYSIS TABLES ####


## OBJECTIVE ####
# script will: 
# filter the mean difference data sets  
# create tables
#


### Load packages ####
library(stringr)
library(sf)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(gt)
source(here("R", "StratMeanFXs_v2.R"))

### File locations ####
dat.files <- here("data", "rds", "retro-analysis")
tab.files <- here("outputs", "tables")

### Read in data ####
# difference in stratified means created from `01-calculate-retro-indices.R` here("retro-analysis") 
mudiff <- readRDS(here(dat.files, "species_stratmu-diff.rds"))

# difference in coefficient of variation created from `01-calculate-retro-indices.R` here("retro-analysis")
cvdiff <- readRDS(here(dat.files, "species_cv-diff.rds")) 

# difference in population trends created from `01-calculate-retro-indices.R` here("retro-analysis")
linreg_diff <- readRDS(here(dat.files, "species_slope-diff.rds"))

svspp <- c(15, 32, 72, 103, 105, 106, 121, 131, 141, 503, 999)

### Data Wrangle ####
# filter for top ten species effected in the fall
mudiff10 <- mudiff |>
  group_by(SEASON) |> 
  nest() |>
  mutate(subset = map(data, ~arrange(., desc(MARE)) |> head(10))) |> 
  select(!data) |>
  unnest(cols = subset) |> 
  mutate(COMNAME = str_to_title(COMNAME), 
         SEASON = str_to_title(SEASON))

cvdiff10 <- cvdiff |>
  group_by(SEASON) |> 
  nest() |>
  mutate(subset = map(data, ~arrange(., desc(MARE)) |> head(10))) |> 
  select(!data) |>
  unnest(cols = subset) |> 
  mutate(COMNAME = str_to_title(COMNAME), 
         SEASON = str_to_title(SEASON))

LRdiff10 <- linreg_diff |>
  group_by(SEASON) |> 
  nest() |>
  mutate(subset = map(data, ~arrange(., desc(MAE)) |> head(10))) |> 
  select(!data) |>
  unnest(cols = subset) |> 
  mutate(COMNAME = str_to_title(COMNAME), 
         SEASON = str_to_title(SEASON))


# filter for top ten species effected in the spring
mudiff10_sseep <- mudiff |>
  group_by(SEASON) |> 
  nest() |>
  mutate(subset = map(data, ~filter(., SVSPP %in% svspp) |> arrange(desc(MARE)))) |>
  select(!data) |>
  unnest(cols = subset) |> 
  mutate(COMNAME = str_to_title(COMNAME), 
         SEASON = str_to_title(SEASON))

cvdiff10_sseep <- cvdiff |>
  group_by(SEASON) |> 
  nest() |>
  mutate(subset = map(data, ~filter(., SVSPP %in% svspp) |> arrange(desc(MARE)))) |>
  select(!data) |>
  unnest(cols = subset) |> 
  mutate(COMNAME = str_to_title(COMNAME), 
         SEASON = str_to_title(SEASON))

LRdiff10_sseep <- linreg_diff |>
  group_by(SEASON) |> 
  nest() |>
  mutate(subset = map(data, ~filter(., SVSPP %in% svspp) |> arrange(desc(MAE)))) |> 
  select(!data) |>
  unnest(cols = subset) |> 
  mutate(COMNAME = str_to_title(COMNAME), 
         SEASON = str_to_title(SEASON))


## Make Tables ####
mudiff.tbl <- mudiff10[,c(1,3,7)] |> 
  group_by(SEASON) |>
  gt(rowname_col = "COMNAME") |> 
  fmt_percent() |>
  cols_label(MARE = md("**Mean Absolute Relative Difference**")) |>
  tab_header(title = md("Differences in abundance indices")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold", 
              row_group.background.color = "grey95")
  
cvdiff.tbl <- cvdiff10[,c(1,3,7)] |> 
  group_by(SEASON) |>
  gt(rowname_col = "COMNAME") |> 
  fmt_percent() |>
  cols_label(MARE = md("**Mean Absolute Relative Difference**")) |>
  tab_header(title = md("Differences in survey CVs")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold", 
              row_group.background.color = "grey95")

LRdiff.tbl <- LRdiff10[,c(1,3,5)] |> 
  group_by(SEASON) |>
  gt(rowname_col = "COMNAME") |> 
  fmt_percent() |>
  cols_label(MAE = md("**Mean Absolute Difference**")) |>
  tab_header(title = md("Differences in population trends")) |> 
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold", 
              row_group.background.color = "grey95")

### Spring ####
mudiff_sseep.tbl <- mudiff10_sseep[,c(1,3,7)] |> 
  group_by(SEASON) |>
  gt(rowname_col = "COMNAME") |> 
  fmt_percent() |>
  cols_label(MARE = md("**Mean Absolute Relative Difference**")) |>
  tab_header(title = md("Differences in abundance indices")) |> 
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold", 
              row_group.background.color = "grey95")

cvdiff_sseep.tbl <- cvdiff10_sseep[,c(1,3,7)] |> 
  group_by(SEASON) |>
  gt(rowname_col = "COMNAME") |> 
  fmt_percent() |>
  cols_label(MARE = md("**Mean Absolute Relative Difference**")) |>
  tab_header(title = md("Differences in survey CVs")) |> 
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold", 
              row_group.background.color = "grey95")

LRdiff_sseep.tbl <- LRdiff10_sseep[,c(1,3,5)] |> 
  group_by(SEASON) |>
  gt(rowname_col = "COMNAME") |> 
  fmt_percent() |>
  cols_label(MAE = md("**Mean Absolute Difference**")) |>
  tab_header(title = md("Differences in population trends")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold", 
              row_group.background.color = "grey95")

## Save the data ####
tbl.data <- list("mudiff_table" = mudiff.tbl, 
                 "SSEEP_mudiff" = mudiff_sseep.tbl, 
                 "cvdiff_table" = cvdiff.tbl, 
                 "SSEEP_cvdiff" = cvdiff_sseep.tbl,
                 "slope-diff_table" = LRdiff.tbl,
                 "SSEEP_slope-diff" = LRdiff_sseep.tbl)

pmap(list(tbl.data, names(tbl.data)), ~gtsave(data = .x, filename = str_c(.y, "png", sep = "."), path = here(tab.files), expand = 10))

pmap(list(tbl.data, names(tbl.data)), ~gtsave(data = .x, filename = str_c(.y, "docx", sep = "."), path = here(tab.files), expand = 10))
