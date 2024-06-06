### created: 06/06/2024
### last updated: 

# 03 - CREATE RETROSPECTIVE ANALYSIS TABLES ####


## OBJECTIVE ####
# create tables of top ten impacts by species 
#
# script will: 
# filter the mean difference data sets and create tables 
# 
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


### Data Wrangle ####
# filter for top ten species effected in the fall
mudiff10_fall <- mudiff |>
  filter(SEASON == "FALL") |>
  arrange(desc(MARE_perc)) |> 
  head(10) |> 
  mutate(MARE_perc = round(MARE_perc,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = MARE_perc) 

cvdiff10_fall <- cvdiff |>
  filter(SEASON == "FALL") |>
  arrange(desc(MARE_perc)) |> 
  head(10) |> 
  mutate(MARE_perc = round(MARE_perc,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = MARE_perc)

LRdiff10_fall <- linreg_diff |>
  filter(SEASON == "FALL") |>
  arrange(desc(MARE_perc)) |> 
  head(10) |> 
  mutate(MARE_perc = round(MARE_perc,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = MARE_perc)


# filter for top ten species effected in the spring
mudiff10_spring <- mudiff |>
  filter(SEASON == "SPRING") |>
  arrange(desc(MARE_perc)) |> 
  head(10) |> 
  mutate(MARE_perc = round(MARE_perc,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = MARE_perc)

cvdiff10_spring <- cvdiff |>
  filter(SEASON == "SPRING") |>
  arrange(desc(MARE_perc)) |> 
  head(10) |> 
  mutate(MARE_perc = round(MARE_perc,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = MARE_perc)

LRdiff10_spring <- linreg_diff |>
  filter(SEASON == "SPRING") |>
  arrange(desc(MARE_perc)) |> 
  head(10) |> 
  mutate(MARE_perc = round(MARE_perc,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = MARE_perc)


## Make Tables ####
### Fall ####
mudiff_fall.tbl <- mudiff10_fall[,c(3,6)] |> 
  column_to_rownames(var = "Species") |>
  gt(rownames_to_stub = TRUE) |> 
  tab_header(title = md("The largest differences in fall abundance indices")) |>
  tab_stubhead(label = md("**Species**")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold")
  
cvdiff_fall.tbl <- cvdiff10_fall[,c(3,6)] |> 
  column_to_rownames(var = "Species") |>
  gt(rownames_to_stub = TRUE) |> 
  tab_header(title = md("The largest differences in fall survey CVs")) |>
  tab_stubhead(label = md("**Species**")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold")

LRdiff_fall.tbl <- LRdiff10_fall[,c(3,6)] |> 
  column_to_rownames(var = "Species") |>
  gt(rownames_to_stub = TRUE) |> 
  tab_header(title = md("The largest differences in fall population trends")) |>
  tab_stubhead(label = md("**Species**")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold")

### Spring ####
mudiff_spr.tbl <- mudiff10_spring[,c(3,6)] |> 
  column_to_rownames(var = "Species") |>
  gt(rownames_to_stub = TRUE) |> 
  tab_header(title = md("The largest differences in spring abundance indices")) |>
  tab_stubhead(label = md("**Species**")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold")

cvdiff_spr.tbl <- cvdiff10_spring[,c(3,6)] |> 
  column_to_rownames(var = "Species") |>
  gt(rownames_to_stub = TRUE) |> 
  tab_header(title = md("The largest differences in spring survey CVs")) |>
  tab_stubhead(label = md("**Species**")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold")

LRdiff_spr.tbl <- LRdiff10_spring[,c(3,6)] |> 
  column_to_rownames(var = "Species") |>
  gt(rownames_to_stub = TRUE) |> 
  tab_header(title = md("The largest differences in spring population trends")) |>
  tab_stubhead(label = md("**Species**")) |>
  cols_width(everything() ~ px(200)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold")

## Save the data ####
tbl.data <- list("fall_mudiff.png" = mudiff_fall.tbl, 
                 "spring_mudiff.png" = mudiff_spr.tbl, 
                 "fall_cvdiff.png" = cvdiff_fall.tbl, 
                 "spring_cvdiff.png" = cvdiff_spr.tbl,
                 "fall_slope-diff.png" = LRdiff_fall.tbl,
                 "spring_slope-diff.png" = LRdiff_spr.tbl)

pmap(list(tbl.data, names(tbl.data)), ~gtsave(data = .x, filename = .y, path = here(tab.files), expand = 10))
