### created: 04/24/2024
### last updated: 05/20/2024

# 01 - DIAGNOSTICS: AIC ####

## OBJECTIVE ####
# Identify AIC values for model fits 
# pull additional model information for ease of reference
# format information into a table

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
# library(kableExtra)
library(gt)

### Environment Set Up ####
# season 
season <- "fall"

# species
species <- "sumflounder"

species_name <- "summer flounder"

### File locations ####
dat.files <- here("sdmtmb",  species, "data") ## FIXME when repo is reorganized

# model locations
mod.locs <- here(dat.files, "mods", season) ## FIXME when repo is reogranized 


### Read in data ####
# number of models to read in 
mod.num <- length(list.files(mod.locs, pattern = "[a-z]\\d+")) # find files in mod.locs that contain a pattern related to the model file names only; excludes folders. 
# this is used here rather than assigning list.files(mod.locs) into mod.files (below), because models numbered in the teens are listed between model 1 and 2 due to folder organization. 

# file names that will be read in 
mod.files <- str_c("m",seq(mod.num), "_", season, ".rds", sep = "")

# extract only the m# string
mod.names <- str_c("m",seq(mod.num)) 

# read in the data 
mods.list <- mod.files |>
  map(~list(.)) |>
  map(~readRDS(here(mod.locs, .)))

## EXTRACT VALUES ####
# apply AIC function to each model
aic <- map(mods.list, ~AIC(.)) |> 
  as.data.frame(row.names = "AIC") |> 
  t()
rownames(aic) <- mod.names

### FORMULA ####
# extract the formulas used for the model fit
form <- map(mods.list, ~pluck(., "formula")) |> 
  as.character() |> 
  map(~str_sub(., 6, -2)) |> 
  as.data.frame(row.names = "formula") |>
  t()
rownames(form) <- mod.names

### SPATIAL REs ####
# extract whether spatial random effects were estimated in the model fit
spatial <- map(mods.list, ~pluck(., "spatial")) |> 
  as.character() |>
  as.data.frame() |> 
  rename(spatial = `as.character(map(mods.list, ~pluck(., "spatial")))`) |> 
  mutate(spatial = case_when(spatial=="on" ~ str_to_title(spatial), TRUE ~"-"))
rownames(spatial) <- mod.names

### SPATIOTEMPORAL REs ####
# extract whether spatiotemporal random effects were estimated in the model fit
spatiotemporal <- map(mods.list, ~pluck(., "spatiotemporal")) |>
  as.character() |>
  as.data.frame() |>
  rename(spatiotemporal = `as.character(map(mods.list, ~pluck(., "spatiotemporal")))`)  |> 
  mutate(spatiotemporal = case_when(spatiotemporal=="iid" ~ str_to_upper(spatiotemporal), TRUE ~"-"))
rownames(spatiotemporal) <- mod.names


### TIME ####
# extract which time variable was used to estimate spatiotemporal random effects in the model fit
time <- map(mods.list, ~pluck(., "time")) |> 
  as.data.frame(row.names = "time") |> 
  t() |> as.data.frame() |> mutate(time = case_when(time=="EST_YEAR" ~ "Year", TRUE ~"-"))
rownames(time) <- mod.names

### CONVERGENCE ####
# extract whether the model fit converged
conv <- map(mods.list, ~pluck(., "sd_report", "pdHess")) |> 
  as.data.frame(row.names = "convergence") |> 
  t()|> as.data.frame() |> mutate(convergence = str_to_title(convergence))
rownames(conv) <- mod.names

### FAMILY ####
# set the observation family used to fit the model 
fam <- map(mods.list, ~pluck(., "family", "family")) |>
  as.data.frame(row.names = "family") |>
  t() |> as.data.frame() |> mutate(family = str_to_title(family))
rownames(fam) <- mod.names

## CREATE CONFIGURATIONS TABLE ####
mods_tbl <- form |> 
  bind_cols(spatial, spatiotemporal, time,  #spatial_varying_formula, 
            conv, aic, fam) |> 
  mutate(season = str_to_title(season)) |> 
  relocate(c(convergence, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") 


# save the data
saveRDS(mods_tbl, file = here(dat.files, str_c(season, "-mod-configs.rds", sep = ""))) ## FIXME when repo is reorganized 


# kable(mods_tbl, align = "lcccc", caption = str_c(str_to_sentence(species_name), "seasonal model configurations", sep = ""), format.args = list(big.mark = ","), booktabs = TRUE) |>
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) |>
#   row_spec(which(mods_tbl$AIC == min(mods_tbl$AIC)), color = "red") 

mods_gt <- mods_tbl |> column_to_rownames(var = "models") |>
  gt(rownames_to_stub = TRUE) |> # adds a line between model names and estimates 
  tab_stubhead(label = md("**Models**")) |> # labels the stubhead column
  fmt_number() |> # rounds numbers to 2 decimal points
  cols_label(formula = md("**Formula**"), 
             spatial = md("**Spatial random fields**"),
             spatiotemporal = md("**Spatiotemporal random fields**"),
             time = md("**Time**"),
             family = md("**Family**"),
             season = md("**Season**"), 
             convergence = md("**Convergence**")) |>
  tab_header(title = md("Fall summer flounder model configurations")) |>
  tab_options(table.width = pct(85), 
              column_labels.font.weight = "bold")

gtsave(mods_gt, filename = "fall_mod_aic_tbl.png", path = here("sdmtmb", "sumflounder", "tables"), expand = 100)
