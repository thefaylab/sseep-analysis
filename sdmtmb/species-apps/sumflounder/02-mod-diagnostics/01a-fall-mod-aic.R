### created: 08/02/2023
### last updated: 04/17/2024

# 01a - FALL MODEL DIAGNOSTICS: AIC ####

## OBJECTIVE ####
# Identify AIC values for all fall models fit to summer flounder data
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
library(kableExtra)

### Environment Set Up ####
# season 
season <- "fall"

# file names that will be read in 
mod.files <- str_c("m",seq(1:12), "_", season, ".rds", sep = "")

# extract only the m# string
mod.names <- str_extract(mod.files, "[a-z]\\d+") 

### File locations ####
sumflounder.dat <- here("sdmtmb",  "sumflounder", "data")

# model locations
mod.locs <- here(sumflounder.dat, "mods", season)

## READ DATA ####
mods.list <- mod.files |>
  map(~list(.)) |>
  map(~readRDS(here(mod.locs, .)))

# mod.names <- str_c("m",seq(1:12)) #|> 
#   append(str_c("m",c(8, 11, 14:16),"a"))

## EXTRACT VALUES ####
# apply AIC function to each model
aic <- map(mods.list, ~AIC(.)) |> 
  as.data.frame(row.names = "AIC") |> 
  t()
rownames(aic) <- mod.names#seq(1:17)

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

# spatial_varying_formula <- map(mods, ~pluck(., "spatial_varying_formula")) |> 
#   as.character() |>  
#   as.data.frame() |> 
#   rename(spatial_varying = `as.character(map(mods, ~pluck(., "spatial_varying_formula")))`)

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
  # rename(formula = ...1, 
  #        time = ...4, 
  #        converged = ...5, 
  #        AIC = ...6) |> 
  mutate(#extra_time = as.integer(2020), 
         # family = "tweedie", 
         # time = ifelse(time=="EST_YEAR", time, "NULL"), 
         season = str_to_title(season)) |> 
  relocate(c(convergence, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") #|> 
 # arrange(AIC)
  
  
# save the data
saveRDS(mods_tbl, file = here(sumflounder.dat, str_c(season, "-mod-configs.rds", sep = "")))


kable(mods_tbl, align = "lcccc", caption = "Summer flounder seasonal model configurations", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) |>
row_spec(which(mods_tbl$AIC == min(mods_tbl$AIC)), color = "red") 

