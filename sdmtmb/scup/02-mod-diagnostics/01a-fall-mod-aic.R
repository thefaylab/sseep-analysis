### created: 11/24/2023
### last updated: 12/09/2023

# 01a - FALL MODEL DIAGNOSTICS: AIC ####

## OBJECTIVE ####
# Identify AIC values for all fall models fit to scup data
# pull additional model information for ease of reference
# format information into a table

## LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)

here()


# read in models
fall.mods <- str_c("m",seq(1:8), "_fall_tw.rds") |>
  #append(str_c("m",c(8, 11, 14:16),"_fall.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "scup", "data", "mods", .)))

mod.names <- str_c("m",seq(1:8)) #|> 
  #append(str_c("m",c(8, 11, 14:16),"a"))

# apply AIC function to each model
fall.aic <- map(fall.mods, ~AIC(.)) |> 
  as.data.frame() |> 
  t()
rownames(fall.aic) <- mod.names
  
fall.form <- map(fall.mods, ~pluck(., "formula")) |> 
  as.character() |> 
  map(~str_sub(., 6, -2)) |> 
  as.data.frame() |>
  t()
rownames(fall.form) <- mod.names

spatial <- map(fall.mods, ~pluck(., "spatial")) |> 
  as.character() |>
  as.data.frame() |> 
  rename(spatial = `as.character(map(fall.mods, ~pluck(., "spatial")))`)
rownames(spatial) <- mod.names

spatiotemporal <- map(fall.mods, ~pluck(., "spatiotemporal")) |>
  as.character() |>
  as.data.frame() |>
  rename(spatiotemporal = `as.character(map(fall.mods, ~pluck(., "spatiotemporal")))`)
rownames(spatiotemporal) <- mod.names

# spatial_varying_formula <- map(fall.mods, ~pluck(., "spatial_varying_formula")) |> 
#   as.character() |>  
#   as.data.frame() |> 
#   rename(spatial_varying = `as.character(map(fall.mods, ~pluck(., "spatial_varying_formula")))`)

time <- map(fall.mods, ~pluck(., "time")) |> 
  as.data.frame() |> 
  t() 
rownames(time) <- mod.names


convergence <- map(fall.mods, ~pluck(., "sd_report", "pdHess")) |> 
  as.data.frame() |> 
  t()
rownames(convergence) <- mod.names
  
  
mods <- fall.form |> 
  bind_cols(spatial, spatiotemporal, time,  #spatial_varying_formula, 
            convergence, fall.aic) |> 
  rename(formula = ...1, 
         time = ...4, 
         converged = ...5, 
         AIC = ...6) |> 
  mutate(#extra_time = as.integer(2020), 
         family = "tweedie", 
         time = ifelse(time=="EST_YEAR", time, "NULL"), 
         season = "FALL") |> 
  relocate(c(converged, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") |> 
  arrange(AIC)
  
  
# save the data
saveRDS(mods, file = here("sdmtmb", "scup", "data", "fall-mod-configs.rds"))


kable(mods, align = "lcccc", caption = "Fall Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)# %>%
#row_spec(7, color = "red") 

