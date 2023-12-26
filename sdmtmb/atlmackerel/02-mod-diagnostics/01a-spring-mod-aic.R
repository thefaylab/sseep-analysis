### created: 11/07/2023
### last updated: 12/13/2023

# 01a - SPRING MODEL DIAGNOSTICS: AIC####

## OBJECTIVE ####
# Identify AIC values for all spring models fit to atlantic mackerel data
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

## TWEEDIE MODELS ####
spr.mods <- str_c("m",seq(1:12), "_spring.rds") |>
  #append(str_c("m",c(8, 11, 14:17),"_spring2.rds")) |>
  #append(str_c("m",c(15:17),"_spring3.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "atlmackerel", "data", "mods", .)))

mod.names <- str_c("m",seq(1:12)) #|> 
  #append(str_c("m",c(8, 11, 14:17),"a")) |>
  #append(str_c("m",c(15:17),"b"))

spr.aic <- map(spr.mods, ~AIC(.)) |> 
  as.data.frame() |> 
  t()
rownames(spr.aic) <- mod.names#seq(1:17)

spr.form <- map(spr.mods, ~pluck(., "formula")) |> 
  as.character() |> 
  map(~str_sub(., 6, -2)) |> 
  as.data.frame() |>
  t()
rownames(spr.form) <- mod.names

spatial <- map(spr.mods, ~pluck(., "spatial")) |> 
  as.character() |>
  as.data.frame() |> 
  rename(spatial = `as.character(map(spr.mods, ~pluck(., "spatial")))`)
rownames(spatial) <- mod.names

spatiotemporal <- map(spr.mods, ~pluck(., "spatiotemporal")) |>
  as.character() |>
  as.data.frame() |>
  rename(spatiotemporal = `as.character(map(spr.mods, ~pluck(., "spatiotemporal")))`)
rownames(spatiotemporal) <- mod.names

# spatial_varying_formula <- map(spr.mods, ~pluck(., "spatial_varying_formula")) |> 
#   as.character() |>  
#   as.data.frame() |> 
#   rename(spatial_varying = `as.character(map(spr.mods, ~pluck(., "spatial_varying_formula")))`)

time <- map(spr.mods, ~pluck(., "time")) |> 
  as.data.frame() |> 
  t() 
rownames(time) <- mod.names


convergence <- map(spr.mods, ~pluck(., "sd_report", "pdHess")) |> 
  as.data.frame() |> 
  t()
rownames(convergence) <- mod.names


mods <- spr.form |> 
  bind_cols(spatial, spatiotemporal, time, #spatial_varying_formula, 
            convergence, spr.aic) |> 
  rename(formula = ...1, 
         time = ...4, 
         converged = ...5, 
         AIC = ...6) |> 
  mutate(family = "tweedie", 
         time = ifelse(time=="EST_YEAR", time, "NULL"), 
         season = "SPRING") |> 
  relocate(c(converged, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") |> 
  arrange(AIC)

### save the data ####
saveRDS(mods, file = here("sdmtmb", "atlmackerel", "data", "spr-mod-configs.rds"))
# 
# 
kable(mods, align = "lcccc", caption = "Atlantic Mackerel Spring Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%


## DELTA/POISSON/GAMMA MODELS ####
spr_dpg.mods <- str_c("m",seq(1:12), "_spring_dpg.rds") |>
  #append(str_c("m",c(8, 11, 14:17),"_spring2.rds")) |>
  #append(str_c("m",c(15:17),"_spring3.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "atlmackerel", "data", "mods", .)))

dpg_mod.names <- str_c("m",seq(1:12)) #|> 
#append(str_c("m",c(8, 11, 14:17),"a")) |>
#append(str_c("m",c(15:17),"b"))

spr_dpg.aic <- map(spr_dpg.mods, ~AIC(.)) |> 
  as.data.frame() |> 
  t()
rownames(spr_dpg.aic) <- dpg_mod.names#seq(1:17)

spr_dpg.form <- map(spr_dpg.mods, ~pluck(., "formula", 1)) |> 
  as.character() |> 
  #map(~str_sub(., 6, -2)) |> 
  as.data.frame() |>
  rename(formula = `as.character(map(spr_dpg.mods, ~pluck(., "formula", 1)))`)
rownames(spr_dpg.form) <- dpg_mod.names

dpg.spatial <- map(spr_dpg.mods, ~pluck(., "spatial", 1)) |> 
  as.character() |>
  as.data.frame() |> 
  rename(spatial = `as.character(map(spr_dpg.mods, ~pluck(., "spatial", 1)))`) #|> 
  #mutate(mod2_spatial = map(spr_dpg.mods, ~pluck(., "spatial", 2)))#, 
  
rownames(dpg.spatial) <- dpg_mod.names

dpg.spatiotemporal <- map(spr_dpg.mods, ~pluck(., "spatiotemporal", 1)) |>
  as.character() |>
  as.data.frame() |>
  rename(spatiotemporal = `as.character(map(spr_dpg.mods, ~pluck(., "spatiotemporal", 1)))`)
rownames(dpg.spatiotemporal) <- dpg_mod.names

# spatial_varying_formula <- map(spr.mods, ~pluck(., "spatial_varying_formula")) |> 
#   as.character() |>  
#   as.data.frame() |> 
#   rename(spatial_varying = `as.character(map(spr.mods, ~pluck(., "spatial_varying_formula")))`)

dpg.time <- map(spr_dpg.mods, ~pluck(., "time")) |> 
  as.data.frame() |> 
  t() 
rownames(dpg.time) <- dpg_mod.names


dpg_convergence <- map(spr_dpg.mods, ~pluck(., "sd_report", "pdHess")) |> 
  as.data.frame() |> 
  t()
rownames(dpg_convergence) <- dpg_mod.names


dpg_mods <- spr_dpg.form |> 
  bind_cols(dpg.spatial, dpg.spatiotemporal, dpg.time, #spatial_varying_formula, 
            dpg_convergence, spr_dpg.aic) |> 
  rename(time = ...4, 
         converged = ...5, 
         AIC = ...6) |> 
  mutate(family = "delta poisson link gamma", 
         time = ifelse(time=="EST_YEAR", time, "NULL"), 
         season = "SPRING") |> 
  relocate(c(converged, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") |> 
  arrange(AIC)

### save the data ####
saveRDS(dpg_mods, file = here("sdmtmb", "atlmackerel", "data", "spr-dpg-mod-configs.rds"))
# 
# 
kable(dpg_mods, align = "lcccc", caption = "Spring Atlantic mackerel model configurations", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%

