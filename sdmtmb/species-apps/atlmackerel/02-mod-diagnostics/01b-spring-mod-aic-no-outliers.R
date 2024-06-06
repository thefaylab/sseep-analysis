### created: 11/07/2023
### last updated: 12/26/2023

# 01b - SPRING MODEL DIAGNOSTICS: AIC####

## OBJECTIVE ####
# Identify AIC values for all spring models fit to atlantic mackerel data when outliers of biomass are removed from the data
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
spr.mods_no.out <- str_c("m",seq(1:12), "_spring_no.out.rds") |>
  #append(str_c("m",c(8, 11, 14:17),"_spring2.rds")) |>
  #append(str_c("m",c(15:17),"_spring3.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "atlmackerel", "data", "mods", .)))

mod_no.out.names <- str_c("m",seq(1:12)) #|> 
  #append(str_c("m",c(8, 11, 14:17),"a")) |>
  #append(str_c("m",c(15:17),"b"))

spr.aic_no.out <- map(spr.mods_no.out, ~AIC(.)) |> 
  as.data.frame() |> 
  t()
rownames(spr.aic_no.out) <- mod_no.out.names#seq(1:17)

spr.form_no.out <- map(spr.mods_no.out, ~pluck(., "formula")) |> 
  as.character() |> 
  map(~str_sub(., 6, -2)) |> 
  as.data.frame() |>
  t()
rownames(spr.form_no.out) <- mod_no.out.names

spatial_no.out <- map(spr.mods_no.out, ~pluck(., "spatial")) |> 
  as.character() |>
  as.data.frame() |> 
  rename(spatial = `as.character(map(spr.mods_no.out, ~pluck(., "spatial")))`)
rownames(spatial_no.out) <- mod_no.out.names

spatiotemporal_no.out <- map(spr.mods_no.out, ~pluck(., "spatiotemporal")) |>
  as.character() |>
  as.data.frame() |>
  rename(spatiotemporal = `as.character(map(spr.mods_no.out, ~pluck(., "spatiotemporal")))`)
rownames(spatiotemporal_no.out) <- mod_no.out.names

# spatial_varying_formula <- map(spr.mods, ~pluck(., "spatial_varying_formula")) |> 
#   as.character() |>  
#   as.data.frame() |> 
#   rename(spatial_varying = `as.character(map(spr.mods, ~pluck(., "spatial_varying_formula")))`)

time_no.out <- map(spr.mods_no.out, ~pluck(., "time")) |> 
  as.data.frame() |> 
  t() 
rownames(time_no.out) <- mod_no.out.names


convergence_no.out <- map(spr.mods_no.out, ~pluck(., "sd_report", "pdHess")) |> 
  as.data.frame() |> 
  t()
rownames(convergence_no.out) <- mod_no.out.names


mods_no.out <- spr.form_no.out |> 
  bind_cols(spatial_no.out, spatiotemporal_no.out, time_no.out, #spatial_varying_formula, 
            convergence_no.out, spr.aic_no.out) |> 
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
saveRDS(mods_no.out, file = here("sdmtmb", "atlmackerel", "data", "spr-mod_no.out_configs.rds"))
# 
# 
kable(mods_no.out, align = "lcccc", caption = "Atlantic mackerel spring model configurations with outliers removed", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%


## DELTA/POISSON/GAMMA MODELS ####
spr_dpg.mods_no.out <- str_c("m",seq(1:12), "_spring_no.out.dpg.rds") |>
  #append(str_c("m",c(8, 11, 14:17),"_spring2.rds")) |>
  #append(str_c("m",c(15:17),"_spring3.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "atlmackerel", "data", "mods", .)))

dpg_mod_no.out.names <- str_c("m",seq(1:12)) #|> 
#append(str_c("m",c(8, 11, 14:17),"a")) |>
#append(str_c("m",c(15:17),"b"))

spr_dpg.aic_no.out <- map(spr_dpg.mods_no.out, ~AIC(.)) |> 
  as.data.frame() |> 
  t()
rownames(spr_dpg.aic_no.out) <- dpg_mod_no.out.names#seq(1:17)

spr_dpg.form_no.out <- map(spr_dpg.mods_no.out, ~pluck(., "formula", 1)) |> 
  as.character() |> 
  #map(~str_sub(., 6, -2)) |> 
  as.data.frame() |>
  rename(formula = `as.character(map(spr_dpg.mods_no.out, ~pluck(., "formula", 1)))`)
rownames(spr_dpg.form_no.out) <- dpg_mod_no.out.names

dpg.spatial_no.out <- map(spr_dpg.mods_no.out, ~pluck(., "spatial", 1)) |> 
  as.character() |>
  as.data.frame() |> 
  rename(spatial = `as.character(map(spr_dpg.mods_no.out, ~pluck(., "spatial", 1)))`)
rownames(dpg.spatial_no.out) <- dpg_mod_no.out.names

dpg.spatiotemporal_no.out <- map(spr_dpg.mods_no.out, ~pluck(., "spatiotemporal", 1)) |>
  as.character() |>
  as.data.frame() |>
  rename(spatiotemporal = `as.character(map(spr_dpg.mods_no.out, ~pluck(., "spatiotemporal", 1)))`)
rownames(dpg.spatiotemporal_no.out) <- dpg_mod_no.out.names

# spatial_varying_formula <- map(spr.mods, ~pluck(., "spatial_varying_formula")) |> 
#   as.character() |>  
#   as.data.frame() |> 
#   rename(spatial_varying = `as.character(map(spr.mods, ~pluck(., "spatial_varying_formula")))`)

dpg.time_no.out <- map(spr_dpg.mods_no.out, ~pluck(., "time")) |> 
  as.data.frame() |> 
  t() 
rownames(dpg.time_no.out) <- dpg_mod_no.out.names


dpg_convergence_no.out <- map(spr_dpg.mods_no.out, ~pluck(., "sd_report", "pdHess")) |> 
  as.data.frame() |> 
  t()
rownames(dpg_convergence_no.out) <- dpg_mod_no.out.names


dpg_mods_no.out <- spr_dpg.form_no.out |> 
  bind_cols(dpg.spatial_no.out, dpg.spatiotemporal_no.out, dpg.time_no.out, #spatial_varying_formula, 
            dpg_convergence_no.out, spr_dpg.aic_no.out) |> 
  rename(time = ...4, 
         converged = ...5, 
         AIC = ...6) |> 
  mutate(family = "delta possion link gamma", 
         time = ifelse(time=="EST_YEAR", time, "NULL"), 
         season = "SPRING") |> 
  relocate(c(converged, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") |> 
  arrange(AIC)

### save the data ####
saveRDS(dpg_mods_no.out, file = here("sdmtmb", "atlmackerel", "data", "spr-dpg-mod-configs_no.outliers.rds"))
# 
# 
kable(dpg_mods_no.out, align = "lcccc", caption = "Atlantic mackerel spring model configurations with outliers removed", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%
