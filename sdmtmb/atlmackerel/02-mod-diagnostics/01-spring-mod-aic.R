### created: 11/07/2023
### last updated: 

# 01 - SPRING MODEL DIAGNOSTICS: AIC####

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
kable(mods, align = "lcccc", caption = "Spring Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%
