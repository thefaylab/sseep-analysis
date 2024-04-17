### created: 11/07/2023
### last updated: 04/11/2024

# 01a - SPRING MODEL DIAGNOSTICS: AIC####

## OBJECTIVE ####
# Identify AIC values for all spring models fit to atlantic mackerel data
# pull additional model information for ease of reference
# format information into a table

### Load Packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)

here()

### Environment set up #### 
# folder location based on data set used: c("all-dat", "no-bio-out", "no-dep-out")
dat_set <- "no-dep-out"

# data description: c("All observations", "Biomass outliers removed", "Depth outliers removed")
data_desc <- "Depth outliers removed"

# model family folder: c("tweedie", "dpg")
fam_file <- "tweedie"

# model family description: c("Tweedie", "Delta Gamma")
fam_desc <- "Tweedie"

# file names that will be read in 
mod.files <- str_c("m",seq(1:14), "_spring.rds")

# extract only the m# string
mod.names <- str_extract(mod.files, "[a-z]\\d+") 

### File Locations 
# species folder 
atlmack.dat <- here("sdmtmb", "atlmackerel", "data")

# model locations
mod.locs <- here(atlmack.dat, "mods", fam_file, dat_set)
# dpg.mod.locs <- here("sdmtmb", "atlmackerel", "data", "mods", "dpg", "all-dat")

## READ DATA ####
# create a list of all the model files read in from the folder 
mods.list <- mod.files |>
  map(~list(.)) |>
  map(~readRDS(here(mod.locs, .)))

## EXTRACT VALUES ####

### AIC ####
# extract the aic values for the model fit
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
  rename(spatiotemporal = `as.character(map(mods.list, ~pluck(., "spatiotemporal")))`) |> 
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
  t() |> as.data.frame() |> mutate(convergence = str_to_title(convergence))
rownames(conv) <- mod.names

### FAMILY ####
# set the observation family used to fit the model 
# fam <- map(mods.list, ~pluck(., "family", "family")) |> 
#   as.data.frame(row.names = "family") |> 
#   t() |> as.data.frame() |> mutate(family = str_to_title(family))
# rownames(fam) <- mod.names
fam <- rep(fam_desc, length(mod.names)) |> 
  as.data.frame() |> 
  rename(family = `rep(fam_desc, length(mod.names))`)
rownames(fam) <- mod.names


mods_tbl <- form |> 
  bind_cols(spatial, spatiotemporal, time, #spatial_varying_formula, 
            conv, aic, fam) |> 
  mutate(#family = "tweedie",
         # time = case_when(time=="EST_YEAR" ~ "Year", time, "-"), 
         season = "SPRING", 
         data = data_desc) |> 
  relocate(c(convergence, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") #|>
  # arrange(AIC)

## SAVE THE DATA ####
saveRDS(mods_tbl, file = here(atlmack.dat, str_c(str_c(fam_file, "mod-configs", dat_set, sep = "_"), ".rds", sep = "")))
# 
# 
kable(mods_tbl, align = "lcccc", caption = "Atlantic Mackerel Spring Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) |>
kable_styling(full_width = F, fixed_thead = T, font_size = 14) |>
  row_spec(which(mods_tbl$AIC == min(mods_tbl$AIC)), color = "red") 


## DELTA GAMMA MODELS ###
# dpg.mods <- spr.mods[-11] |> # remove m11 file since was unable to converge in model fitting script. Revisit once troubleshot  
#   map(~list(.)) |>
#   map(~readRDS(here(dpg.mod.locs, .)))
# 
# dpg_mod.names <- str_extract(spr.mods[-11], "[a-z]\\d+") # remove m11 file since was unable to converge in model fitting script. Revisit once troubleshot
# 
# #|> 
# #append(str_c("m",c(8, 11, 14:17),"a")) |>
# #append(str_c("m",c(15:17),"b"))
# 
# dpg.aic <- map(dpg.mods, ~AIC(.)) |> 
#   as.data.frame(row.names = "AIC") |> 
#   t()
# rownames(dpg.aic) <- dpg_mod.names#seq(1:17)
# 
# dpg.form <- map(dpg.mods, ~pluck(., "formula", 1)) |> 
#   as.character() |> 
#   #map(~str_sub(., 6, -2)) |> 
#   as.data.frame() |>
#   rename(formula = `as.character(map(dpg.mods, ~pluck(., "formula", 1)))`)
# rownames(dpg.form) <- dpg_mod.names
# 
# dpg.sre <- map(dpg.mods, ~pluck(., "spatial", 1)) |> 
#   as.character() |>
#   as.data.frame() |> 
#   rename(spatial = `as.character(map(dpg.mods, ~pluck(., "spatial", 1)))`) |> 
#   mutate(spatial = case_when(spatial=="on" ~ str_to_title(spatial), TRUE ~"-")) #|> 
#   #mutate(mod2_spatial = map(spr_dpg.mods, ~pluck(., "spatial", 2)))#, 
#   
# rownames(dpg.sre) <- dpg_mod.names
# 
# # dpg.fam <- map(dpg.mods, ~pluck(., "family", "family", 1)) |> 
# #   as.data.frame(row.names = "family") |> 
# #   t() |> as.data.frame() |> mutate(family = "Binomial Gamma")
# 
# dpg.stre <- map(dpg.mods, ~pluck(., "spatiotemporal", 1)) |>
#   as.character() |>
#   as.data.frame() |>
#   rename(spatiotemporal = `as.character(map(dpg.mods, ~pluck(., "spatiotemporal", 1)))`) |> 
#   mutate(spatiotemporal = case_when(spatiotemporal =="iid" ~ str_to_upper(spatiotemporal), TRUE ~"-"))
# rownames(dpg.stre) <- dpg_mod.names
# 
# # spatial_varying_formula <- map(spr.mods, ~pluck(., "spatial_varying_formula")) |> 
# #   as.character() |>  
# #   as.data.frame() |> 
# #   rename(spatial_varying = `as.character(map(spr.mods, ~pluck(., "spatial_varying_formula")))`)
# 
# dpg.time <- map(dpg.mods, ~pluck(., "time")) |> 
#   as.data.frame(row.names = "time") |> 
#   t() |> as.data.frame() |> mutate(time = case_when(time=="EST_YEAR" ~ "Year", TRUE ~"-"))
# rownames(dpg.time) <- dpg_mod.names
# 
# 
# dpg.conv <- map(dpg.mods, ~pluck(., "sd_report", "pdHess")) |> 
#   as.data.frame(row.names = "convergence") |> 
#   t() |> as.data.frame() |> mutate(convergence = str_to_title(convergence))
# rownames(dpg.conv) <- dpg_mod.names
# 
# 
# dpg_mods <- dpg.form |> 
#   bind_cols(dpg.sre, dpg.stre, dpg.time, #spatial_varying_formula, 
#             dpg.conv, dpg.aic) |> 
#   # rename(time = ...4, 
#   #        converged = ...5, 
#   #        AIC = ...6) |> 
#   mutate(family = "Delta Gamma", 
#          # time = ifelse(time=="EST_YEAR", time, "NULL"), 
#          season = "SPRING", 
#          data = "All Observations") |> 
#   relocate(c(convergence, AIC), .after = everything()) |> 
#   rownames_to_column(var = "models") #|>
#   #arrange(AIC)
# 
# ### save the data ###
# saveRDS(dpg_mods, file = here(altmack.dat, "dpg-mod-configs_all-dat.rds"))
# 
# 
# kable(dpg_mods, align = "lcccc", caption = "Spring Atlantic mackerel model configurations", format.args = list(big.mark = ","), booktabs = TRUE) |>
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%

