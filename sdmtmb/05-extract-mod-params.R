### created: 06/06/2024
### last updated: 11/10/2024

# 05 - EXTRACT MODEL PARAMETERS ####

## OBJECTIVE ####
# extract existing model fit parameters 

### Load packages ####
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(gt)
set.seed(123)
theme_set(theme_bw())

# season 
season <- "fall"

# species
species <- "sumflounder"

species_name <- "summer flounder"

### File locations ####
dat.files <- here("data", "rds", "sdmtmb", species) #

### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, "mods", season, str_c("m10_", season, ".rds", sep = ""))) ## FIXME as needed

## Extract depth coefficients ####
x_mat <- poly(mod$data$AVGDEPTH, 2)
depth_coefs <- attr(x_mat, "coefs")


## Extract model coefficients ####
est <- tidy(mod)
# ests1 <- tidy(mod, model = 1)
# ests2 <- tidy(mod, model = 2)
est$term <- c("Depth, 1", "Depth, 2", str_c("Year:", unique(sort(mod$data$EST_YEAR)), sep = " "), "Area: Wind")

## Extract random parameters ####
ran_pars <- tidy(mod, "ran_pars")
# ran_pars1 <- tidy(mod, "ran_pars", model = 1)
# ran_pars2 <- tidy(mod, "ran_pars", model = 2)
ran_pars$term <- c("Range", "Dispersion", "Spatial SD", "Spatiotemporal SD", "Power parameter")

### Save the data ####
data <- list("mod.rds" = mod,
             "depth_matrix.rds" = x_mat,
             "depth_coefs.rds" = depth_coefs,
             "mod-ests.rds" = est,
             "mod_ran-pars.rds" = ran_pars)#,
             # "dpg-mod1-ests.rds" = ests1,
             # "dpg-mod2-ests.rds" = ests2,
             # "dpg-mod1_ran-pars.rds" = ran_pars1,
             # "dpg-mod2_ran-pars.rds" = ran_pars2)

pmap(list(data, names(data)), ~saveRDS(.x, here(dat.files, str_c(season, .y, sep = "_")))) 

## Make tables ####
est.tbl <- est |> column_to_rownames(var = "term") |>
  gt(rownames_to_stub = TRUE) |> # adds a line between covariate names and estimates 
  tab_stubhead(label = "Covariate") |> # labels the stubhead column
  fmt_number() |> # rounds numbers to 2 decimal points
  cols_label(estimate = md("**Estimate**"), 
             std.error = md("**SE**")) |>
  tab_header(title = md(str_c(str_to_title(season), str_to_title(species_name), "model coefficients", sep = " "))) |>
  tab_options(table.width = pct(40), 
              column_labels.font.weight = "bold")

rp.tbl <- ran_pars |> column_to_rownames(var = "term") |>
  gt(rownames_to_stub = TRUE) |> 
  tab_stubhead(label = "Parameter") |>
  fmt_number() |>
  tab_header(title = md(str_c("Estimates for random parameters from the", str_to_title(season), str_to_title(species_name), "model", sep = " "))) |>
  tab_options(table.width = pct(40), 
              column_labels.font.weight = "bold")


### Save tables ####
tables <- list("mod-est-tbl.png" = est.tbl, 
               "ran-pars-tbl.png"= rp.tbl)

pmap(list(tables, names(tables)), ~gtsave(.x, filename = str_c(season, .y, sep = "_"), path = here("outputs", "sdmtmb", species, "tables"), expand = 25))
