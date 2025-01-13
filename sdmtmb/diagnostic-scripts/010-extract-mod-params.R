### created: 05/29/2024
### last updated: 

# 10 - DIAGNOSTICS: EXTRACT MODEL PARAMETERS ####

## OBJECTIVE ####
# extract model fit parameters 

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sdmTMB)
library(gt)

set.seed(123)
theme_set(theme_bw())

### Environment Set Up ####
# season 
season <- "fall"

# species
species <- "sumflounder"

species_name <- "summer flounder"

### File locations ####
dat.files <- here("sdmtmb",  species, "data") ## FIXME when repo is reorganized
tab.files <- here("sdmtmb",  species, "tables") ## FIXME when repo is reorganized
# plot.files <- here("sdmtmb", species, "plots", "post-check") ## FIXME when repo is reorganized

### Read in data ####
# best fit model according to AIC or other model selection
mod <- readRDS(here(dat.files, "mods", season, str_c("m10_", season, ".rds", sep = ""))) ## FIXME as needed
# mod <- readRDS(here(dat.files, "mods", "dpg", "no-dep-out", str_c("m4_", season, ".rds", sep = "")))


# estimates
ests <- tidy(mod) |> 
  rename(Covariate = term, 
         Coefficient = estimate, 
         SE = std.error)
est$Covariate <- c("Depth, 1", "Depth, 2", str_c("Year", unique(sort(mod$data$EST_YEAR)), sep = ": ")) ## FIXME as needed


# random parameters
ran_pars <- tidy(mod, "ran_pars") |> 
  rename(Parameter = term, 
         Estimate = estimate, 
         SE = std.error)
ran_pars$Parameter <- c("Shared Range", "Phi", "Spatial SD", "Spatiotemporal SD", "Tweedie power parameter") ## FIXME as needed

### save the data
data <- list("mod_ests.rds" = ests, 
             "ran_pars.rds" = ran_pars)

pmap(list(data, names(data)), ~saveRDS(.x, here(dat.files, str_c(season, .y, sep = "_"))))


### Tables 
est.tbl <- ests |> column_to_rownames(var = "Covariate") |>
  gt(rownames_to_stub = TRUE) |> # adds a line between covariate names and estimates 
  tab_stubhead(label = md("**Covariate**")) |> # labels the stubhead column
  fmt_number() |> # rounds numbers to 2 decimal points
  tab_header(title = md(str_c("Estimated model coefficients for", season, species_name, sep = ""))) |>
  cols_width(everything() ~ px(120)) |> 
  tab_options(table.width = px(200),
              column_labels.font.weight = "bold")


ranpar.tbl <- ran_pars |> column_to_rownames(var = "Parameter") |>
  gt(rownames_to_stub = TRUE) |> 
  tab_stubhead(label = md("**Parameter**")) |>
  fmt_number() |>
  tab_header(title = md(str_c("Estimated random parameters from the",  season, species_name, "model", sep = ""))) |>
  cols_width(everything() ~ px(120)) |> 
  tab_options(table.width = px(200), 
              column_labels.font.weight = "bold")

### save the data
tables <- list("est_tbl.png" = est.tbl, 
               "ran-pars_tbl.png" = ranpar.tbl)

pmap(list(tables, names(tables)), ~gtsave(.x, filename = str_c(season, .y, sep = "_"), path = here(tab.files), expand = 10))


