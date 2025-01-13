### created: 04/30/2024
### last updated: 11/07/2024

# 04 - CONSOLIDATE DIAGNOSTICS ####

## OBJECTIVE ####
# join the aic, cross validation test error rate, and deviance tables to create one over-arching table of diagnostics for each seasonal model set 

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
# library(kableExtra)
# library(ggeffects)
library(gt)
set.seed(123)
theme_set(theme_bw())

### Environment Set Up ####
# season 
season <- "spring"

# species
species <- "atlmackerel"

species_name <- "atlantic mackerel"

### File locations ####
dat.files <- here("data", "rds", "sdmtmb",  species)
# dat.files <- here("data", "rds", "sdmtmb",  species, "diagnostics", "dpg", "no-dep-out")

### Read in data ####
mods_tbl <- readRDS(file = here(dat.files, str_c(season, "-mod-configs.rds", sep = ""))) 

cvs_tbl <- readRDS(file = here(dat.files, str_c(season, "-cv-diagnostics.rds", sep = "")))

deviance <- readRDS(file = here(dat.files, str_c(season, "deviance.rds", sep = "_"))) |> 
  rownames_to_column(var = "models")

## COMBINE TABLES ####
diagnostics <- left_join(mods_tbl, cvs_tbl, by = "models") |> 
  left_join(deviance, by = "models") |>
  rename(mod_convergence = convergence.x, 
         cv_convergence = convergence.y) |> 
  relocate(AIC, .after = cv_convergence) |> 
  mutate(cv_convergence = str_to_sentence(cv_convergence))


# kable(diagnostics, align = "lcccc", caption = str_c(str_to_sentence(species_name), "seasonal model diagnostics", sep = " "), format.args = list(big.mark = ","), booktabs = TRUE) |>
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>
  #row_spec(which(diagnostics$AIC == min(diagnostics$AIC)), color = "red") 

diagnostics_gt <- diagnostics |> column_to_rownames(var = "models") |>
  gt(rownames_to_stub = TRUE) |> # adds a line between model names and estimates 
  tab_stubhead(label = md("**Models**")) |> # labels the stubhead column
  fmt_number() |> # rounds numbers to 2 decimal points
  fmt_percent(columns = dev_exp) |> # makes into a percent
  fmt_percent(columns = perc_dev_exp, scale_values = FALSE) |> # only adds the % sign
  cols_label_with(everything(), fn = function(x) str_to_sentence(x)) |>
  cols_label(AIC = md("**AIC**"),
             mod_convergence = md("**Model Convergence**"), 
             cv_convergence = md("**Cross Validation Convergence**"),
             cv_mse = md("**MSE**"), 
             log_likelihood = md("**Log Likelihood**"), 
             dev_exp = md("**Deviance Explained**"), 
             perc_dev_exp = md("**Percent Deviance Explained**")) |>
  tab_header(title = md(str_c(str_to_title(season), species_name, "diagnostics", sep = " "))) |>
  tab_options(column_labels.font.weight = "bold", 
              table.width = pct(30))

saveRDS(diagnostics, here(dat.files, str_c(season, "diagnostics.rds", sep = "_")))

gtsave(diagnostics_gt, filename = str_c(season, "diagnostics.docx", sep = "_"), path = here("outputs", "sdmtmb", species, "tables"), expand = 100)

# gtsave(diagnostics_gt, filename = str_c(season, "diagnostics.png", sep = "_"), path = here("outputs", "sdmtmb", species, "tables"), expand = 100)
