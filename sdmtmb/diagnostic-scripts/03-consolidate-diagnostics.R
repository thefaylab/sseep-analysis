### created: 04/30/2024
### last updated: 

# 03 - CONSOLIDATE DIAGNOSTICS ####

## OBJECTIVE ####
# join the aic and cross validation test error rate tables to create one over-arching table of diagnostics for each model 

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)
library(ggeffects)
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

### Read in data ####
mods_tbl <- readRDS(file = here(dat.files, str_c(season, "-mod-configs.rds", sep = ""))) ## FIXME when repo is reorganized 

cvs_tbl <- readRDS(file = here(dat.files, str_c(season, "-cv-diagnostics.rds", sep = "")))

## COMBINE TABLES ####
diagnostics <- left_join(mods_tbl, cvs_tbl, by = "models") |> 
  rename(mod_convergence = convergence.x, 
         cv_convergence = convergence.y) |> 
  relocate(AIC, .after = cv_convergence) |> 
  mutate(cv_convergence = str_to_sentence(cv_convergence))


kable(diagnostics, align = "lcccc", caption = str_c(str_to_sentence(species_name), "seasonal model diagnostics", sep = " "), format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>
  #row_spec(which(diagnostics$AIC == min(diagnostics$AIC)), color = "red") 


saveRDS(diagnostics, here(dat.files, str_c(season, "diagnostics.rds", sep = "_")))
