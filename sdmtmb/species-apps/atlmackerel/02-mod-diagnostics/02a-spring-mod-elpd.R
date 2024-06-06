### created: 11/15/2023
### last updated: 04/11/2024

# 02a - Model Performance: Sum of Log Likelihoods ####

## OBJECTIVE ####
# Script will 
## read the models fit with cross-validation methods 
## quantify the sum of log-likelihoods of each model 
## compare the values for log-likelihood to determine predictive accuracy and performance of each model 

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

# cross validation family description: c("Tweedie", "Delta Gamma")
fam_desc <- "Tweedie"

# file names that will be read in 
cv.files <- str_c("m",seq(1:14), "-cv.rds")

# extract only the m# string
cv.names <- str_extract(cv.files, "[a-z]\\d+") 

### File Locations 
# species folder 
atlmack.dat <- here("sdmtmb", "atlmackerel", "data")

# model locations
cv.locs <- here(atlmack.dat, "cross-valid", fam_file, dat_set)

## READ DATA ####
# create a list of all the cross validation files read in from the folder 
cv.list <- cv.files |>
  map(~list(.)) |>
  map(~readRDS(here(cv.locs, .)))


## EXTRACT METRICS ####
# Sum of log-likelihoods
sum_logliks <- map(cv.list, ~pluck(., "sum_loglik")) |> 
  as.data.frame(row.names = "sum log likelihood") |>
  t()
rownames(sum_logliks) <- cv.names
# colnames(sum_logliks) <- "sum_loglik"

# expected log predictive density
# elpd <- map(spring.cv, ~pluck(., "elpd")) |> 
#   as.data.frame() |> 
#   t()
# rownames(elpd) <- mod.names
# colnames(elpd) <- "elpd"

# convergence 
conv <- map(cv.list, ~pluck(., "converged")) |> 
  as.data.frame(row.names = "convergence") |> 
  t()
rownames(conv) <- cv.names
# colnames(convergence) <- "converged"

# bind all information
cvs <- sum_logliks |> 
  as.data.frame() |>
  rownames_to_column(var = "models") |>
  bind_cols(conv) |> 
  mutate(family = fam_desc, 
         data = data_desc, 
         convergence = str_to_title(convergence)) #|>
  #arrange(desc(sum_loglik))
  
  
# save the data
saveRDS(cvs, file = here(atlmack.dat, str_c(str_c(fam_file, "sumloglik", dat_set, sep = "_"), ".rds", sep = "")))


kable(cvs, align = "lcccc", caption = "Atlantic mackerel spring cross validations", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) |> 
  row_spec(which(cvs$`sum log likelihood` == max(cvs$`sum log likelihood`)), color = "red") 

## COMBINE AIC AND ELPD TABLES ####
mods <- readRDS(file = here(atlmack.dat, str_c(str_c(fam_file, "mod-configs", dat_set, sep = "_"), ".rds", sep = "")))

diagnostics <- left_join(mods, cvs, by = "models") |> 
  select(!c(family.y, data.y)) |> 
  rename(mod_convergence = convergence.x,
         CV_convergence = convergence.y,
         family = family.x, 
         data = data.x) |>
  relocate(mod_convergence, .after = `sum log likelihood`)#|>
  # mutate(AIC = round(AIC, 2),
         # sum_loglik = round(sum_loglik, 2), 
         # elpd = round(elpd, 2))

saveRDS(diagnostics, file = here(atlmack.dat, str_c(str_c(fam_file, "diagnostics", dat_set, sep = "_"), ".rds", sep = "")))

kable(diagnostics, align = "lcccc", caption = "Atlantic mackerel model performance", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>
  #save_kable(file = here("sdmtmb", "atlmackerel", "plots", "mod-diagnostics-tbl.png"))
