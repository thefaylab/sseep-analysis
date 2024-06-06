### created: 08/02/2023
### last updated: 12/13/2023

# 02a - Fall Model Performance: Sum of Log Likelihoods ####

## OBJECTIVE ####
# Script will 
## read the 12 summer flounder models fit with cross-validation methods 
## quantify the sum of log-likelihoods of each model 
## compare the 12 values for log-likelihood to determine predictive accuracy and performance of each model

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

### Environment Set Up ####
# season 
season <- "fall"

# file names that will be read in 
cv.files <- str_c("m",seq(1:12), "-cv.rds", sep = "")

# extract only the m# string
cv.names <- str_extract(cv.files, "[a-z]\\d+") 

### File locations ####
sumflounder.dat <- here("sdmtmb",  "sumflounder", "data")

# model locations
cv.locs <- here(sumflounder.dat, "cross-valid", season)

## READ DATA ####
cv.list <- cv.files |>
  #append(str_c("m",c(8, 11, 15:16), "-fall-cv2.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here(cv.locs, .)))


  
sum_logliks <- map(cv.list, ~pluck(., "sum_loglik")) |> 
  as.data.frame(row.names = "Sum log likelihood") |>
  t()
rownames(sum_logliks) <- cv.names
# colnames(sum_logliks) <- "Sum log likelihood"


# elpd <- map(fall.cv, ~pluck(., "elpd")) |> 
#   as.data.frame() |> 
#   t() 
# rownames(elpd) <- mod.names
# colnames(elpd) <- "Expected log pointwise predictive density"


conv <- map(cv.list, ~pluck(., "converged")) |> 
  as.data.frame(row.names = "convergence") |> 
  t()
rownames(conv) <- cv.names
# colnames(convergence) <- "Convergence"

cvs_tbl <- sum_logliks |> 
  as.data.frame() |>
  rownames_to_column(var = "models") |>
  bind_cols(conv)# |>
  # mutate(`Sum log likelihood` = round(`Sum log likelihood`,2),
  #        `Expected log pointwise predictive density` = round(`Expected log pointwise predictive density`,2)) |>
  # arrange(desc(elpd))
  # 
  
# save the data
saveRDS(cvs_tbl, file = here(sumflounder.dat, str_c(season, "-sumloglik.rds", sep = "")))


kable(cvs_tbl, align = "lcccc", caption = "Summer flounder fall cross validations diagnostics", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) |>
row_spec(which(cvs_tbl$`Sum log likelihood` == max(cvs_tbl$`Sum log likelihood`)), color = "red") 

## COMBINE AIC AND ELPD TABLES ####
mods <- readRDS(file = here(sumflounder.dat, str_c(season, "-mod-configs.rds", sep = "")))

diagnostics <- left_join(mods, cvs_tbl, by = "models") |> 
  select(!convergence.y) |> 
  rename(convergence = convergence.x) #|>
  #mutate(AIC = round(AIC, 2))

kable(diagnostics, align = "lcccc", caption = "Summer flounder seasonal model performance", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>
  #save_kable(file = here("sdmtmb", "atlmackerel", "plots", "mod-diagnostics-tbl.png"))

