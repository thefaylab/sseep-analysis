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


# read in models
fall.cv <- str_c("m",c(1:12), "-fall-cv.rds") |>
  #append(str_c("m",c(8, 11, 15:16), "-fall-cv2.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "sumflounder", "data", "cross-valid", .)))

mod.names <- str_c("m",c(1:12)) #|> 
  #append(str_c("m",c(8, 11, 15:16),"a.cv"))


  
sum_logliks <- map(fall.cv, ~pluck(., "sum_loglik")) |> 
  as.data.frame() |>
  t()
rownames(sum_logliks) <- mod.names
colnames(sum_logliks) <- "Sum log likelihood"


elpd <- map(fall.cv, ~pluck(., "elpd")) |> 
  as.data.frame() |> 
  t() 
rownames(elpd) <- mod.names
colnames(elpd) <- "Expected log pointwise predictive density"


convergence <- map(fall.cv, ~pluck(., "converged")) |> 
  as.data.frame() |> 
  t()
rownames(convergence) <- mod.names
colnames(convergence) <- "Convergence"

cvs <- sum_logliks |> 
  as.data.frame() |>
  rownames_to_column(var = "models") |>
  bind_cols(elpd, convergence) |> 
  mutate(`Sum log likelihood` = round(`Sum log likelihood`,2),
         `Expected log pointwise predictive density` = round(`Expected log pointwise predictive density`,2)) |>
  arrange(desc(elpd))
  
  
# save the data
saveRDS(cvs, file = here("sdmtmb", "sumflounder", "data", "fall-cvs.rds"))


kable(cvs, align = "lcccc", caption = "Summer flounder fall cross validations diagnostics", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)# %>%
#row_spec(7, color = "red") 

## COMBINE AIC AND ELPD TABLES ####
mods <- readRDS(file = here("sdmtmb", "sumflounder", "data", "fall-mod-configs.rds"))

diagnostics <- left_join(mods, cvs, by = "models") |> 
  select(!converged) |> 
  mutate(AIC = round(AIC, 2))

kable(diagnostics, align = "lcccc", caption = "Summer flounder fall model performance", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>
  #save_kable(file = here("sdmtmb", "atlmackerel", "plots", "mod-diagnostics-tbl.png"))

