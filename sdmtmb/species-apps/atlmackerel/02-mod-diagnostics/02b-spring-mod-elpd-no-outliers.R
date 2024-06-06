### created: 12/13/2023
### last updated: 12/26/2023

# 02b - Model Performance: Sum of Log Likelihoods ####

## OBJECTIVE ####
# Script will 
## read the 12 models fit with cross-validation methods when outliers of biomass are removed 
## quantify the sum of log-likelihoods of each model 
## compare the 12 values for log-likelihood to determine predictive accuracy and performance of each model when outliers are removed  

### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)

here()


### DATA ####
spring.cv_no.out <- str_c("m",seq(1:12), "-cv_no.out.rds") |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "atlmackerel", "data", "cross-valid", .)))


mod.names_no.out <- str_c("m",seq(1:12)) 

## EXTRACT METRICS ####
# Sum of log-likelihoods
sum_logliks_no.out <- map(spring.cv_no.out, ~pluck(., "sum_loglik")) |> 
  as.data.frame() |>
  t()
rownames(sum_logliks_no.out) <- mod.names
colnames(sum_logliks_no.out) <- "sum_loglik"

# expected log predictive density
elpd_no.out <- map(spring.cv_no.out, ~pluck(., "elpd")) |> 
  as.data.frame() |> 
  t()
rownames(elpd_no.out) <- mod.names
colnames(elpd_no.out) <- "elpd"

# convergence 
convergence_no.out <- map(spring.cv_no.out, ~pluck(., "converged")) |> 
  as.data.frame() |> 
  t()
rownames(convergence_no.out) <- mod.names
colnames(convergence_no.out) <- "converged"

# bind all information
cvs_no.out <- sum_logliks_no.out |> 
  as.data.frame() |>
  rownames_to_column(var = "models") |>
  bind_cols(elpd_no.out, convergence_no.out) |> 
  mutate(family = "Tweedie", 
         outliers = "Removed") |>
  arrange(desc(sum_loglik))
  
  
# save the data
saveRDS(cvs_no.out, file = here("sdmtmb", "atlmackerel", "data", "spring-cvs_no.out.rds"))


kable(cvs_no.out, align = "lcccc", caption = "Atlantic mackerel spring cross validations", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)# %>%
#row_spec(7, color = "red") 

## COMBINE AIC AND ELPD TABLES ####
mods_no.out <- readRDS(file = here("sdmtmb", "atlmackerel", "data", "spr-dpg-mod-configs_no.outliers.rds"))

diagnostics_no.out <- left_join(mods_no.out, cvs_no.out, by = "models") |> 
  select(!c(converged.y, family.x)) |> 
  rename(converged = converged.x,
         family = family.y) |> 
  mutate(AIC = round(AIC, 2),
         sum_loglik = round(sum_loglik, 2), 
         elpd = round(elpd, 2))

kable(diagnostics_no.out, align = "lcccc", caption = "Atlantic mackerel spring model performance", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>
  #save_kable(file = here("sdmtmb", "atlmackerel", "plots", "mod-diagnostics-tbl.png"))
