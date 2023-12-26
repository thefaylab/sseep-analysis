### created: 11/15/2023
### last updated: 12/13/2023

# 02a - Model Performance: Sum of Log Likelihoods ####

## OBJECTIVE ####
# Script will 
## read the 12 models fit with cross-validation methods 
## quantify the sum of log-likelihoods of each model 
## compare the 12 values for log-likelihood to determine predictive accuracy and performance of each model 

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
spring.cv <- str_c("m",seq(1:12), "-cv.rds") |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "atlmackerel", "data", "cross-valid", .)))


mod.names <- str_c("m",seq(1:12)) 

## EXTRACT METRICS ####
# Sum of log-likelihoods
sum_logliks <- map(spring.cv, ~pluck(., "sum_loglik")) |> 
  as.data.frame() |>
  t()
rownames(sum_logliks) <- mod.names
colnames(sum_logliks) <- "sum_loglik"

# expected log predictive density
elpd <- map(spring.cv, ~pluck(., "elpd")) |> 
  as.data.frame() |> 
  t()
rownames(elpd) <- mod.names
colnames(elpd) <- "elpd"

# convergence 
convergence <- map(spring.cv, ~pluck(., "converged")) |> 
  as.data.frame() |> 
  t()
rownames(convergence) <- mod.names
colnames(convergence) <- "converged"

# bind all information
cvs <- sum_logliks |> 
  as.data.frame() |>
  rownames_to_column(var = "models") |>
  bind_cols(elpd, convergence) |> 
  mutate(family = "Tweedie", 
         outliers = "Present") |>
  arrange(desc(sum_loglik))
  
  
# save the data
saveRDS(cvs, file = here("sdmtmb", "atlmackerel", "data", "spring-cvs.rds"))


kable(cvs, align = "lcccc", caption = "Atlantic mackerel spring cross validations", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)# %>%
#row_spec(7, color = "red") 

## COMBINE AIC AND ELPD TABLES ####
mods <- readRDS(file = here("sdmtmb", "atlmackerel", "data", "spr-mod-configs.rds"))

diagnostics <- left_join(mods, cvs, by = "models") |> 
  select(!c(converged.y, family.x)) |> 
  rename(converged = converged.x, 
         family = family.y) |> 
  mutate(AIC = round(AIC, 2),
         sum_loglik = round(sum_loglik, 2), 
         elpd = round(elpd, 2))

kable(diagnostics, align = "lcccc", caption = "Atlantic mackerel model performance", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>
  #save_kable(file = here("sdmtmb", "atlmackerel", "plots", "mod-diagnostics-tbl.png"))
