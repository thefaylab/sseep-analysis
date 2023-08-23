### created: 08/02/2023
### last updated: 08/17/2023

# 02a - ####

## OBJECTIVE ####
#   

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
fall.cv <- str_c("m",c(1:12, 14:17), "-fall-cv.rds") |>
  append(str_c("m",c(8, 11, 15:16), "-fall-cv2.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "sumflounder", "data", "cross-valid", .)))

mod.names <- str_c("m",c(1:12, 14:17), ".cv") |> 
  append(str_c("m",c(8, 11, 15:16),"a.cv"))


  
sum_logliks <- map(fall.cv, ~pluck(., "sum_loglik")) |> 
  as.data.frame() |>
  t()
rownames(sum_logliks) <- mod.names
colnames(sum_logliks) <- "sum_loglik"


elpd <- map(fall.cv, ~pluck(., "elpd")) |> 
  as.data.frame() |> 
  t()
rownames(elpd) <- mod.names
colnames(elpd) <- "elpd"


convergence <- map(fall.cv, ~pluck(., "converged")) |> 
  as.data.frame() |> 
  t()
rownames(convergence) <- mod.names
colnames(convergence) <- "converged"

cvs <- sum_logliks |> 
  as.data.frame() |>
  rownames_to_column(var = "models") |>
  bind_cols(elpd, convergence) |> 
  arrange(desc(elpd))
  
  
# save the data
saveRDS(cvs, file = here("sdmtmb", "sumflounder", "data", "fall-cvs.rds"))


kable(mods, align = "lcccc", caption = "Fall Cross Validations", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)# %>%
#row_spec(7, color = "red") 

