### created: 08/02/2023
### last updated: 08/17/2023

# 02b - ####

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
spring.cv <- str_c("m",seq(1:17), "-spring-cv.rds") |>
  append(str_c("m",c(8, 11, 14:15, 17), "-spring-cv2.rds")) |>
  append(str_c("m",c(15, 17), "-spring-cv3.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "sumflounder", "data", "cross-valid", .)))


mod.names <- str_c("m",seq(1:17)) |>
  append(str_c("m",c(8, 11, 14:15, 17), "a")) |>
  append(str_c("m",c(15, 17), "b"))


  
sum_logliks <- map(spring.cv, ~pluck(., "sum_loglik")) |> 
  as.data.frame() |>
  t()
rownames(sum_logliks) <- mod.names
colnames(sum_logliks) <- "sum_loglik"


elpd <- map(spring.cv, ~pluck(., "elpd")) |> 
  as.data.frame() |> 
  t()
rownames(elpd) <- mod.names
colnames(elpd) <- "elpd"


convergence <- map(spring.cv, ~pluck(., "converged")) |> 
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
saveRDS(cvs, file = here("sdmtmb", "sumflounder", "data", "spring-cvs.rds"))


kable(mods, align = "lcccc", caption = "Spring Cross Validations", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)# %>%
#row_spec(7, color = "red") 

