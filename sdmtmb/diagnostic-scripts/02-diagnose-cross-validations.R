### created: 04/25/2024
### last updated: 05/20/2024

# 02 - DIAGNOSTICS: CROSS VALIDATION TEST ERROR RATE ####

## OBJECTIVE ####
# extract convergence and total log-likehood information from each model 
# calculate the test error rate of each fold and the average test error rate 
# format information into a table

### Load packages ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
# library(kableExtra)
library(gt)

### Environment Set Up ####
# season 
season <- "spring"

# species
species <- "atlmackerel"

species_name <- "atlantic mackerel"

### File locations ####
dat.files <- here("sdmtmb",  species, "data") ## FIXME when repo is reorganized

# model locations
cv.locs <- here(dat.files, "cross-valid", season) ## FIXME when repo is reogranized 


### Read in data ####
# number of models to read in 
cv.num <- length(list.files(cv.locs, pattern = "[a-z]\\d+")) # find files in cv.locs that contain a pattern related to the model file names only; excludes folders. 
# this is used here rather than assigning list.files(cv.locs) into cv.files (below), because models numbered in the teens are listed between model 1 and 2 due to folder organization. 

# file names that will be read in 
cv.files <- str_c("m",seq(cv.num), "-cv.rds", sep = "")

# extract only the m# string
cv.names <- str_c("m",seq(cv.num))

# load in
cv.list <- cv.files |>
  map(~list(.)) |>
  map(~readRDS(here(cv.locs, .)))


## EXTRACT VALUES #### 
# extract the sum of likelihood of folds 
sum_logliks <- map(cv.list, ~pluck(., "sum_loglik")) |> 
  as.data.frame(row.names = "Sum log likelihood") |>
  t()
rownames(sum_logliks) <- cv.names

# extract whether the cross-validation converged 
conv <- map(cv.list, ~pluck(., "converged")) |> 
  as.data.frame(row.names = "convergence") |> 
  t()
rownames(conv) <- cv.names


## CALCULATE TEST ERROR RATES ####
fold.errs <- map(cv.list, ~group_by(.$data, cv_fold) |>
                   mutate(error = EXPCATCHWT - cv_predicted, 
                          sq.err = error^2) |> 
                   summarise(mse = mean(sq.err))
)

cv.errors <- map(fold.errs, ~mean(.$mse)) |> 
  map(~pluck(.)) |> 
  as.data.frame(row.names = "cv_mse") |>
  t()
rownames(cv.errors) <- cv.names


## CREATE CROSS-VALIDATION TABLE ####
cvs_tbl <- sum_logliks |> 
  as.data.frame() |>
  rownames_to_column(var = "models") |>
  bind_cols(conv, cv.errors) |> 
  relocate(convergence, .before = `Sum log likelihood`) 

# save the data
saveRDS(cvs_tbl, file = here(dat.files, str_c(season, "-cv-diagnostics.rds", sep = ""))) ## FIXME when repo is reorganized 

# kable(cvs_tbl, align = "lcccc", caption = str_c(str_to_sentence(species_name), season, "cross validation diagnostics", sep = " "), format.args = list(big.mark = ","), booktabs = TRUE) |>
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) |>
#   row_spec(which(cvs_tbl$cv_mse == min(cvs_tbl$cv_mse)), color = "red") 

cvs_gt <- cvs_tbl |> column_to_rownames(var = "models") |>
  gt(rownames_to_stub = TRUE) |> # adds a line between model names and estimates 
  tab_stubhead(label = md("**Models**")) |> # labels the stubhead column
  fmt_number() |> # rounds numbers to 2 decimal points
  cols_label(convergence = md("**Convergence**"), 
             `Sum log likelihood` = md("**Sum log likelihood**"),
             cv_mse = md("**MSE**")) |>
  tab_header(title = md("Fall summer cross validation metrics")) |>
  tab_options(table.width = pct(30))

gtsave(cvs_gt, filename = "fall_cross-valid_diagnostics.png", path = here("sdmtmb", "sumflounder", "tables"), expand = 100)



