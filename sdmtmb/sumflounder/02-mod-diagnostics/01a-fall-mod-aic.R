### created: 
### last updated:

#  - ####

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
fall.mods <- str_c("m",seq(1:17), "_fall.rds") |>
  append(str_c("m",c(8, 11, 14:16),"_fall2.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "sumflounder", "data", "mods", .)))

mod.names <- str_c("m",seq(1:17)) |> 
  append(str_c("m",c(8, 11, 14:16),"a"))

# apply AIC function to each model
fall.aic <- map(fall.mods, ~AIC(.)) |> 
  as.data.frame() |> 
  t()
rownames(fall.aic) <- mod.names#seq(1:17)
  
fall.form <- map(fall.mods, ~pluck(., "formula")) |> 
  as.character() |> 
  map(~str_sub(., 6, -2)) |> 
  as.data.frame() |>
  t()
rownames(fall.form) <- mod.names

spatial <- map(fall.mods, ~pluck(., "spatial")) |> 
  as.character() |>
  as.data.frame() |> 
  rename(spatial = `as.character(map(fall.mods, ~pluck(., "spatial")))`)
rownames(spatial) <- mod.names

spatiotemporal <- map(fall.mods, ~pluck(., "spatiotemporal")) |>
  as.character() |>
  as.data.frame() |>
  rename(spatiotemporal = `as.character(map(fall.mods, ~pluck(., "spatiotemporal")))`)
rownames(spatiotemporal) <- mod.names

spatial_varying_formula <- map(fall.mods, ~pluck(., "spatial_varying_formula")) |> 
  as.character() |>  
  as.data.frame() |> 
  rename(spatial_varying = `as.character(map(fall.mods, ~pluck(., "spatial_varying_formula")))`)

time <- map(fall.mods, ~pluck(., "time")) |> 
  as.data.frame() |> 
  t() 
rownames(time) <- mod.names


convergence <- map(fall.mods, ~pluck(., "sd_report", "pdHess")) |> 
  as.data.frame() |> 
  t()
rownames(convergence) <- mod.names
  
  
mods <- fall.form |> 
  bind_cols(spatial, spatiotemporal, time,  spatial_varying_formula, convergence, fall.aic) |> 
  rename(formula = ...1, 
         time = ...4, 
         converged = ...6, 
         AIC = ...7) |> 
  mutate(extra_time = as.integer(2020), 
         family = "tweedie", 
         time = ifelse(time=="EST_YEAR", time, "NULL"), 
         season = "FALL") |> 
  relocate(c(converged, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") |> 
  arrange(AIC)
  
  
# fall.aic <- fall.aic |> 
#   bind_cols(mod.names) |>
#   rename(AIC = ...1, 
#          models = ...2) |> 
#   relocate(models, AIC) |>
#   arrange(AIC)
# m8a_fall <- readRDS(here(sdmtmb.dir, "model-outputs", "m8_fall2.rds"))
# m11a_fall <- readRDS(here(sdmtmb.dir, "model-outputs", "m11_fall2.rds"))
# m14a_fall <- readRDS(here(sdmtmb.dir, "model-outputs", "m14_fall2.rds"))
# m8a <- data.frame(models = "m8b", AIC = AIC(m8a_fall))
# m11a <- data.frame(models = "m11b", AIC = AIC(m11a_fall))
# m14a<- data.frame(models = "m14b", AIC = AIC(m14a_fall))
# fall.aic <- bind_rows(fall.aic, m8a, m11a, m14a) |> mutate(AIC = round(AIC, 2)) |> arrange(desc(AIC))

# kable(fall.aic, align = "lcccc", caption = "Model AIC Values", format.args = list(big.mark = ","), booktabs = TRUE) %>%
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%
#row_spec(7, color = "red") 


### CONFIGURATIONS TABLE #####
# fall.mods.config <- data.frame("Models" = mod.names, #c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
#                                "Formula" = c("EXPCATCHWT ~ AVGDEPTH", "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ AVGDEPTH",
#                                              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ AVGDEPTH", 
#                                              "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1"), 
#                                "Family" = rep(paste("Tweedie(link = log)"), 7), 
#                                "Spatial Fields" = c(rep(paste("off"), 2), rep(paste("on"), 4), "off"), 
#                                "Time" = c(rep(paste("-"), 4), rep(paste("EST_YEAR"), 3)), 
#                                "Spatiotemporal Fields" =  c(rep(paste("-"), 4), rep(paste("IID"), 3)))

# save the data
saveRDS(mods, file = here("sdmtmb", "sumflounder", "data", "fall-mod-configs.rds"))


kable(mods, align = "lcccc", caption = "Fall Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)# %>%
#row_spec(7, color = "red") 

#### DEPTH RELATIONSHIP TABLE #####
# fall.dep.rel <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
#                            "depth_rel" = c(m1_fall$model$par[2], m2_fall$model$par[14], m3_fall$model$par[2], m4_fall$model$par[14],
#                                            m5_fall$model$par[2], m6_fall$model$par[14], m7_fall$model$par[14])) 
# 
# # save the data
# saveRDS(fall.dep.rel, file = here("sdmtmb", "data", "fall-depth-rel.rds"))
# 
# 
# fall.dep.rel <- fall.dep.rel %>% rename("Depth Estimate" = depth_rel)
# 
# kable(fall.dep.rel, align = "lcccc", caption = "Fall Depth Relationships", format.args = list(big.mark = ","), booktabs = TRUE) %>%
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
#   row_spec(7, color = "red") 
