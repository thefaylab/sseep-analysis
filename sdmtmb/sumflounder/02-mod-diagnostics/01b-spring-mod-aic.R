### created: 
### last updated:

#  - ####

## OBJECTIVE ####
  

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

spr.mods <- str_c("m",seq(1:17), "_spring.rds") |>
  append(str_c("m",c(8, 11, 14:17),"_spring2.rds")) |>
  append(str_c("m",c(15:17),"_spring3.rds")) |>
  map(~list(.)) |>
  map(~readRDS(here("sdmtmb",  "sumflounder", "data", "mods", .)))

mod.names <- str_c("m",seq(1:17)) |> 
  append(str_c("m",c(8, 11, 14:17),"a")) |>
  append(str_c("m",c(15:17),"b"))

spr.aic <- map(spr.mods, ~AIC(.)) |> 
  as.data.frame() |> 
  t()
rownames(spr.aic) <- mod.names#seq(1:17)

spr.form <- map(spr.mods, ~pluck(., "formula")) |> 
  as.character() |> 
  map(~str_sub(., 6, -2)) |> 
  as.data.frame() |>
  t()
rownames(spr.form) <- mod.names

spatial <- map(spr.mods, ~pluck(., "spatial")) |> 
  as.character() |>
  as.data.frame() |> 
  rename(spatial = `as.character(map(spr.mods, ~pluck(., "spatial")))`)
rownames(spatial) <- mod.names

spatiotemporal <- map(spr.mods, ~pluck(., "spatiotemporal")) |>
  as.character() |>
  as.data.frame() |>
  rename(spatiotemporal = `as.character(map(spr.mods, ~pluck(., "spatiotemporal")))`)
rownames(spatiotemporal) <- mod.names

spatial_varying_formula <- map(spr.mods, ~pluck(., "spatial_varying_formula")) |> 
  as.character() |>  
  as.data.frame() |> 
  rename(spatial_varying = `as.character(map(spr.mods, ~pluck(., "spatial_varying_formula")))`)

time <- map(spr.mods, ~pluck(., "time")) |> 
  as.data.frame() |> 
  t() 
rownames(time) <- mod.names


convergence <- map(spr.mods, ~pluck(., "sd_report", "pdHess")) |> 
  as.data.frame() |> 
  t()
rownames(convergence) <- mod.names


mods <- spr.form |> 
  bind_cols(spatial, spatiotemporal, time,  spatial_varying_formula, convergence, spr.aic) |> 
  rename(formula = ...1, 
         time = ...4, 
         converged = ...6, 
         AIC = ...7) |> 
  mutate(family = "tweedie", 
         time = ifelse(time=="EST_YEAR", time, "NULL"), 
         season = "SPRING") |> 
  relocate(c(converged, AIC), .after = everything()) |> 
  rownames_to_column(var = "models") |> 
  arrange(AIC)

####  ####
# mods <- str_c("m",seq(1:14)) |>
#   map(~list(.)) |>
#   map(~readRDS(here(sdmtmb.dir, "model-outputs", str_c(., "_spring.rds"))))
# #names <- c(str_c("m",seq(1:14)), "m8b", "m11b", "m14b")
# 
# # mods[[15]] <- m8_spring2
# # mods[[16]] <- m11_spring2
# # mods[[17]] <- m14_spring2
# # names(mods) <- names
# # spr.mods <- list(m1_spring, m2_spring, m3_spring, m4_spring, m5_spring, m6_spring, m7_spring, m8_spring, m9_spring, m10_spring, m11_spring, m12_spring, m13_spring, m14_spring)
# AIC(m8_spring2)
# AIC(m11_spring2)
# AIC(m14_spring2)
# 
# mod.names <- str_c("m",seq(1:14))
# spr.aic <- map(mods, ~AIC(.)) |> 
#   as.data.frame() |> t()
# rownames(spr.aic) <- seq(1:14)
# spr.aic <- spr.aic |> 
#   bind_cols(mod.names) |>
#   rename(AIC = ...1, 
#          models = ...2) |> 
#   relocate(models, AIC)
# 
# m8a_spr <- readRDS(here(sdmtmb.dir, "model-outputs", "m8_spring2.rds"))
# m11a_spr <- readRDS(here(sdmtmb.dir, "model-outputs", "m11_spring2.rds"))
# #m14a_spr <- readRDS(here(sdmtmb.dir, "model-outputs", "m14_spring2.rds"))
# m8as <- data.frame(models = "m8b", AIC = AIC(m8a_spr))
# m11as <- data.frame(models = "m11b", AIC = AIC(m11a_spr))
# #m14as<- data.frame(models = "m14b", AIC = AIC(m14a_spr))
# spr.aic <- bind_rows(spr.aic, m8as, m11as) |> mutate(AIC = round(AIC, 2)) |> arrange(desc(AIC))
# # m7 is best overall 
# # m8 is best with wind covariate
# #m8a without spatially-varying covariate - better fit than m8 but still not better than m7 - IID models
# 
# 
# kable(spr.aic, align = "lcccc", caption = "Model AIC Values", format.args = list(big.mark = ","), booktabs = TRUE) %>%
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%
# #row_spec(7, color = "red")
# 
# map(mods, ~logLik(.))
# 
# 
####  #####
# spr.mods.config <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
#                               "Formula" = c("EXPCATCHWT ~ AVGDEPTH", "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ AVGDEPTH",
#                                             "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ AVGDEPTH", 
#                                             "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1", "EXPCATCHWT ~ s(AVGDEPTH) + as.factor(EST_YEAR) - 1"), 
#                               "Family" = rep(paste("Tweedie(link = log)"), 7), 
#                               "Spatial Fields" = c(rep(paste("off"), 2), rep(paste("on"), 4), "off"), 
#                               "Time" = c(rep(paste("-"), 4), rep(paste("EST_YEAR"), 3)), 
#                               "Spatiotemporal Fields" =  c(rep(paste("-"), 4), rep(paste("IID"), 3)))
# 
### save the data ####
saveRDS(mods, file = here("sdmtmb", "sumflounder", "data", "spr-mod-configs.rds"))
# 
# 
kable(mods, align = "lcccc", caption = "Spring Model Configurations", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #%>%
#row_spec(7, color = "red")
# 
# ## DEPTH RELATIONSHIP TABLE #####
# spr.dep.rel <- data.frame("Models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
#                           "depth_rel" = c(m1_spring$model$par[2], m2_spring$model$par[14], m3_spring$model$par[2], m4_spring$model$par[14],
#                                           m5_spring$model$par[2], m6_spring$model$par[14], m7_spring$model$par[14])) 
# 
# # save the data
# saveRDS(spr.dep.rel, file = here("sdmtmb", "data", "spr-depth-rel.rds"))
# 
# 
# spr.dep.rel <- spr.dep.rel %>% rename("Spring Depth Estimates" = depth_rel)
# 
# kable(spr.dep.rel, align = "lcccc", caption = "Spring Depth Relationships", format.args = list(big.mark = ","), booktabs = TRUE) %>%
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
#   row_spec(7, color = "red") 
