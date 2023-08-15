### created: 12/10/2022
### last updated: 03/03/2023

#### 03b - COMPARE SPRING MODELS ####

###################
#### OBJECTIVE ####
###################
# compare and cross validate models fit to historical summer flounder data  

####################

#### LOAD PACKAGES ####
# install.packages("remotes")
# library(remotes)
# remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE)
# install.packages("marmap") 
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(kableExtra)
library(patchwork)
# library(marmap)
# library(raster)

here()


#### COMPARISON TABLE ####

spr.mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                       "AIC" = c(AIC(m1_spring), AIC(m2_spring), AIC(m3_spring), AIC(m4_spring), AIC(m5_spring), AIC(m6_spring), AIC(m7_spring)),# )
                       
                       
                       #mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                       "TOTAL_ELPD" = c(m1spr.cv$elpd, m2spr.cv$elpd, m3spr.cv$elpd, m4spr.cv$elpd, m5spr.cv$elpd, m6spr.cv$elpd, m7spr.cv$elpd), 
                       "TOTAL_LOGLIK" = c(m1spr.cv$sum_loglik, m2spr.cv$sum_loglik, m3spr.cv$sum_loglik, m4spr.cv$sum_loglik, m5spr.cv$sum_loglik, m6spr.cv$sum_loglik, m7spr.cv$sum_loglik)) #%>% 
#right_join(mods, by = "models")

# save the data
saveRDS(spr.mods, file = here("sdmtmb", "model-outputs", "spr-mod-diagnostics.rds"))


# format the table for presentation
spr_mods_tbl <- spr.mods %>% 
  mutate(TOTAL_ELPD = round(TOTAL_ELPD, 2),
         TOTAL_LOGLIK = round(TOTAL_LOGLIK, 2)) %>% 
  rename(" " = models, 
         "Total Expected Log Predictive Density" = TOTAL_ELPD,
         "Total Log Likelihood" = TOTAL_LOGLIK)%>% 
  arrange(desc(AIC))

kable(spr_mods_tbl[,c(1, 4, 2:3)], align = "lcccc", caption = "Spring Model Diagnostic Values", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 

# how to save the table 





