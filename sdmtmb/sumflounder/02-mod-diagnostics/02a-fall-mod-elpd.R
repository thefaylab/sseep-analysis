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

fall.mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                        "AIC" = c(AIC(m1_fall), AIC(m2_fall), AIC(m3_fall), AIC(m4_fall), AIC(m5_fall), AIC(m6_fall), AIC(m7_fall)),#)
                        
                        
                        #fall.mods <- data.frame("models" = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                        "TOTAL_ELPD" = c(m1fall.cv$elpd, m2fall.cv$elpd, m3fall.cv$elpd, m4fall.cv$elpd, m5fall.cv$elpd, m6fall.cv$elpd, m7fall.cv$elpd), 
                        "TOTAL_LOGLIK" = c(m1fall.cv$sum_loglik, m2fall.cv$sum_loglik, m3fall.cv$sum_loglik, m4fall.cv$sum_loglik, m5fall.cv$sum_loglik, m6fall.cv$sum_loglik, m7fall.cv$sum_loglik)) #%>% 
#right_join(lamods, by = "models")

# save the data
saveRDS(fall.mods, file = here("sdmtmb", "model-outputs", "fall-mod-diagnostics.rds"))


# format the table for presentation
fall_mods_tbl <- fall.mods %>% 
  mutate(TOTAL_ELPD = round(TOTAL_ELPD, 2),
         TOTAL_LOGLIK = round(TOTAL_LOGLIK, 2)) %>% 
  rename(" " = models, 
         "Total Expected Log Predictive Density" = TOTAL_ELPD,
         "Total Log Likelihood" = TOTAL_LOGLIK)%>% 
  arrange(desc(AIC))

kable(fall_mods_tbl[,c(1, 4, 2:3)], align = "lcccc", caption = "Fall Model Diagnostic Values", format.args = list(big.mark = ","), booktabs = TRUE) %>%
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) %>%
  row_spec(7, color = "red") 

# how to save the table 





