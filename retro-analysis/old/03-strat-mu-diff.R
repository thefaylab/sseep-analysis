### created: 11/28/2022
### last updated: 11/28/2023

# 03 - CALCULATING MEAN SQUARED DIFFERENCES FOR STRATIFIED MEANS BY SPECIES ####


## OBJECTIVE ####
# calculate mean squared relative differences for each species as a metric of bias between scenarios (status quo and preclusion)



### LOAD PACKAGES ####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(kableExtra)
library(patchwork)

### dependencies ####
# install.packages("magick")
# install.packages("webshot")
# webshot::install_phantomjs()


### LOAD DATA ####
# dataset created from `02-bind-stratmeans.R` here("retro-analysis"). Contains stratified means and variances for each species and unique tow over time.
data <- readRDS(here("data", "rds", "retro-analysis", "strat-mu_all.rds")) 

# create a lookup table with filename-friendly species names
specieslookup <- data |>
  ungroup() |>
  select(SVSPP, COMNAME) |>
  distinct() |>
  mutate(spname = str_to_lower(gsub(" ", "_", COMNAME)))

## CALCULATE MEAN SQUARED RELATIVE DIFFERENCES #####  
mudiff_dat <- data |> 
  #mutate(stratmu = ifelse(stratmu==0, 1, stratmu)) |>
  #filter(EST_YEAR %in% c(2016:2019, 2021)) |> #filter for recent 5 years, skipping 2020
  #mutate(log_mu = log(stratmu)) |>
  # arrange(desc(stratmu)) |>
  group_by(SVSPP, EST_YEAR, SEASON) |> #, 
  #summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") |>
  summarise(diff_mu = diff(stratmu), .groups = "drop") |>
  # arrange(desc(diff_mu)) |>
  mutate(exp_mu = (exp(diff_mu))-1,
         sq_diff = exp_mu^2) |>
  #arrange(desc(sq_diff))|>
  #ungroup()|>
  #summarize(sq_diff = (diff(stratmu))^2, .groups = "drop") |> # calculate the relative differences and square them; drop the groups for further analysis
  group_by(SVSPP, SEASON) |>
  filter(EST_YEAR %in% tail(EST_YEAR, 5)) |> 
  summarize(mudiff = mean(sq_diff), .groups = "drop") |> # calculate the average; drop the grouping factor 
  mutate(mudiff = sqrt(mudiff)*100) |>
  arrange(desc(mudiff)) |> # arrange highest to lowest 
  left_join(specieslookup, by = "SVSPP") # add the species look up data 


# print the first 10 rows of the data 
topten_fall <- mudiff_dat |>
  filter(SEASON == "FALL") |>
  head(10) |> 
  mutate(mudiff = round(mudiff,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = mudiff)

fall_table <- kable(topten_fall[,c(4,3,2)], align = "lcccc", caption = "Ten Highest Percent Differences in Abundance Indices by Species", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>

# save_kable(fall_table, here("outputs", "fall_mudiff.png"))


topten_spring <- mudiff_dat |>
  filter(SEASON == "SPRING") |>
  head(10) |> 
  mutate(mudiff = round(mudiff,0), 
         COMNAME = str_to_sentence(COMNAME), 
         SEASON = str_to_sentence(SEASON))|> 
  rename(Species = COMNAME, 
         Season = SEASON, 
         "Relative Percent Difference" = mudiff)


spring_table <- kable(topten_spring[,c(4,3,2)], align = "lcccc", caption = "Ten Highest Percent Differences in Abundance Indices by Species", format.args = list(big.mark = ","), booktabs = TRUE) |>
  kable_styling(full_width = F, fixed_thead = T, font_size = 14)

save_kable(spring_table, here("outputs", "spring_mudiff.png"))


### save the data 
saveRDS(mudiff_dat, file = here("data", "rds", "retro-analysis", "species_mean-sq-diff.rds"))


## PLOT THE DISTRIBUTION #####

# sf_mudiff <- mudiff_dat |>
#   filter(SVSPP == 103) |> 
#   mutate(count = 1, 
#          across(mudiff, round, 3))

mudiff_dat <- mudiff_dat |>
  mutate(across(mudiff, round, 3))

ggplot() +  
  geom_histogram(data = mudiff_dat, aes(mudiff, after_stat(count)), color = "white", fill = "#5dc5e9") +
  #scale_x_continuous(breaks = seq(0, 0.525, 0.025)) + 
  #geom_point(data = sf_mudiff, aes(x = mudiff, y = count), color = "#0a4c8a", size = 2.5, shape = 7) +
  #geom_text(data = sf_mudiff, aes(x = mudiff, y = count, label = mudiff), vjust = -2.5, color = "#0a4c8a", angle= 45, nudge_x = 0.045, size = 4) +
  facet_wrap(~SEASON) +
  labs(x = "Relative percent differences between preclusion and status quo annual stratified mean abundance indices", y = "Number of species") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


### save the plot
ggsave(filename ="species_mean-sq-diff.png", plot = last_plot(), device = "png" , path = here("outputs", "plots"), width = 10, height = 8)
#ggsave(filename ="sf_mudiff.jpeg", device = "jpeg" , path = here("outputs", "sumflounder"), width = 10, height = 5)


1 FALL       2017  0       0      0       
2 FALL       2018 -0.260  -0.229  0.0524  
3 FALL       2019 -0.164  -0.151  0.0228  
4 FALL       2021 -0.294  -0.255  0.0651  
5 SPRING     2017  0.0226  0.0229 0.000523
6 SPRING     2018 -0.176  -0.161  0.0259  
7 SPRING     2019 -0.151  -0.140  0.0197  
8 SPRING     2020  0.164   0.178  0.0318  
9 SPRING     2021  0.0214  0.0216 0.000467