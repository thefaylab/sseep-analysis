### created:      11/27/2022
### last update:  
###

## 01b - LEAST IMPACTED SPECIES ANALYSIS ####

##################
### OBJECTIVE ####
##################
# # calculate the least impacted species by number and weight within the wind areas.
# find species that overlap in least numbers and least weight lost due to wind
# plot impacted species 

# awaiting new full raw dataset from Catherine to run in here(...); when received run and check code. Derived data from tidy-data script will be called to run in the loops. 
####################


### LOAD PACKAGES ###
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

here()


### LOAD DATA ###
# dataset created from `02-complete-dataset.R` here("tidy-data"). Contains complete observations for each species and unique tow.
data <- readRDS(here("data", "rds", "merged_data_complete.rds")) %>% 
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT)) # fills expcatch wt values with 0 if missing

# creates a wind dataframe based on the AREA column
wind <- data %>% filter(AREA == "WIND")

# species dataframe for adding to final dataset for plotting by common name
species <- data %>% 
  select(SVSPP, COMNAME, SCINAME) %>%
  unique()


### SUMMARISE #####
#### BY NUMBER ####
least_n <- wind %>% 
  group_by(SVSPP) %>% 
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), # finds total numbers for each species 
            towct = length(unique(STATION))) %>% # finds total number of unique tows for each species 
  filter(EXPCATCHNUM != 0) %>%
  slice_min(order_by = EXPCATCHNUM, n = 25) %>% # pulls the top 25 based on max EXPCATCHNUM
  left_join(species, by = "SVSPP") # joins species dataframe to add common names 

# save data
saveRDS(least_n, here("data", "rds", "least_num-impact_sp.rds"))


#### BY WEIGHT ####
least_wt <- wind %>% 
  group_by(SVSPP) %>% 
  summarise(EXPCATCHWT = sum(EXPCATCHWT), # finds total biomass for each species 
            towct = length(unique(STATION))) %>% # finds total number of unique tows for each species 
  filter(EXPCATCHWT != 0) %>%
  slice_min(order_by = EXPCATCHWT, n = 25) %>% # pulls the top 25 based on max EXPCATCHWT
  left_join(species, by = "SVSPP") # joins species dataframe to add common names 

# save data
saveRDS(least_wt, here("data", "rds", "least_wt-impact_sp.rds"))


#### COMBINE ####
# find species that appear in both number and weight dataframes 
least <- inner_join(least_n, least_wt, by = "SVSPP") %>%  # 17 of 25 species have bottom weights and numbers impacted
  select(!c(COMNAME.y, SCINAME.y, towct.y)) %>% 
  rename(COMNAME = COMNAME.x, 
         SCINAME = SCINAME.x, 
         towct = towct.x)

# save data
saveRDS(least, here("data", "rds", "least_impact_sp.rds"))


### PLOT #####
ggplot(least) +
  geom_col(aes(EXPCATCHNUM, fct_inorder(COMNAME))) + 
  labs(x = "TOTAL WEIGHT (kg)", y = "SPECIES", title = "Least Impacted Species by Number within Potential Wind Areas")

ggsave(filename = "least_n-species.png", device = "png", path = here("init-analysis", "outputs"), width = 8, height = 5)

ggplot(least %>% arrange(EXPCATCHWT)) +
  geom_col(aes(EXPCATCHWT, fct_inorder(COMNAME))) + 
  labs(x = "TOTAL WEIGHT (kg)", y = "SPECIES", title = "Least Impacted Species by Weight within Potential Wind Areas")

ggsave(filename = "least_wt-species.png", device = "png", path = here("init-analysis", "outputs"), width = 8, height = 5)