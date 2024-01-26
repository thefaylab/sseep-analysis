### created:      11/27/2022
### last update:  
###

## 01a - MOST IMPACTED SPECIES ANALYSIS ####

##################
### OBJECTIVE ####
##################
# calculate the most impacted species by number and weight within the wind areas.
# find species that overlap in most numbers and most weight lost due to wind
# plot impacted species 

# awaiting new full raw dataset from Catherine to run in here(...); when received run and check code. Derived data from tidy-data script will be called to run in the loops. 
####################


### LOAD PACKAGES #####
library(stringr)
library(patchwork)
library(here)
suppressPackageStartupMessages(library(tidyverse))

here()


### LOAD DATA #####
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
#### BY NUMBER #####
top_n <- wind %>% 
  group_by(SVSPP) %>% 
  summarise(EXPCATCHNUM = sum(EXPCATCHNUM), # finds total numbers for each species 
            towct = length(unique(STATION))) %>% # finds total number of unique tows for each species 
  slice_max(order_by = EXPCATCHNUM, n = 25) %>% # pulls the top 25 based on max EXPCATCHNUM
  left_join(species, by = "SVSPP") # joins species dataframe to add common names 

# save data
saveRDS(top_n, here("data", "rds", "top_num-impact_sp.rds"))


#### BY WEIGHT ####
top_wt <- wind %>% 
  group_by(SVSPP) %>% 
  summarise(EXPCATCHWT = sum(EXPCATCHWT), # finds total biomass for each species 
            towct = length(unique(STATION))) %>% # finds total number of unique tows for each species 
  slice_max(order_by = EXPCATCHWT, n = 25) %>% # pulls the top 25 based on max EXPCATCHWT
  left_join(species, by = "SVSPP") # joins species dataframe to add common names 

# save data
saveRDS(top_wt, here("data", "rds", "top_wt-impact_sp.rds"))


#### COMBINE ####
# find species that appear in both number and weight dataframes 
top <- inner_join(top_n, top_wt, by = "SVSPP") %>% # 17 of 25 species have top weights and numbers impacted
  select(!c(COMNAME.y, SCINAME.y, towct.y)) %>% 
  rename(COMNAME = COMNAME.x, 
         SCINAME = SCINAME.x, 
         towct = towct.x)

# save data 
saveRDS(top, here("data", "rds", "top_impact_sp.rds"))


### PLOT #####
ggplot(top) +
  geom_col(aes(EXPCATCHNUM, fct_inorder(COMNAME))) + 
  labs(x = "TOTAL CATCH (n)", y = "SPECIES", title = "Top Impacted Species by Number within Potential Wind Areas")

ggsave(filename = "top_n-species.png", device = "png", path = here("init-analysis", "outputs"), width = 8, height = 5)


ggplot(top %>% arrange(desc(EXPCATCHWT))) +
  geom_col(aes(EXPCATCHWT, fct_inorder(COMNAME))) + 
  labs(x = "TOTAL WEIGHT (kg)", y = "SPECIES", title = "Top Impacted Species by Weight within Potential Wind Areas")

ggsave(filename = "top_wt-species.png", device = "png", path = here("init-analysis", "outputs"), width = 9, height = 5)

