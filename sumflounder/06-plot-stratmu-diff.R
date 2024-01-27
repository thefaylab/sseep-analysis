### created: 01/26/2024
### last updated: 

# 06 - PLOT SUMMER FLOUNDER MEAN DIFFERENCES ####


## OBJECTIVE ####
# plot the mean relative difference between survey effort scenario indices for summer flounder compared to the relative differences across all species observed on the bottom trawl survey



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
# dataset created from `03-strat-mu-diff.R` here("retro-analysis"). 
mudiff_all <- readRDS(here("data", "rds", "retro-analysis", "species_mean-sq-diff.rds"))

# dataset created from `05-calculate-sumflounder-stratmu.R` here("sumflounder"). 
mudiff_dat <- readRDS(here("data", "sumflounder", "sf_mudiffdat.rds"))

# create a lookup table with filename-friendly species names
# specieslookup <- data |>
#   ungroup() |>
#   select(SVSPP, COMNAME) |>
#   distinct() |>
#   mutate(spname = str_to_lower(gsub(" ", "_", COMNAME)))

## CALCULATE MEAN SQUARED RELATIVE DIFFERENCES #####  
# mudiff_dat <- data |> 
#   #mutate(stratmu = ifelse(stratmu==0, 1, stratmu)) |>
#   filter(EST_YEAR %in% c(2016:2019, 2021)) |> #filter for recent 5 years, skipping 2020
#   mutate(log_mu = log(stratmu)) |>
#   arrange(desc(stratmu)) |>
#   group_by(SVSPP, EST_YEAR, SEASON) |> #, 
#   #summarize(sq_diff = (exp(diff(log(stratmu)))-1)^2, .groups = "drop") |>
#   summarise(diff_mu = diff(stratmu)) |>
#   arrange(desc(diff_mu)) |>
#   mutate(exp_mu = (exp(diff_mu))-1) |>
#   arrange(desc(exp_mu)) |>
#   mutate(sq_diff = exp_mu^2) |>
#   arrange(desc(sq_diff))|>
#   ungroup()|>
#   #summarize(sq_diff = (diff(stratmu))^2, .groups = "drop") |> # calculate the relative differences and square them; drop the groups for further analysis
#   group_by(SVSPP, SEASON) |>
#   summarize(mudiff = mean(sq_diff), .groups = "drop") |> # calculate the average; drop the grouping factor 
#   mutate(mudiff = sqrt(mudiff)*100) |>
#   arrange(desc(mudiff)) |> # arrange highest to lowest 
#   left_join(specieslookup, by = "SVSPP") # add the species look up data 


# print the first 10 rows of the data 
# topten_fall <- mudiff_dat |>
#   filter(SEASON == "FALL") |>
#   head(25) |> 
#   mutate(mudiff = round(mudiff, 2), 
#          COMNAME = str_to_sentence(COMNAME)) |> 
#   rename(Species = COMNAME, 
#          "Relative Percent Difference" = mudiff)
# 
# fall_table <- kable(topten_fall[,c(4,3,2)], align = "lcccc", caption = "Ten Highest Percent Differences in Abundance Indices by Species", format.args = list(big.mark = ","), booktabs = TRUE) |>
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14) #|>
# 
# save_kable(fall_table, here("outputs", "fall_mudiff.png"))
# 
#   
# topten_spring <- mudiff_dat |>
#     filter(SEASON == "SPRING") |> 
#     head(25) |> 
#     mutate(mudiff = round(mudiff, 2), 
#            COMNAME = str_to_sentence(COMNAME)) |> 
#     rename(Species = COMNAME, 
#            "Relative Percent Difference" = mudiff)
# 
# 
# spring_table <- kable(topten_spring[,c(4,3,2)], align = "lcccc", caption = "Top Ten Mean Squared Differences by Species", format.args = list(big.mark = ","), booktabs = TRUE) |>
#   kable_styling(full_width = F, fixed_thead = T, font_size = 14)
# 
# save_kable(spring_table, here("outputs", "spring_mudiff.png"))


### save the data 
# saveRDS(mudiff_dat, file = here("data", "rds", "species_mean-sq-diff.rds"))


## PLOT THE DISTRIBUTION #####

# sf_mudiff <- mudiff_dat |>
#   filter(SVSPP == 103) |>
#   mutate(count = 1,
#          across(mudiff, round, 0), 
#          difference = str_c(as.character(mudiff), "%"))

# sf_mudiff_fall <- mudiff_all |> 
#   filter(SVSPP == 103, SEASON == "FALL")
# 
# sf_mudiff_spr <- mudiff_all |> 
#   filter(SEASON == "SPRING")

# mudiff_dat <- mudiff_dat |>
#   mutate(across(mudiff, round, 3))

# mudiff_plot <- ggplot() +  
#   geom_histogram(data = mudiff_all, aes(mudiff, after_stat(count)), color = "white", fill = "#5dc5e9") +
#   facet_wrap(~SEASON) +
#   labs(x = "Relative percent differences between preclusion and status quo annual stratified mean abundance indices", y = "Number of species") +
#   theme_bw()


# presentation plots
sf_mudiff_fall_plot <- ggplot() +  
  geom_histogram(data = mudiff_all |> filter(SEASON == "FALL"), aes(mudiff, after_stat(count)), color = "white", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(mudiff_dat[1,3]), linetype = 6, color = "#0a4c8a", linewidth = 1) + 
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Relative percent differences", y = "Number of species") +
  ylim(0, 125)+
  theme_bw() + 
  theme(axis.title = element_text(size = 16), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text = element_text(size = 14), strip.text = element_text(size = 14))

sf_mudiff_spr_plot <- ggplot() +  
  geom_histogram(data = mudiff_all |> filter(SEASON == "SPRING"), aes(mudiff, after_stat(count)), color = "white", fill = "#5dc5e9") +
  geom_vline(xintercept = as.numeric(mudiff_dat[2,3]), linetype = 6, color = "#0a4c8a", linewidth = 1) + 
  facet_wrap(~str_to_title(SEASON)) +
  labs(x = "Relative percent differences", y = "") +
  ylim(0,120) +
  theme_bw() + 
  theme(axis.title = element_text(size = 16), axis.title.x = element_text(margin = margin(5, 5, 5, 5)), axis.text.x = element_text(size = 16), axis.title.y = element_text(margin = margin(5, 5, 5, 5)), axis.text.y =  element_blank(), axis.ticks.y = element_blank(), strip.text = element_text(size = 14))

sf_mudiff_present <- ((sf_mudiff_fall_plot + theme(plot.margin = unit(c(5, 2, 5, 5), "pt"))) + (sf_mudiff_spr_plot + theme(plot.margin = unit(c(5,5,5,2), "pt")))) + plot_annotation(title = "Average squared relative differences of annual stratified mean abundance indices between survey effort scenarios", theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))



### save the plot
# ggsave(filename ="species_mean-sq-diff.png", plot = mudiff_plot, device = "png" , path = here("outputs", "plots"), width = 10, height = 8)

ggsave(filename ="sf_mudiff.png", device = "png", plot = sf_mudiff_plot, path = here("outputs", "sumflounder"), width = 10, height = 5)

ggsave(filename ="sf_mudiff_present.png", device = "png", plot = sf_mudiff_present, path = here("outputs", "sumflounder"), width = 12, height = 5)


