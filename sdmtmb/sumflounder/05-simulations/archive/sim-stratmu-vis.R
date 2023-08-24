
sdmtmb.dir <- "../sseep-analysis/sdmtmb"
sseep.dir <- "../sseep-analysis"
source(here(sseep.dir, "R", "StratMeanFXs_v2.R"))
# OBJECTIVE ####

## LOAD DATA ####
fall_sim_mu <- readRDS(here(sdmtmb.dir, "data", "fall_sim_mu.rds"))
fall_sim_mu5x <- readRDS(here(sdmtmb.dir, "data", "fall_sim_mu5x.rds"))
fall_sim_mu10x <- readRDS( here(sdmtmb.dir, "data", "fall_sim_mu10x.rds"))

spr_sim_mu <- readRDS(here(sdmtmb.dir, "data", "spr_sim_mu.rds"))
spr_sim_mu5x <- readRDS(here(sdmtmb.dir, "data", "spr_sim_mu5x.rds"))
spr_sim_mu10x <- readRDS(here(sdmtmb.dir, "data", "spr_sim_mu10x.rds"))

## FUNCTION ####
plot_stratmu <- function(df, ...){
  ggplot() + 
    geom_pointrange(data = df, aes(x = as.factor(EST_YEAR), y = stratmu, color = METHOD, shape = TYPE, ymin=lower, ymax = upper), position =  position_dodge2(width=0.4)) +
    facet_wrap(vars(SEASON), scales = "free_y") +
    scale_color_manual(values = c("#0a4c8a", "#3f7f00")) +
    labs(SEASON = "", TYPE = "", x = "", y = "", ...) +
    ylim(0,NA) +
    theme_bw() +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = -1),
          axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))
}

plot_stratmu(spr_sim_mu10x) + 
  labs(title = "10x", x = "Year", y = "Stratified Mean (kg/tow)")

# PATCHWORK ####
## base ####
fall_base <- plot_stratmu(fall_sim_mu) + 
  labs(title = "A", y = "Stratified Mean (kg/tow)")
spr_base <- plot_stratmu(spr_sim_mu)

## 5x ####
fall_5x <- plot_stratmu(fall_sim_mu5x) + 
  labs(title = "B", y = "Stratified Mean (kg/tow)")
spr_5x <- plot_stratmu(spr_sim_mu5x)

## 10x ####
fall_10x <- plot_stratmu(fall_sim_mu10x) + 
  labs(title = "C", x = "Year", y = "Stratified Mean (kg/tow)")
spr_10x <- plot_stratmu(spr_sim_mu10x) + 
  labs(x = "Year")

## FINAL ####
(fall_base + spr_base) /
  (fall_5x + spr_5x) /
  (fall_10x + spr_10x) +
  plot_layout(guides = "collect") &  
  theme(legend.position = "bottom")

