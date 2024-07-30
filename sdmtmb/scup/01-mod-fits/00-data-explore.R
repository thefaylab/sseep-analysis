
# 01a - PREPARE SCUP DATA ####


## LOAD PACKAGES ####
suppressPackageStartupMessages(library(tidyverse)) 
library(here)
library(sf) 
library(sdmTMB)
library(gghighlight)
library(ggsflabel)

here()

## LOAD DATA ####
# raw bottom trawl data; contains present only observations and will be used to pull depth, bottom temperature, and area swept values for each station below.  

#complete <- readRDS(here("data", "rds", "completed_bts_data.rds"))


scup_complete0 <- readRDS(here("data", "rds", "completed_bts_data.rds")) |> 
  filter(SVSPP == 143, 
         EST_YEAR %in% c(2009:2016,2018,2019,2021,2022)) |> 
  mutate(EXPCATCHWT = ifelse(is.na(EXPCATCHWT), 0, EXPCATCHWT))


temp_data <- read.csv(here("data","raw-data","NEFSC_BTS_ALLCATCHES.csv")) |> 
  dplyr::select(CRUISE6, STATION, STRATUM, BOTTEMP, EST_YEAR) |>
  filter(EST_YEAR %in% c(2009:2016,2018,2019,2021,2022)) |>
  group_by(STRATUM, CRUISE6, STATION) |>
  distinct() |>
  mutate(code = str_c(STRATUM, CRUISE6, STATION))


scup_complete <- scup_complete0 |>
  mutate(code = str_c(STRATUM, CRUISE6, STATION)) |>
  left_join(temp_data, by="code") |>
  select(!c(STRATUM.y,CRUISE6.y, STATION.y, EST_YEAR.y)) |>
  rename(STRATUM = STRATUM.x, CRUISE6 = CRUISE6.x, STATION = STATION.x, EST_YEAR = EST_YEAR.x)

# read the strata shapefile in to plot the polygons based on proportion and overlay the tow points
strata <- readRDS(here("data", "rds", "active_strata.rds")) 
# wind areas 
wind_areas <- readRDS(here("data", "rds", "all_wind_areas_Jun2022.rds")) 
wind_areas2 <- readRDS(here("data", "rds", "all_wind_areas_Jan2023.rds")) 

coastline <- sf::st_read(dsn = here("gis", "eastern_coast_UTM.shp"))


scup_complete2 <- full_join(scup_complete, strata, by = "STRATUM")   #join by STRATA to obtain depth ranges

scup_fall <- scup_complete2 |> filter(SEASON == "FALL")
scup_spring <- scup_complete2 |> filter(SEASON == "SPRING")


ggplot(strata) + 
  geom_sf(aes(fill = as.factor(Depth_m), geometry=Shape)) +  
  scale_fill_manual(values = c("#d48a3a", "#afdb88", "#21c99b", "#e2b29b", "#ff6fcf","#e8f5cc","#6274db","#80e6e6","#b0b1fa")) 


ggplot(strata) + 
  geom_sf(aes(fill = as.factor(Region), geometry=Shape)) +  
  scale_fill_manual(values = c("blue", "red","green", "yellow")) 



summary(scup_fall$AVGDEPTH)
summary(scup_spring$AVGDEPTH)

##FALL
z.prop.fall <- scup_fall |> 
  filter(!is.na(AVGDEPTH)) |>
  mutate(depcat = cut(AVGDEPTH, breaks = seq(0,380,10))) |>
         group_by(depcat) |>  
         summarize(prop_zero = mean(EXPCATCHWT==0))

z.prop.fall2 <- scup_fall |> 
  filter(!is.na(AVGDEPTH)) |>
  group_by(Depth_m) |>  
  summarize(prop_zero = mean(EXPCATCHWT==0))

z.prop.strat.fall <- scup_fall |> 
  filter(!is.na(AVGDEPTH)) |>
  group_by(STRATUM,Region) |>  
  summarize(prop_zero = mean(EXPCATCHWT==0))

z.prop.fall.depth <- scup_fall |> 
  filter(!is.na(AVGDEPTH)) |>
  group_by(AVGDEPTH) |>  
  reframe(prop_zero = mean(EXPCATCHWT==0))

cummcatch.fall.depth <- scup_fall |> 
  filter(!is.na(AVGDEPTH)) |>
  group_by(AVGDEPTH) |>  
  reframe(prop_zero = mean(EXPCATCHWT==0))


#PLOTS FALL
ggplot(z.prop.fall) +
  geom_point(aes(x =depcat , y = prop_zero)) +
  labs(x = "Depth cat (m)", y = "Proportion of zeros", subtitle = "Fall", 
       title = "Proportion of zeros by depth bins") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  ylim(0,1)


z.prop.fall2 |>
  ggplot() +
  geom_point(aes(x =reorder(Depth_m, prop_zero) , y = prop_zero, color=Depth_m), size=3) + 
  scale_color_manual(values = c("18-27"="#21c99b","27-46"="#e2b29b",
                                "9-35"="#b0b1fa","9-27"="#80e6e6",
                                "27-55"="#ff6fcf","46-55"="#e8f5cc",
                                "55-110"="#6274db","110-183"="#afdb88", 
                                ">183"="#d48a3a")) +
  theme_bw() + theme(legend.position="none") +
  labs(title = "Proportion of zeros by depth category", 
       subtitle = "Fall",
       x = "Depth cat (m)", y = "Proportion of zeros") +
  ylim(0,1)


ggplot(z.prop.strat.fall) +
  geom_point(aes(x =reorder(STRATUM, prop_zero) , y = prop_zero, color=as.factor(Region)),size=2) +
  scale_color_manual(values = c("1"="blue","3"="red",
                                "7"="green","8"="yellow")) +
  labs(x = "Stratum", y = "Proportion of zeros", subtitle = "Fall",
       title = "Proportion of zeros by stratum") +
  theme(axis.text.x = element_text(angle = 90, size = 7))+
  ylim(0,1)


ggplot(z.prop.fall.depth) +
  geom_point(aes(x =AVGDEPTH , y = prop_zero)) +
  labs(x = "Avg Depth (m)", y = "Pro zeroes", subtitle = "Fall", 
       title = "Proportion of zeros by depth") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  ylim(0,1)
       
#SPRING

z.prop.spring <- scup_spring |> 
  filter(!is.na(AVGDEPTH)) |>
  mutate(depcat = cut(AVGDEPTH, breaks = seq(0,380,10))) |>
  group_by(SEASON, depcat) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

z.prop.spring2 <- scup_spring |> 
  filter(!is.na(AVGDEPTH)) |>
  #mutate(depcat = cut(AVGDEPTH, breaks = seq(0,380,10))) |>
  group_by(Depth_m) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

z.prop.strat.spring <- scup_spring |> 
  filter(!is.na(AVGDEPTH)) |>
  #mutate(depcat = cut(AVGDEPTH, breaks = seq(0,380,10))) |>
  group_by(STRATUM, Region) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

##PLOTS SPRING
ggplot(z.prop.spring) +
  geom_point(aes(x =depcat , y = prop_zero)) +
  labs(x = "Depth cat (m)", y = "Proportion of zeros", subtitle = "Spring", 
       title = "Proportion of zeros by depth bins") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  ylim(0,1)


z.prop.spring2 |>
  ggplot() +
  geom_point(aes(x =reorder(Depth_m, prop_zero) , y = prop_zero, color=Depth_m), size=3) + 
  scale_color_manual(values = c("18-27"="#21c99b","27-46"="#e2b29b",
                                "9-35"="#b0b1fa","9-27"="#80e6e6",
                                "27-55"="#ff6fcf","46-55"="#e8f5cc",
                                "55-110"="#6274db","110-183"="#afdb88", 
                                ">183"="#d48a3a")) +
  theme_bw() + theme(legend.position="none") +
  labs(title = "Proportion of zeros by depth category", 
       subtitle = "Spring",
       x = "Depth cat (m)", y = "Proportion of zeros") +
  ylim(0,1)


ggplot(z.prop.strat.spring) +
  geom_point(aes(x =reorder(STRATUM, prop_zero) , y = prop_zero, color=as.factor(Region)),size=2) +
  scale_color_manual(values = c("1"="blue","3"="red",
                                "7"="green","8"="yellow")) +
  labs(x = "Stratum", y = "Proportion of zeros", subtitle = "Spring",
       title = "Proportion of zeros by stratum") +
  theme(axis.text.x = element_text(angle = 90, size = 7))+
  ylim(0,1)



###cum catch




### just MAB ####
### 

mab_strata <- c(3450, 1050, 1610, 1090, 3410, 3380, 3020, 3460 ,3050, 3440, 3260, 3350, 8510, 1010,
 1060, 3080, 3230, 3320, 3290, 8500, 1650, 1690, 7520, 1100, 3110, 3140, 3170, 1020,1740,1700,1730,3200, 1660,1620,1110,1070,1030,1750,1710,1670,1630,8520,1120,1080,1040,1760,1720,1680,1640,8530)
length(mab_strata)

mab_plot <- ggplot() + 
  geom_sf(data = strata, fill = "white", color = "grey80") + 
  geom_sf(data = coastline, fill = "grey60", color = "black") + 
  geom_sf(data = subset(strata, (STRATUM %in% mab_strata)), fill = "steelblue2")+ 
  scale_fill_distiller(palette = 8) +
  coord_sf() 

ggplot(strata) + 
  geom_sf(aes(fill = as.factor(Depth_m), geometry=Shape)) +  
  scale_fill_manual(values = c("#d48a3a", "#afdb88", "#21c99b", "#e2b29b", "#ff6fcf",
                               "#e8f5cc","#6274db","#80e6e6","#b0b1fa")) +
  geom_sf(data = subset(strata, !(STRATUM %in% mab_strata)), color="grey20", alpha=0.8) 

ggplot(strata) + 
  geom_sf(aes(fill = as.factor(Region), geometry=Shape)) +  
  scale_fill_manual(values = c("blue", "red","green", "yellow"))  +
  geom_sf(data = subset(strata, !(STRATUM %in% mab_strata)), color="grey20", alpha=0.8) 



scup_fall_mab <- scup_complete2 |> filter(SEASON == "FALL",
                                      STRATUM %in% mab_strata)
scup_spring_mab <- scup_complete2 |> filter(SEASON == "SPRING",
                                        STRATUM %in% mab_strata)


summary(scup_fall_mab$AVGDEPTH)
summary(scup_spring_mab$AVGDEPTH)

##FALL
z.prop.fall.mab <- scup_fall_mab |> 
  filter(!is.na(AVGDEPTH)) |>
  mutate(depcat = cut(AVGDEPTH, breaks = seq(0,380,10))) |>
  group_by(depcat) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

z.prop.fall2.mab <- scup_fall_mab |> 
  filter(!is.na(AVGDEPTH)) |>
  group_by(Depth_m) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

z.prop.strat.fall.mab <- scup_fall_mab |> 
  filter(!is.na(AVGDEPTH)) |>
  group_by(STRATUM, Region) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

#PLOTS FALL
ggplot(z.prop.fall.mab) +
  geom_point(aes(x =depcat , y = prop_zero)) +
  labs(x = "Depth cat (m)", y = "Proportion of zeros", subtitle = "Fall (MAB only)", 
       title = "Proportion of zeros by depth bins") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  ylim(0,1)


z.prop.fall2.mab |>
  ggplot() +
  geom_point(aes(x =reorder(Depth_m, prop_zero) , y = prop_zero, color=Depth_m), size=3) + 
  scale_color_manual(values = c("18-27"="#21c99b","27-46"="#e2b29b",
                                "9-35"="#b0b1fa","9-27"="#80e6e6",
                                "27-55"="#ff6fcf","46-55"="#e8f5cc",
                                "55-110"="#6274db","110-183"="#afdb88", 
                                ">183"="#d48a3a")) +
  theme_bw() + theme(legend.position="none") +
  labs(title = "Proportion of zeros by depth category", 
       subtitle = "Fall (MAB only)",
       x = "Depth cat (m)", y = "Proportion of zeros") +
  ylim(0,1)


ggplot(z.prop.strat.fall.mab) +
  geom_point(aes(x =reorder(STRATUM, prop_zero) , y = prop_zero, color=as.factor(Region)),size=2) +
  scale_color_manual(values = c("1"="blue","3"="red",
                                "7"="green","8"="yellow")) +
  labs(x = "Stratum", y = "Proportion of zeros", subtitle = "Fall (MAB only)",
       title = "Proportion of zeros by stratum") +
  theme(axis.text.x = element_text(angle = 90, size = 7))+
  ylim(0,1)

#SPRING

z.prop.spring.mab <- scup_spring_mab |> 
  filter(!is.na(AVGDEPTH)) |>
  mutate(depcat = cut(AVGDEPTH, breaks = seq(0,380,10))) |>
  group_by(SEASON, depcat) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

z.prop.spring2.mab <- scup_spring_mab |> 
  filter(!is.na(AVGDEPTH)) |>
  group_by(Depth_m) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

z.prop.strat.spring.mab <- scup_spring_mab |> 
  filter(!is.na(AVGDEPTH)) |>
  group_by(STRATUM,Region) |>  
  summarize(prop_zero = mean(EXPCATCHNUM==0))

##PLOTS SPRING
ggplot(z.prop.spring.mab) +
  geom_point(aes(x =depcat , y = prop_zero)) +
  labs(x = "Depth cat (m)", y = "Proportion of zeros", subtitle = "Spring (MAB only)", 
       title = "Proportion of zeros by depth bins") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  ylim(0,1)


z.prop.spring2.mab |>
  ggplot() +
  geom_point(aes(x =reorder(Depth_m, prop_zero) , y = prop_zero, color=Depth_m), size=3) + 
  scale_color_manual(values = c("18-27"="#21c99b","27-46"="#e2b29b",
                                "9-35"="#b0b1fa","9-27"="#80e6e6",
                                "27-55"="#ff6fcf","46-55"="#e8f5cc",
                                "55-110"="#6274db","110-183"="#afdb88", 
                                ">183"="#d48a3a")) +
  theme_bw() + theme(legend.position="none") +
  labs(title = "Proportion of zeros by depth category", 
       subtitle = "Spring (MAB only)",
       x = "Depth cat (m)", y = "Proportion of zeros") +
  ylim(0,1)



ggplot(z.prop.strat.spring.mab) +
  geom_point(aes(x =reorder(STRATUM, prop_zero) , y = prop_zero, color=as.factor(Region)),size=2) +
  scale_color_manual(values = c("1"="blue","3"="red",
                                "7"="green","8"="yellow")) +
  labs(x = "Stratum", y = "Proportion of zeros", subtitle = "Spring (MAB only)",
       title = "Proportion of zeros by stratum") +
  theme(axis.text.x = element_text(angle = 90, size = 7))+
  ylim(0,1)



###################################CATCHES


dist_wt_fall <- ggplot(strata) + 
  geom_sf() +  
  geom_point(data=scup_fall_mab, aes(x = DECDEG_BEGLON, y = DECDEG_BEGLAT, size=EXPCATCHWT), 
             color = "firebrick3", shape = 20, alpha=0.3) +   
  scale_size(range = c(.01,10)) +
  labs(title = "Fall distribution of catches (wt)", x = "Long", y = "Lat") +
  facet_wrap(~YEAR)
#geom_sf(data = subset(strata, STRATUM %in% c(3200)), fill = "red") 



scup_fall_mab <- scup_fall_mab |> 
  filter(!is.na(AVGDEPTH)) |>
  mutate(depcat = cut(AVGDEPTH, breaks = seq(0,380,10)))
 
scup_spring_mab <- scup_spring_mab |> 
  filter(!is.na(AVGDEPTH)) |>
  mutate(depcat = cut(AVGDEPTH, breaks = seq(0,380,10)))

scup_mab <- rbind(scup_fall_mab, scup_spring_mab)



scup_mab |> ggplot(aes(x=depcat, y=EXPCATCHNUM)) +
  facet_wrap(~SEASON) +
  geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
  labs(x = "Depth bin (m)", y = "Catch (number)", subtitle = "Mid Atlantic", 
       title = "Catch per depth bins") +
  theme(axis.text.x = element_text(angle = 90, size = 7)) 


scup_mab |> group_by(depcat, SEASON) |> 
  summarise(tows = length(unique(TOWID))) |>
  ggplot() + 
  geom_point(aes(x=depcat, y=tows)) +
  labs(x = "Depth bin (m)", y = "Number of tows", subtitle = "Mid Atlantic", 
       title = "Number of tows per depth bin") +
  facet_wrap(~SEASON) +
  theme(axis.text.x = element_text(angle = 90, size = 7))

  




scup_mab |> ggplot(aes(x=as.factor(STRATUM), y=EXPCATCHNUM, 
                       color=as.factor(Region),
                       fill = as.factor(Region))) + 
  geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
  scale_color_manual(values = c("1"="black","3"="black",
                                "7"="black","8"="black")) +
  scale_fill_manual(values = c("1"="blue","3"="red",
                                "7"="green","8"="yellow")) +
  labs(x = "Stratum", y = "Catch (number)", subtitle = "Mid Atlantic", 
       title = "Catch per stratum") +
  facet_wrap(~SEASON) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) 


scup_mab |> 
  group_by(STRATUM, SEASON, Region) |> 
  summarise(tows = length(unique(TOWID))) |>
  ggplot() + 
  geom_point(aes(x=as.factor(STRATUM), y=tows, 
  color=as.factor(Region)), size=1) +
  scale_color_manual(values = c("1"="blue","3"="red",
                               "7"="green","8"="yellow")) +
  labs(x = "Stratum", y = "Number of tows", subtitle = "Mid Atlantic", 
       title = "Number of tows per stratum") +
  facet_wrap(~SEASON) +
  theme(axis.text.x = element_text(angle = 90, size = 7))


scup_mab |> ggplot(aes(x=Depth_m, y=EXPCATCHNUM, 
                       color=as.factor(Depth_m),
                       fill = as.factor(Depth_m))) + 
  geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
  scale_color_manual(values = c("18-27"="grey20","27-46"="grey20",
                                "9-35"="grey20","9-27"="grey20",
                                "27-55"="grey20","46-55"="grey20",
                                "55-110"="grey20","110-183"="grey20", 
                                ">183"="grey20")) +
  scale_fill_manual(values = c("18-27"="#21c99b","27-46"="#e2b29b",
                                "9-35"="#b0b1fa","9-27"="#80e6e6",
                                "27-55"="#ff6fcf","46-55"="#e8f5cc",
                                "55-110"="#6274db","110-183"="#afdb88", 
                                ">183"="#d48a3a")) +
  labs(x = "Depth category (m)", y = "Catch (number)", subtitle = "Mid Atlantic", 
       title = "Catch per depth category") +
  facet_wrap(~SEASON) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) 


scup_mab |> 
  group_by(SEASON, Depth_m) |> 
  summarise(tows = length(unique(TOWID))) |>
  ggplot() + 
  geom_point(aes(x=Depth_m, y=tows, 
                 color=Depth_m), size=2) +
  scale_color_manual(values = c("18-27"="#21c99b","27-46"="#e2b29b",
                                 "9-35"="#b0b1fa","9-27"="#80e6e6",
                                 "27-55"="#ff6fcf","46-55"="#e8f5cc",
                                 "55-110"="#6274db","110-183"="#afdb88", 
                                 ">183"="#d48a3a")) +
  labs(x = "Depth category (m)", y = "Number of tows", subtitle = "Mid Atlantic", 
       title = "Number of tows per depth category") +
  facet_wrap(~SEASON) +
  theme(axis.text.x = element_text(angle = 90, size = 7))




###GAM
###
library(mgcv)
library(gratia)
library(ggplot2)
depth_model_fall <- gam(EXPCATCHNUM ~ s(AVGDEPTH) , data = scup_fall,
                   method = "REML")
mf<-summary(depth_model_fall)
mf$p.table
mf$s.table


depth_model_spring <- gam(EXPCATCHNUM ~ YEAR + s(AVGDEPTH), data = scup_spring,
                        method = "REML")
ms <- summary(depth_model_spring)
ms$p.table
ms$s.table


draw(depth_model_fall)

plot(depth_model_fall, all.terms = TRUE)
plot(depth_model_spring, all.terms = TRUE)



depth_model2_fall <- gam(EXPCATCHNUM ~ s(AVGDEPTH,bs="ts"), data = scup_fall_mab, method = "REML")


m2f <- summary(depth_model2_fall)
m2f$p.table
m2f$s.table




depth_model2_spring <- gam(EXPCATCHNUM ~ s(AVGDEPTH,bs="ts"), data = scup_spring_mab,
                          method = "REML")
m2s <- summary(depth_model2_spring)
m2s$p.table
m2s$s.table


plot(depth_model2_fall, all.terms = TRUE)
plot(depth_model_spring, all.terms = TRUE)
AIC(depth_model_fall,depth_model2_fall)
