
library(here)
library(dplyr)
library(tidyr)
library(tidyverse)
library(jaccard)


data <- readRDS(here("data/merged_data_complete.rds"))
geounits <- read.csv(here("data/mergedpresence3.csv"), sep=";")

geounits <- geounits %>% 
  select(STRATUM, CRUISE6, STATION, GEO_AREA) %>% 
  group_by(STRATUM, CRUISE6, STATION) %>% 
  distinct() %>% 
  mutate(code = str_c(STRATUM, CRUISE6, STATION))

#Cross data with Ecological Production Units (MAB, GOM, GB, SS)
data <- data %>% mutate(code = str_c(STRATUM, CRUISE6, STATION)) %>%
  left_join(geounits, by="code")


#Filtering for Mid Atlantic Bight fall/spring
data_MAB_fall <- data %>%
  filter(GEO_AREA == "MAB", SEASON == "FALL")


data_MAB_spring <- data %>%
  filter(GEO_AREA == "MAB", SEASON == "SPRING")

#testing for one spp
bsb <- data_MAB_fall %>% 
  filter(SVSPP == 141) %>% 
  select(code, PRESENCE) %>% 
  distinct() %>% 
  rename("141" = PRESENCE)

bf <- data_MAB_fall %>% 
  filter(SVSPP == 131) %>% 
  select(code, PRESENCE) %>% 
  distinct() %>% 
  rename("131" = PRESENCE)

df <- left_join(bsb, bf, by = "code")


for (i in sp.names){
  n<- data_MAB_fall %>%
    filter(COMNAME == i) %>%
    select(code, PRESENCE) %>%
    distinct() %>%
    rename_at('PRESENCE', ~ as.character(i))
  
  st.df.fall <- st.df.fall %>%
    left_join(n, by = "code")
}
#Filtering for sp of interes
species <- c(141, 105, 106, 121,15,131,32,72,503,22,23,24,25,26, 103, 143) 
sp.names <- c("BLACK SEA BASS", "YELLOWTAIL FLOUNDER", "WINTER FLOUNDER", "ATLANTIC MACKEREL", "SPINY DOGFISH",
              "BUTTERFISH", "ATLANTIC HERRING", "SILVER HAKE", "LONGFIN SQUID", "BARNDOOR SKATE", "WINTER SKATE",
              "CLEARNOSE SKATE", "ROSETTE SKATE", "LITTLE SKATE", "SUMMER FLOUNDER", "SCUP")
  
st.df.fall <- data_MAB_fall %>% 
  select(code) %>% 
  unique()

for (i in sp.names){
  n<- data_MAB_fall %>% 
    filter(COMNAME == i) %>% 
    select(code, PRESENCE) %>% 
    distinct() %>%
    rename_at('PRESENCE', ~ as.character(i))
  
  st.df.fall <- st.df.fall %>%
    left_join(n, by = "code")
}

st.df.spring <- data_MAB_spring %>% 
  select(code) %>% 
  unique()

for (i in sp.names){
  n<- data_MAB_spring %>% 
    filter(COMNAME == i) %>% 
    select(code, PRESENCE) %>% 
    distinct() %>%
    rename_at('PRESENCE', ~ as.character(i))
  
  st.df.spring <- st.df.spring %>%
    left_join(n, by = "code")
}

#JACCARD SIMILARITY INDEX - FALL

#Data wrangling
st.df.fall <- subset (st.df.fall, select = -code)
#Create a jaccard index storage data frame
jac_fall <- data.frame(matrix(nrow=16,ncol=16)) 
colnames(jac_fall) <- c("bsb", "ytf", "winf", "atlm", "spdog", "bfish", "atlh", 
                        "silhk", "lfsq", "barsk", "winsk", "clesk", "rossk", "litsk", "sumf", "scup")
rownames(jac_fall) <- c("bsb", "ytf", "winf", "atlm", "spdog", "bfish", "atlh", 
                        "silhk", "lfsq", "barsk", "winsk", "clesk", "rossk", "litsk", "sumf", "scup")


#jaccard index of bsb against all the 15 spp
for (i in 1:ncol(st.df.fall)){
jac_fall[i,1]<-jaccard(st.df.fall[,1],st.df.fall[,i]) 
  print(jac_fall[i,1])
}

#jaccard index of ytf against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,2]<-jaccard(st.df.fall[,2],st.df.fall[,i]) 
  print(jac_fall[i,2])
}

#jaccard index of winf against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,3]<-jaccard(st.df.fall[,3],st.df.fall[,i]) 
  print(jac_fall[i,3])
}

#jaccard index of atlm against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,4]<-jaccard(st.df.fall[,4],st.df.fall[,i]) 
  print(jac_fall[i,4])
}

#jaccard index of spdog against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,5]<-jaccard(st.df.fall[,5],st.df.fall[,i]) 
  print(jac_fall[i,5])
}

#jaccard index of bfish against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,6]<-jaccard(st.df.fall[,6],st.df.fall[,i]) 
  print(jac_fall[i,6])
}

#jaccard index of atlh against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,7]<-jaccard(st.df.fall[,7],st.df.fall[,i]) 
  print(jac_fall[i,7])
}

#jaccard index of silhk against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,8]<-jaccard(st.df.fall[,8],st.df.fall[,i]) 
  print(jac_fall[i,8])
}

#jaccard index of lfsq against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,9]<-jaccard(st.df.fall[,9],st.df.fall[,i]) 
  print(jac_fall[i,9])
}

#jaccard index of barsk against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,10]<-jaccard(st.df.fall[,10],st.df.fall[,i]) 
  print(jac_fall[i,10])
}

#jaccard index of winsk against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,11]<-jaccard(st.df.fall[,11],st.df.fall[,i]) 
  print(jac_fall[i,11])
}

#jaccard index of clesk against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,12]<-jaccard(st.df.fall[,12],st.df.fall[,i]) 
  print(jac_fall[i,12])
}

#jaccard index of rossk against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,13]<-jaccard(st.df.fall[,13],st.df.fall[,i]) 
  print(jac_fall[i,13])
}

#jaccard index of litsk against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,14]<-jaccard(st.df.fall[,14],st.df.fall[,i]) 
  print(jac_fall[i,14])
}

#jaccard index of sumf against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,15]<-jaccard(st.df.fall[,15],st.df.fall[,i]) 
  print(jac_fall[i,15])
}

#jaccard index of scup against all the 15 spp
for (i in 1:ncol(st.df.fall)){
  jac_fall[i,16]<-jaccard(st.df.fall[,16],st.df.fall[,i]) 
  print(jac_fall[i,16])
}

jac_fall


#JACCARD SIMILARITY INDEX - SPRING

#Data wrangling
st.df.spring <- subset (st.df.spring, select = -code)
#Create a jaccard index storage data frame
jac_spring <- data.frame(matrix(nrow=16,ncol=16)) 
colnames(jac_spring) <- c("bsb", "ytf", "winf", "atlm", "spdog", "bfish", "atlh", 
                        "silhk", "lfsq", "barsk", "winsk", "clesk", "rossk", "litsk", "sumf", "scup")
rownames(jac_spring) <- c("bsb", "ytf", "winf", "atlm", "spdog", "bfish", "atlh", 
                        "silhk", "lfsq", "barsk", "winsk", "clesk", "rossk", "litsk", "sumf", "scup")

jac_spring
#jaccard index of bsb against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,1]<-jaccard(st.df.spring[,1],st.df.spring[,i]) 
  print(jac_spring[i,1])
}

#jaccard index of ytf against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,2]<-jaccard(st.df.spring[,2],st.df.spring[,i]) 
  print(jac_spring[i,2])
}

#jaccard index of winf against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,3]<-jaccard(st.df.spring[,3],st.df.spring[,i]) 
  print(jac_spring[i,3])
}

#jaccard index of atlm against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,4]<-jaccard(st.df.spring[,4],st.df.spring[,i]) 
  print(jac_spring[i,4])
}

#jaccard index of spdog against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,5]<-jaccard(st.df.spring[,5],st.df.spring[,i]) 
  print(jac_spring[i,5])
}

#jaccard index of bfish against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,6]<-jaccard(st.df.spring[,6],st.df.spring[,i]) 
  print(jac_spring[i,6])
}

#jaccard index of atlh against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,7]<-jaccard(st.df.spring[,7],st.df.spring[,i]) 
  print(jac_spring[i,7])
}

#jaccard index of silhk against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,8]<-jaccard(st.df.spring[,8],st.df.spring[,i]) 
  print(jac_spring[i,8])
}

#jaccard index of lfsq against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,9]<-jaccard(st.df.spring[,9],st.df.spring[,i]) 
  print(jac_spring[i,9])
}

#jaccard index of barsk against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,10]<-jaccard(st.df.spring[,10],st.df.spring[,i]) 
  print(jac_spring[i,10])
}

#jaccard index of winsk against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,11]<-jaccard(st.df.spring[,11],st.df.spring[,i]) 
  print(jac_spring[i,11])
}

#jaccard index of clesk against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,12]<-jaccard(st.df.spring[,12],st.df.spring[,i]) 
  print(jac_spring[i,12])
}

#jaccard index of rossk against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,13]<-jaccard(st.df.spring[,13],st.df.spring[,i]) 
  print(jac_spring[i,13])
}

#jaccard index of litsk against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,14]<-jaccard(st.df.spring[,14],st.df.spring[,i]) 
  print(jac_spring[i,14])
}

#jaccard index of sumf against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,15]<-jaccard(st.df.spring[,15],st.df.spring[,i]) 
  print(jac_spring[i,15])
}

#jaccard index of scup against all the 15 spp
for (i in 1:ncol(st.df.spring)){
  jac_spring[i,16]<-jaccard(st.df.spring[,16],st.df.spring[,i]) 
  print(jac_spring[i,16])
}

jac_spring


write_csv(jac_fall, file = "jac_fall.csv")
write_csv(jac_spring, file = "jac_spring.csv")



### full data ###

st.df <- data %>% 
  select(code) %>% 
  unique()

for (i in sp.names){
  n <- data %>%
    filter(COMNAME == i) %>%
    select(code, PRESENCE) %>%
    distinct() %>%
    rename_at('PRESENCE', ~ as.character(i))
  
  st.df <- st.df %>%
    left_join(n, by = "code")
}

#Filtering for sp of interes
species <- c(141, 105, 106, 121,15,131,32,72,503,22,23,24,25,26,27,28, 103, 143) 
sp.names <- c("BLACK SEA BASS", "YELLOWTAIL FLOUNDER", "WINTER FLOUNDER", "ATLANTIC MACKEREL", "SPINY DOGFISH",
              "BUTTERFISH", "ATLANTIC HERRING", "SILVER HAKE", "LONGFIN SQUID", "BARNDOOR SKATE", "WINTER SKATE",
              "CLEARNOSE SKATE", "ROSETTE SKATE", "LITTLE SKATE",  "SMOOTH SKATE", "THORNY SKATE","SUMMER FLOUNDER", "SCUP")



#Data wrangling
st.df <- subset (st.df, select = -code)
#Create a jaccard index storage data frame
jac <- data.frame(matrix(nrow=18,ncol=18)) 
colnames(jac) <- c("bsb", "ytf", "winf", "atlm", "spdog", "bfish", "atlh", 
                        "silhk", "lfsq", "barsk", "winsk", "clesk", "rossk", "litsk","smosk", "thosk","sumf", "scup")
rownames(jac) <- c("bsb", "ytf", "winf", "atlm", "spdog", "bfish", "atlh", 
                        "silhk", "lfsq", "barsk", "winsk", "clesk", "rossk", "litsk","smosk", "thosk","sumf", "scup")


#jaccard index of bsb against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,1]<-jaccard(st.df[,1],st.df[,i]) 
  print(jac[i,1])
}

#jaccard index of ytf against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,2]<-jaccard(st.df[,2],st.df[,i]) 
  print(jac[i,2])
}


#jaccard index of winf against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,3]<-jaccard(st.df[,3],st.df[,i]) 
  print(jac[i,3])
}

#jaccard index of atlf against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,4]<-jaccard(st.df[,4],st.df[,i]) 
  print(jac[i,4])
}

#jaccard index of spdog against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,5]<-jaccard(st.df[,5],st.df[,i]) 
  print(jac[i,5])
}

#jaccard index of bfish against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,6]<-jaccard(st.df[,6],st.df[,i]) 
  print(jac[i,6])
}


#jaccard index of atlh against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,7]<-jaccard(st.df[,7],st.df[,i]) 
  print(jac[i,7])
}


#jaccard index of silh against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,8]<-jaccard(st.df[,8],st.df[,i]) 
  print(jac[i,8])
}


#jaccard index of lfsq against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,9]<-jaccard(st.df[,9],st.df[,i]) 
  print(jac[i,9])
}


#jaccard index of barsk against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,10]<-jaccard(st.df[,10],st.df[,i]) 
  print(jac[i,10])
}


#jaccard index of winsk against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,11]<-jaccard(st.df[,11],st.df[,i]) 
  print(jac[i,11])
}


#jaccard index of clesk against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,12]<-jaccard(st.df[,12],st.df[,i]) 
  print(jac[i,12])
}

#jaccard index of rossk against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,13]<-jaccard(st.df[,13],st.df[,i]) 
  print(jac[i,13])
}


#jaccard index of litsk against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,14]<-jaccard(st.df[,14],st.df[,i]) 
  print(jac[i,14])
}


#jaccard index of smooth skate against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,15]<-jaccard(st.df[,15],st.df[,i]) 
  print(jac[i,15])
}

#jaccard index of thorny skate against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,16]<-jaccard(st.df[,16],st.df[,i]) 
  print(jac[i,16])
}

#jaccard index of sumf against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,17]<-jaccard(st.df[,17],st.df[,i]) 
  print(jac[i,17])
}

#jaccard index of scup against all the 17 spp
for (i in 1:ncol(st.df)){
  jac[i,18]<-jaccard(st.df[,18],st.df[,i]) 
  print(jac[i,18])
}

jac
write_csv(jac, file = "jac_complete.csv")

