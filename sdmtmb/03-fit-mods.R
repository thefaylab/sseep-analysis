### created: 10/15/2024
### updated: 

# 03 - Fit models ####

## OBJECTIVE ####
# fit various forms of models using prepared seasonal species data
# include predictor for index standardization 
# estimate a coefficient for each year 
# include inside/outside wind area designation as part of the predictor

### Load Packages #### 
library(here)
suppressPackageStartupMessages(library(tidyverse))
library(sdmTMB) 

here()

### Environment Set-Up ####
# season
season <- "fall"

# species
species <- "sumflounder"


### File locations ####
# species folder 
dat.files <- here("data", "rds", "sdmtmb", species) 

# model locations
mods.dat <- here(dat.files, "mods", season)

### Read in data ####
# dataset containing complete observations for species based on their generated seasonal spatial footprint. 
data <- readRDS(here(dat.files, str_c(species, "_", season, ".rds", sep = "")))

# mesh
mesh <- readRDS(here(dat.files, str_c(season, "mesh.rds", sep = "_")))

# model formulas 
formulas <- list("EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR - 1", 
             "EXPCATCHWT ~ poly(AVGDEPTH, 2) + EST_YEAR - 1",
             "EXPCATCHWT ~ s(AVGDEPTH) + EST_YEAR + AREA - 1", 
             "EXPCATCHWT ~ poly(AVGDEPTH,2) + EST_YEAR + AREA - 1")


## Fit Models ####
### No Random Effects Models ####
nore_mods <- map(formulas, ~sdmTMB(formula = .x, 
                                   data = data, 
                                   mesh = mesh, 
                                   family = tweedie(link = "log"), 
                                   spatial = "off", 
                                   control = sdmTMBcontrol(loop = 1), 
                                   silent = F)
                 )

# check convergence of models
map(nore_mods, ~sanity(.))


### Spatial Random Effects Models ####
spatial_mods <- map(formulas, ~sdmTMB(formula = .x, 
                                   data = data, 
                                   mesh = mesh, 
                                   family = tweedie(), 
                                   spatial = "on", 
                                   control = sdmTMBcontrol(loop = 1), 
                                   silent = F)
)

# check convergence of models 
map(spatial_mods, ~sanity(.))


### Spatiotemporal Random Effect Models ####
st_mods <- map(formulas, ~sdmTMB(formula = .x, 
                              data = data, 
                              mesh = mesh, 
                              family = tweedie(), 
                              spatial = "on", 
                              time = "EST_YEAR",
                              spatiotemporal = "IID",
                              control = sdmTMBcontrol(loop = 1), 
                              silent = F)
)

# check convergence of models
map(st_mods, ~sanity(.))


## Save the data #### 
# create one large list object to iterate over 
all_mods <- append(nore_mods, c(spatial_mods, st_mods))

# give each model a name based on its location in the sequence of list items 
names(all_mods) <- map(seq(dim(all_mods)), ~str_c("m", .x, sep = ""))

# save each list item according to its name in the model folder 
pmap(list(all_mods, names(all_mods)), ~saveRDS(.x, here(mods.dat, str_c(.y, "_", season, ".rds", sep = ""))))

