
strata <- readRDS(here(sseep.dir, "data", "rds", "active_strata.rds"))

# read in fall data for model fitting created here("sdmtmb", "sumflounder", "01-mod-fits", "01-prepare-data.R")
sf_fall <- readRDS(here(sdmtmb.dir, "data", "sumflounder_fall.rds"))

# read in the fall mesh for model fitting created here("sdmtmb", "sumflounder", "01-mod-fits", "01-prepare-data.R")
fall_mesh <- readRDS(here(sdmtmb.dir, "data", "fall_mesh.rds"))

# define extra years to forecast 
fall_extra_years <- c(2020, 2022:2026)

fall_grid <- readRDS(here(sdmtmb.dir, "data", "sf_fall_grid_Jun2022.rds"))

gaus_mod <- sdmTMB(EXPCATCHWT ~ poly(AVGDEPTH, 2) + 
                     #as.factor(EST_YEAR) + 
                     as.factor(AREA),#-1,
                   data = sf_fall,
                   mesh = fall_mesh,
                   family = gaussian(),
                   spatial = "on", 
                   time = "EST_YEAR",
                   spatiotemporal = "IID", 
                   extra_time = fall_extra_years, 
                   control = sdmTMBcontrol(newton_loops = 1), 
                   silent = FALSE) 

fall_grid <- sdmTMB::replicate_df(fall_grid, "EST_YEAR", c(2009:2026)) 

# predictions
gaus_preds <- predict(gaus_mod, newdata = fall_grid)#,


future_years <- c(2022:2026)

# get a dataframe containing the values for the random effects fields for the fitted values of the data points
gaus_fit_preds <- predict(gaus_mod)

# LOOP #### 
#for (i in seq(1:5)){
  
  ### DATA WRANGLE ####
  # randomly select 5 years from the historical time period, 2020 is missing for the original observations and should not be included as an option
  resampled_years <- sample(c(2009:2019,2021), size = 5, replace = TRUE)
  
  # use locations from the resampled years as the observations in future years
  # nested loop to filter each resampled year individually and bind together into empty storage filter for instances where a year is replaced and resampled
  future_preds2 <- data.frame() # empty storage dataframe for filtered tow locations
  
  for(t in seq_along(resampled_years)){
    filtered_yr2 <- gaus_fit_preds |> 
      filter(EST_YEAR %in% resampled_years[t]) |> 
      mutate(EST_YEAR = future_years[t])
    
    future_preds2 <- bind_rows(future_preds2, filtered_yr2) 
    
  }
 
  
  # future_preds <- future_preds |> 
  #   mutate(ID = paste(STRATUM,CRUISE6,STATION,sep="_")) |>
  #   group_by(ID) |>
  #   mutate(TOWID = cur_group_id()) |>
  #   ungroup() 
  
  # extract unique tow information for binding later 
  gaus_tows <- future_preds2 |>
    select(X, Y, STRATUM, CRUISE6, STATION, EST_YEAR, AREA, AREA_CODE, SEASON, AVGDEPTH)#, TOWID)
  
  # extract the spatial effect estimates for each tow location for the future years 
  #future_omegas2 <- future_preds2$omega_s #future_tows |> 
  # select(TOWID, omega_s) |> 
  # mutate(TOWID = paste(TOWID, seq(nrow(future_tows)),sep=".")) |>
  # column_to_rownames(var = "TOWID")
  
  
  # filter the response and spatial predictions made across the grid cell here("sdmtmb", "sumflounder", "03-mod-predictions", "01a-fall-forecasts.R"), for the future years. 
  gaus_grid <- gaus_preds |> 
    filter(EST_YEAR %in% future_years) #|> 
    #mutate(EXPCATCHWT = est)
  # mutate(ID = paste(STRATUM,cell,sep="_")) |>
  # group_by(ID) |>
  # mutate(CELLID = cur_group_id()) |>
  # ungroup()
  
  # extract uniqueu grid information for binding later
  gaus_grid_info <- gaus_preds |> 
   select(!c(est, est_non_rf, est_rf, omega_s, epsilon_st))
  
  # extract the spatial effect estimates in each cell for the future years
  #grid_omegas2 <- grid_preds2$omega_s #|>
  # select(CELLID, omega_s) |>
  # column_to_rownames(var = "CELLID")
  
  ### SIMULATION DATA ####
  # bind the future tow locations and the future grid cells to create an overall location dataframe where new response values will simulated at
  gaus_data <- bind_rows(future_preds2, gaus_grid) |> 
    rename(omegas = omega_s)
  
  # bind the spatial effect estimates from the future years and the grid to fix the spatial random effects
  #omegas2 <- append(future_omegas2, grid_omegas2) |> as.matrix()
  
  # create a mesh from the binded simulation data 
  gaus_mesh <- make_mesh(gaus_data, xy_cols = c("X", "Y"), cutoff = 10)
  #plot(mesh)
  
  ## BASE SCENARIO ####
  base.sim.gaus <- sdmTMB_simulate(
    #EXPCATCHWT~poly(AVGDEPTH,2) + as.factor(AREA) + omegas,
    formula = ~1 +
      poly(AVGDEPTH, 2) +
      as.factor(AREA) +
      omegas,
    data = gaus_data,
    mesh = gaus_mesh,
    family = gaussian(),
    #spatial = "on",
    time = "EST_YEAR",
    #spatiotemporal = "iid",
    B = c(#0, 
          3.33,
          #0, 
          -37.3, 
          #0, 
          -26.3, 
          #0, 
          1.82, 
          1), # coefficient estimates; reduction = half of estimated wind coefficient (0.09); subtractive -> log(pred.effect/2) = log(pred.effect) - log(2)
    range = 28.9, 
    #rho = 0.158, # AR1 correlation
    sigma_O = 0,#5.41,
    sigma_E = 0, #5.77,
    phi = 4.11)#, # dispersion parameter
    #tweedie_p = 1.5)#, # tweedie power parameter 
    #fixed_re = list(omega_s = omegas2, epsilon_st = NULL, zeta_s = NULL))
  
  ### BASE SIMULATION DATA WRANGLING ####
  # extract the simulated data pertaining to the future tow locations
  # base_gaus_towdat <- right_join(base.sim.gaus, gaus_tows, by = c("X", "Y", "EST_YEAR")) |> 
  #   rename(EXPCATCHWT = observed) #|> 
    # mutate(SVSPP = 103, 
    #        rep = i)
  
  # add to simulated tow dataframe
  #sim_base_tows2 <- bind_rows(sim_base_tows2, base_towdat2)
  
  # extract the simulated data pertaining to the future grid cells
  base_gaus_grid <- right_join(base.sim.gaus, gaus_grid, by = c("X", "Y", "EST_YEAR")) #|>
    #rename(EXPCATCHWT = observed) #|> 
   # mutate(rep = i)
  
  
  
  head(base_gaus_grid |> select(X,Y,cell,EST_YEAR, mu, eta, observed, est, omegas))
  # head(grid_preds |> select(X,Y,cell,EST_YEAR, est, est_non_rf, est_rf, omega_s))
  # add to simulated grid dataframe
  #sim_base_grid2 <- bind_rows(sim_base_grid2, base_griddat2)
  
  ## INCREASE SCENARIO ####
#   inc.sim2 <- sdmTMB_simulate(
#     formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
#     data = data2,
#     mesh = mesh2,
#     family = gaussian(link = "log"),
#     spatial = "on",
#     time = "EST_YEAR",
#     spatiotemporal = "iid", 
#     B = c(-1.46, -60.7, -20.2, (0.08+log(2))), # coefficient estimates; reduction = half of estimated wind coefficient (0.09); subtractive -> log(pred.effect/2) = log(pred.effect) - log(2)
#     range = 17.9, 
#     #rho = 0.158, # AR1 correlation
#     sigma_O = 2.08,
#     sigma_E = 2.84,
#     phi = 0.387, # dispersion parameter
#     #tweedie_p = 1.26, # tweedie power parameter 
#     fixed_re = list(omega_s = omegas2, epsilon_st = NULL, zeta_s = NULL))
#   
#   
#   ### INCREASE SIMULATION DATA WRANGLING ####
#   # extract the simulated data pertaining to the future tow locations
#   inc_towdat2 <- right_join(inc.sim2, future_tows2, by = c("X", "Y", "EST_YEAR")) |> 
#     rename(EXPCATCHWT = observed) |> 
#     mutate(SVSPP = 103, 
#            rep = i)
#   
#   # add to simulated tow dataframe
#   sim_increase_tows2 <- bind_rows(sim_increase_tows2, inc_towdat2)
#   
#   # extract the simulated data pertaining to the future grid cells
#   inc_griddat2 <- right_join(inc.sim2, grid_info2, by = c("X", "Y", "EST_YEAR")) |>
#     rename(EXPCATCHWT = observed) |> 
#     mutate(rep = i)
#   
#   # add to simulated grid dataframe
#   sim_increase_grid2 <- bind_rows(sim_increase_grid2, inc_griddat2)
#   
#   ## DECREASE SCENARIO ####
#   dec.sim2 <- sdmTMB_simulate(
#     formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
#     data = data2,
#     mesh = mesh2,
#     family = gaussian(link = "log"),
#     spatial = "on", 
#     time = "EST_YEAR",
#     spatiotemporal = "iid",
#     B = c(-1.46, -60.7, -20.2, (0.08-log(2))), # coefficient estimates; reduction = half of estimated wind coefficient (0.09); subtractive -> log(pred.effect/2) = log(pred.effect) - log(2)
#     range = 17.9, 
#     #rho = 0.158, # AR1 correlation
#     sigma_O = 2.08,
#     sigma_E = 2.84,
#     phi = 0.387, # dispersion parameter
#     #tweedie_p = 1.26, # tweedie power parameter 
#     fixed_re = list(omega_s = omegas2, epsilon_st = NULL, zeta_s = NULL))
#   
#   ### DECREASE SIMULATION DATA WRANGLING ####
#   # extract the simulated data pertaining to the future tow locations
#   dec_towdat2 <- right_join(dec.sim2, future_tows2, by = c("X", "Y", "EST_YEAR")) |> 
#     rename(EXPCATCHWT = observed) |> 
#     mutate(SVSPP = 103, 
#            rep = i)
#   
#   # add to simulated tow dataframe
#   sim_decrease_tows2 <- bind_rows(sim_decrease_tows2, dec_towdat2)
#   
#   # extract the simulated data pertaining to the future grid cells
#   dec_griddat2 <- right_join(dec.sim2, grid_info2, by = c("X", "Y", "EST_YEAR")) |>
#     rename(EXPCATCHWT = observed) |> 
#     mutate(rep = i)
#   
#   # add to simulated grid dataframe
#   sim_decrease_grid2 <- bind_rows(sim_decrease_grid2, dec_griddat2)
#   
# }
# 
# 
# ggplot() +
#   geom_sf(data = coastline, fill = "#efe5c7", color = "#816c62") +
#   geom_sf(data = strata_utm, fill = NA) +
#   geom_tile(data = sim_base_grid, mapping = aes(X*1000, Y*1000, fill = omega_s), width = 10000, height = 10000) +
#   #scale_fill_viridis_c(trans = "sqrt") +
#   labs(x = "Longitude", y = "Latitude") + 
#   theme(panel.grid = element_blank()) 
# 
# ggplot() +
#   geom_sf(data = coastline, fill = "#efe5c7", color = "#816c62") +
#   geom_sf(data = strata_utm, fill = NA) +
#   geom_tile(data = fall_preds, mapping = aes(X*1000, Y*1000, fill = omega_s), width = 10000, height = 10000) +
#   #scale_fill_viridis_c(trans = "sqrt")+
#   labs(x = "Longitude", y = "Latitude") + 
#   theme(panel.grid = element_blank()) 




