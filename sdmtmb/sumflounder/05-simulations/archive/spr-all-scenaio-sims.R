


# base loop
spr.base_sims <- data.frame()

for(i in seq(1)){
  
  x <- sdmTMB_simulate(
    formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
    data = future_data,
    mesh = spring_mesh,
    family = tweedie(link = "log"),
    time = "EST_YEAR",
    B = c(-0.06, 7.66, -27.7, 0.34), # coefficient estimates 
    range = 96.2, 
    #rho = 0.158, # AR1 correlation
    sigma_O = 1.15,
    sigma_E = 0.844,
    phi = 1.83, # dispersion parameter
    tweedie_p = 1.39, # tweedie power parameter 
    fixed_re = fixed_re)#,
  #seed = 42)#,
  #extra_time = fall_extra_years)
  
  base_dat <- bind_cols(x, spring_info) |> 
    rename(EXPCATCHWT = observed) |>
    mutate(SVSPP = 103, 
           rep = i)
  
  spr.base_sims <- bind_rows(spr.base_sims, base_dat)
  
}



# increase loop
spr.increase_sims <- data.frame()

for(i in seq(1:1000)){
  
  x <- sdmTMB_simulate(
    formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
    data = future_data,
    mesh = spring_mesh,
    family = tweedie(link = "log"),
    time = "EST_YEAR",
    B = c(-0.06, 7.66, -27.7, log(6)), # coefficient estimates 
    range = 96.2, 
    #rho = 0.158, # AR1 correlation
    sigma_O = 1.15,
    sigma_E = 0.844,
    phi = 1.83, # dispersion parameter
    tweedie_p = 1.39, # tweedie power parameter 
    fixed_re = fixed_re)#,
  #seed = 42)#,
  #extra_time = spring_extra_years)
  
  increase_dat <- bind_cols(x, spring_info) |> 
    rename(EXPCATCHWT = observed) |>
    mutate(SVSPP = 103, 
           rep = i)
  
  spr.increase_sims <- bind_rows(spr.increase_sims, increase_dat)
  
}




# decrease loop 

spr.decrease_sims <- data.frame()

for(i in seq(1:1000)){
  
  x <- sdmTMB_simulate(
    formula = ~1 + poly(AVGDEPTH, 2) + as.factor(AREA),
    data = future_data,
    mesh = spring_mesh,
    family = tweedie(link = "log"),
    time = "EST_YEAR",
    B = c(-0.06, 7.66, -27.7, log(0.052)), # coefficient estimates 
    range = 96.2, 
    #rho = 0.158, # AR1 correlation
    sigma_O = 1.15,
    sigma_E = 0.844,
    phi = 1.83, # dispersion parameter
    tweedie_p = 1.39, # tweedie power parameter 
    fixed_re = fixed_re)#,
  #seed = 42)#,
  #extra_time = fall_extra_years)
  
  decrease_dat <- bind_cols(x, spring_info) |> 
    rename(EXPCATCHWT = observed) |>
    mutate(SVSPP = 103, 
           rep = i)
  
  spr.decrease_sims <- bind_rows(spr.decrease_sims, decrease_dat)
  
}