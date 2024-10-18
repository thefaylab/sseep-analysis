#' Create a dataframe of potential survey locations 
#' 
#' Sample locations used in the model fitting process to serve as potential future locations for simulating response values with `sdmTMB_simulate()`
#' 
#' @param year_data integer vector of year values to sample
#' @param nyears number of years to sample
#' @param mod_preds output from `sdmTMB::predict()`; estimated spatial random effects in link space at a given time and location 
#' @param seed Random number seed
#' 
#' @returns 
#' An input dataframe for sdmTMB_simulate() containing X and Y columns in UTM coordinates, the fixed effects components of the fitted linear predictor, and the estimated spatial random effects 


sample_years <- function(nyears, year_data, seed = sample.int(1e6, 1L)){
  # create one data frame of future locations 
  set.seed(seed)
  
  year_samps <- map(nyears, ~sample(unique(year_data), size = 1, replace = TRUE))
  
  return(year_samps)

}



#' Create a dataframe of potential survey locations 
#' 
#' Sample locations used in the model fitting process to serve as potential future locations for simulating response values with `sdmTMB_simulate()`
#' 
#' @param year_samps output list object from `sample_years()`
#' @param mod_preds output from `sdmTMB::predict()`; estimated spatial random effects in link space at a given time and location 
#' @param seed Random number seed
#' 
#' @returns 
#' An input dataframe for sdmTMB_simulate() containing X and Y columns in UTM coordinates, the fixed effects components of the fitted linear predictor, and the estimated spatial random effects 
# 
#' @examples
#' set.seed(1)
#' creates one data frame of 5 years of "new locations" based on historical locations
#' years <- c(2009:2016, 2017:2019, 2021)
#' nsims <- 1:5
#' 
# test_data <- survey_locations(nyears = nsims, year_data = years, mod_preds = preds)
# 
# 
# creates list of 5 iterations of new locations, and a set seed for reproducibility 
# test_data2 <- map2(1:5, seed, ~survey_locations(nyears = nsims, year_data = years, mod_preds = preds, seed = .y))


# sample the model fitted predictions for a number of sampled years to serve as the new locations from historical survey locations. 
survey_locations <- function(#nyears, year_data, 
                             year_samps, mod_preds, seed = sample.int(1e6, 1L)){

set.seed(seed)

#year_samps <- map(nyears, ~sample(unique(year_data), size = 1, replace = TRUE)) #|> list()) # |> set_names(nm=str_c("sim",nsims, sep=""))) |> pivot_longer()) |> rownames_to_column(var = "future_year"))

mod_preds_list <- list(mod_preds)

future_locs <- map2(mod_preds_list, year_samps, ~filter(.x, EST_YEAR %in% .y)) |>
  map2_dfr(seq(year_samps), ~mutate(., future_year = rep(.y, length.out = nrow(.x))))
# future_locs <- map2(data, year_samps, ~filter(.x, EST_YEAR %in% .y)) |> 
#   map2(seq(year_samps), ~mutate(., future_year = rep(.y, length.out = nrow(.x))))

return(future_locs)
}


#' Create a dataframe of potential grid locations 
#' 
#' Sample locations used in the model fitting process to serve as potential future locations for simulating response values with `sdmTMB_simulate()`
#' 
#' @param year_samps output list object from `sample_years()`
#' @param grid_preds output from `sdmTMB::predict()`; estimated spatial random effects in link space at a given time and location  
#' @param seed Random number seed
#' 
#'  @returns 
#' An input dataframe for sdmTMB_simulate() containing X and Y columns in UTM coordinates, the fixed effects components of the fitted linear predictor, and the estimated spatial random effects 
#' 
#' @examples
#' set.seed(1)
#' creates one data frame of 5 years of "new locations" based on historical locations
#' years <- c(2009:2016, 2017:2019, 2021)
#' nsims <- 1:5
#' test_data3 <- map2(1:5, seed, ~sample_grid(nyears = nsims, year_data = years, grid_preds = fall_grid_preds, seed = .y))


grid_locations <- function(#nyears, year_data,
                           year_samps, grid_preds, seed = sample.int(1e6, 1L)){

  set.seed(seed)
  
  # year_samps <- map(nyears, ~sample(unique(year_data), size = 1, replace = TRUE)) #|> list()) # |> set_names(nm=str_c("sim",nsims, sep=""))) |> pivot_longer()) |> rownames_to_column(var = "future_year"))
  
  grid_preds_list <- list(grid_preds)
  
  future_grid <- map2(grid_preds_list, year_samps, ~filter(.x, EST_YEAR %in% .y)) |> 
    map2_dfr(seq(year_samps), ~mutate(., future_year = rep(.y, length.out = nrow(.x)))) 
  
  return(future_grid)
}


#' Extract coefficients from model fit 
#' 
#' 
#' 
#' @param year_samps list of lists object containing sampled years; output list object from `sample_years()`
#' @param mod_coefs dataframe of model coefficient estimates; output from sdmTMB::tidy()
#' @param n_depth_coefs total number of depth coefficients estimated in the original sdmTMB() model fit.
#' @param wind_coef if a wind coefficient was estimated in the original sdmTMB() model fit. Default is TRUE
#' @param treatment if wind_coef = TRUE, an integer vector value to change wind coefficient in log space. if wind_ or serve as a wind estimated coefficient it wind_coef is FALSE
#' @param seed Random number seed
#' 
#' @returns 
#'  
#'  


prepare_coefs <- function(#nyears, 
                          year_data, 
                          year_samps, 
                          mod_coefs, 
                          n_depth_coefs, 
                          wind_coef = TRUE, 
                          spatial_coef = TRUE,
                          treatment = 0)#,
                          #seed = sample.int(1e6, 1L))
                         {
 
  #set.seed(seed)
  
  
   year_samps_flat <- unlist(year_samps, recursive = F) |> unique() |> sort() 
   depth_mod_coefs <- mod_coefs$estimate[seq(n_depth_coefs)]
 
  if(wind_coef == TRUE){
    names <- c(rep("DEPTH_COEF", times = n_depth_coefs), year_data,  "WIND_COEF")
  
    mod_coefs$term <- names
   
    # n_coefs <- length(mod_coefs$estimate)
    # coefs <- mod_coefs$estimate[-n_coefs]
    wind <- tail(mod_coefs$estimate, 1) + treatment
    
    coefs_list <- list(mod_coefs)
  
    year_coefs <- map2_dbl(coefs_list, year_samps_flat, ~filter(.x, term == as.character(.y)) |> pluck("estimate"))
    
    if(spatial_coef == TRUE){
    
      b <- c(depth_mod_coefs, year_coefs, wind, 1)
    
      } else {
    
      b <- c(depth_mod_coefs, year_coefs, wind)
      
      } 
    
    } else {
      names <- c(rep("DEPTH_COEF", times = n_depth_coefs), year_data)
      mod_coefs$term <- names
       
      coefs_list <- list(mod_coefs)
       
      year_coefs <- map2_dbl(coefs_list, year_samps_flat, ~filter(.x, term == as.character(.y)) |> pluck("estimate"))
      
      if(spatial_coef == TRUE){
        
        b <- c(depth_mod_coefs, year_coefs, treatment, 1)
      
        } else {
        
        b <- c(depth_mod_coefs, year_coefs, treatment)
        
        }
  } 
  
  return(b)
  
  # future_grid <- map2(grid_preds_list, year_samps, ~filter(.x, EST_YEAR %in% .y)) |> 
  #   map2_dfr(seq(year_samps), ~mutate(., future_year = rep(.y, length.out = nrow(.x)))) 
  
  
}

                   