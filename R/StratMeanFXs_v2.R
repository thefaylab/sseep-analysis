#' Calculate estimates of stratified mean abundance 
#'
#' @description Calculate the random stratified mean catch rate to serve as an index of relative abundance according to a random stratified survey design (Krebs, Ch.8, Eq. 8.17)
#'
#' @param dat: survey catch rate data
#' @param strata: dataframe of survey strata footprint containing per strata area information
#'
#' @returns
#' A dataframe containing annual estimates of abundance and survey coefficients of variation for a given species similar to those used as inputs for species stock assessments.
#'
#'
#'@examples
#'
#' strata <- readRDS(here("data","rds", "active_strata_wts.rds"))
#' dat <- data.frame(STRATUM = c(1650, 1650, 1650, 1650, 3230, 3230, 3230, 1010, 1010, 1010, 1010, 3050, 3050, 3050), 
#'                   STATION = c(56, 18, 45, 225, 4, 5, 50, 65, 180, 303, 314, 27, 8, 178), 
#'                   EXPCATCHWT = c(10, 5, 10, 5, 1,1,1, 10, 5, 10, 5, 1, 1, 1))
#'stratified.mean(dat, strata) 
#'
#'
#'


stratified.mean <- function(dat, strata){
  
  #figure out the strata to use for this calculation, will be the strata from the survey data frame
  strat_use <- unique(dat$STRATUM)
  
  #adjust the weights for the strata used in the calculation
  strata_wts_use <- strata |>
    filter(STRATUM %in% strat_use) |>
    mutate(RelWt = Area_SqNm / sum(Area_SqNm))
  
  #total survey area for this calculation
  survey_area <- sum(strata_wts_use$Area_SqNm)
  
  
  individual <- dat |> 
    group_by(STRATUM) |>
    summarise(towct = length(unique(STATION)), # calculate unique tows
              mu = sum(EXPCATCHWT)/towct, # find the average biomass based on unique tows rather than observations to avoid potential duplication 
              var = ifelse(towct == 1, 0, # if the tow count equals 1, then variance about the mean should be 0
                           sum((EXPCATCHWT - mu)^2)/(towct - 1))) |> # if tow count does not equal 1, then find the variance of biomass
    left_join(strata_wts_use, by = "STRATUM") |> # add each stratum area and relative weight to the dataset based on STRATUM number
    mutate(wt_mu = Area_SqNm * mu, # part one of the stratified mean formula
           wt_var = ((((RelWt)^2) * var) / towct) * (1 - (towct / Area_SqNm))) # part one of the stratified variance formula
    
  # BTSArea <- as.integer(sum(strata_wts$Area_SqNm))
  
  stratified <- individual |> 
    # group_by(EST_YEAR) |> 
    summarise(stratmu = (sum(wt_mu)) / survey_area, # part two of the stratified mean formula
              stratvar = sum(wt_var), 
              cv = sqrt(stratvar)/stratmu) |>  # part two of the stratified variance formula
    mutate(cv = ifelse(is.na(cv), 0, cv))
  
  return(stratified)
  
  #return(individual)
}  

#' Calculate error
#'
#' @description calculates bias, relative bias, and absolute relative bias
#'
#' @param dat: dataframe containing differing values of random stratified abundance indices output from `stratified.mean()`
#' @param observed: column name containing observed values 
#' @param expected: column name containing expected values 
#' 
#'
#' @returns A dataframe where:
#' * `error` represents the bias or error 
#' * `rel.err` represents the relative bias 
#' * `abs.err` represents the absolute relative error 
#'
#' @examples
#' dat <- data.frame(EST_YEAR = c(2009, 2010, 2015), 
#'                   stratmu_x = c(5, 10, 2), 
#'                   stratmu_y = c(2.5, 5, 1))
#' calc.errors(dat, stratmu_x, stratmu_y)
#' 
#' 

calc.errors <- function(dat, observed, expected){

error.dat <- dat |> 
  mutate(error = {{observed}} - {{expected}},
         rel.err = error / {{observed}}, 
         abs.err = abs(rel.err)) |> 
  mutate(error = ifelse(is.nan(error), 0, error), 
         rel.err = ifelse(is.nan(rel.err), 0, rel.err), 
         abs.err =  ifelse(is.nan(abs.err), 0, abs.err))
  
return(error.dat)
  
}

#' Calculate mean percent differences 
#'
#' @description Calculate the mean percent difference 
#'
#' @param errors: dataframe containing differing values of random stratified abundance indices output from `calc.errors()`
#' 
#'
#' @returns A dataframe where:
#' * `ME` represents the mean error
#' * `MRE_perc` represents the mean percent relative difference
#' * `MARE_perc` represents the mean percent absolute relative difference 
#'
#' @examples
#' dat <- data.frame(EST_YEAR = c(2009, 2010, 2015), 
#'                   stratmu_x = c(5, 10, 2), 
#'                   stratmu_y = c(2.5, 5, 1))
#' errors <- calc.errors(dat, stratmu_x, stratmu_y)
#' mean.diff(errors)
#'                   
#' 

mean.diff <- function(error.dat){
  
  mudiff <- error.dat |> 
    summarise(ME = mean(error), 
              MRE_perc = mean(rel.err)*100, 
              MARE_perc = mean(abs.err)*100)
  
  return(mudiff)
  
}

                  
