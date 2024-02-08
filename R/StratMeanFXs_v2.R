#' Calculate estimates of stratified mean abundance 
#'
#' Calculate the random stratified mean catch rate to serve as an index of relative abundance according to a random stratified survey design (Krebs, Ch.8, Eq. 8.17)
#'
#' @param dat: survey catch rate data
#' @param strata: dataframe of survey strata footprint containing per strata area information
#'
#' @returns
#' A dataframe containing annual estimates of abundance and survey coefficients of variation for a given species similar to those used as inputs for species stock assessments.
#'


stratified.mean <- function(dat, strata){
  
  #figure out the strata to use for this calculation, will be the strata from the survey data frame
  strat_use <- unique(dat$STRATUM)
  
  #adjust the weights for the strata used in the calculation
  strata_wts_use <- strata_wts |>
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

#' Calculate mean relative percent difference  
#'
#' Calculate the mean relative percent difference between two estimates of random stratified mean abundance indices under survey effort treatments 
#'
#' @param dat: dataframe containing differing values of random stratified abundance indices output from `stratified.mean()`
#'
#' @returns
#' A dataframe containing the mean relative percent difference between survey effort stratified abundance indices.
#'

mean.diff <- function(dat){

mudiff <- dat |> 
  group_by(EST_YEAR) |> 
  summarise(diff_mu = diff(stratmu), .groups = "drop") |>
  mutate(exp_mu = (exp(diff_mu))-1,
         sq_diff = exp_mu^2) |>
  filter(EST_YEAR %in% tail(EST_YEAR, 5)) |> 
  summarize(mudiff = mean(sq_diff), .groups = "drop") |> # calculate the average; drop the grouping factor 
  mutate(mudiff = sqrt(mudiff)*100) 

return(mudiff)
  
}
