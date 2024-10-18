#' Draw MCMC samples over iterations
#' 
#' Draw MCMC samples over a number of iterations to create a distribution of samples
#' 
#' @param reps number of samples for iteration
#' @param mod 
#' @param mcmc_warmup 
#' @param mcmc_iter 
#' @param seed Random number seed
#' 
#' @returns 
#' A list item with length(reps); each sublist contains a sample draw for each data point used in model fitting through `sdmTMB()` 
rep_predict_mcmc <- function(mod, mcmc_warmup = 400, 
                             mcmc_iter = 800, seed){
  set.seed(seed) # new random sample for each replicate
  
  samps <- sdmTMBextra::predict_mle_mcmc(mod, mcmc_iter = mcmc_iter, mcmc_warmup = mcmc_warmup)
  
  mcres <- #map(samps, ~
    residuals(mod, type = "mle-mcmc", mcmc_samples = samps)#)
  
  return(mcres)
  } 
 
 

#' Generate residuals 
#' 
#' 
#' Generate residuals based on MCMC samples drawn with `rep_predict_mcmc()`
#' 
#' @param mod An `sdmTMB()` model
#' @param reps number of replicates to perform
#' @param type default is mle-mcmc. Fixed effects are held at their MLEs and random effects are taken from a single posterior sample obtained with MCMC. [See sdmTMB documentation](https://pbs-assess.github.io/sdmTMB/reference/residuals.sdmTMB.html). 
#' @param seed Random number seed; should be the same seed provided to `rep_predict_mcmc()`
#' 
#' @returns 
#' A list item with length(reps); each sublist contains a residual for each data point used in model fitting through `sdmTMB()`
#' 

rep_residuals <- function(mod, type = "mle-mcmc", mcmc_samples, seed){
  # set.seed(seed)
  
  mcres <- map(mcmc_samples, ~residuals(object = mod, 
                                  type = type,
                                  mcmc_samples = .))
  }


