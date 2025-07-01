library(nationalparkcolors)
library(infer)
library(boot)
library(NatParksPalettes)
#' 
#' Plot distributions
#'
#' @description 
#'
#' @param data:
#' @param diff_col:
#' @param pal: default is 2 colors from park_palette("SmokyMountains") from library(nationalparkcolors)
#' 
#'
#' @returns 
#' 
#'
#' @examples

plot_distribution <- function(dist_dat, ci_dat, scenario = "Status quo survey effort", pal = c("#40663F", "#497381", "#4E5462", "#D58A60"), ...){
  
  plot_dat <- dist_dat |> filter(effort == scenario)
  
  if(scenario == "Status quo survey effort"){
    plot <- ggplot(plot_dat) +
      geom_histogram(aes(x = estimate, fill = pal[1]), color = "white") + 
      scale_fill_manual(values = c(pal[1]), labels = c(scenario), name = NULL) +
      shade_confidence_interval(endpoints = ci_dat, color = pal[2], fill = pal[2], alpha = 0.45) +
      #facet_wrap(~str_to_title(SEASON)) +
      labs(x = "Linear regression slope of abundance index", y = "Number of linear regression slopes") #+
      # theme_bw() +
      # theme(title = element_text(size = 10), legend.position = "bottom", panel.grid = element_blank())
      }
  else {
    plot <- ggplot(plot_dat) +
      geom_histogram(aes(x = estimate, fill = pal[4]), color = "white") + 
      scale_fill_manual(values = c(pal[4]), labels = c(scenario), name = NULL) +
      shade_confidence_interval(endpoints = ci_dat, color = pal[2], fill = pal[2], alpha = 0.45) +
      #facet_wrap(~str_to_title(SEASON)) +
      labs(x = "Linear regression slope of abundance index", y = "Number of linear regression slopes") #+
      # theme_bw() +
      # theme(title = element_text(size = 10), legend.position = "bottom", panel.grid =  element_blank())
  }
  
  return(plot)
}


#' Plot 
#'
#' @description 
#'
#' @param data:
#' @param year_col:
#' @param pal: default is 2 colors from park_palette("SmokyMountains") from library(nationalparkcolors)
#'
#'
#' @returns 
#' 
#'
#' @examples
#'                    
#' 


plot.stratmu <- function(data, year_col, pal = c("#40663F",  "#D58A60"), ...){
  
  data <- data |> 
    mutate(sdlog = sqrt(log(1+(sqrt(stratvar)/stratmu)^2)), #logistic standard deviation
           lower = qlnorm(0.025, log(stratmu), sdlog), # lower quantile of the logistic normal distribution
           upper = qlnorm(0.975, log(stratmu), sdlog)) |> # upper quantile of the logistic normal distribution
    mutate(sdlog = ifelse(is.nan(sdlog), 0, sdlog), # if sdlog is NaN, replace with 0
           lower = ifelse(is.nan(lower), 0, lower), # if the lower quantile is NaN, replace with 0
           upper = ifelse(is.nan(upper), 0, upper)) # if the upper quantile is NaN, replace with 0
  
  
  plot <- ggplot(data) + 
    aes(as.factor({{year_col}}), # x value 
        stratmu, ...) + # y value - stratified mean value
    # color = TYPE, # differentiate color based on whether or not wind tows were included in the calculation
    # shape = TYPE) +  # differentiate shapes based on whether or not wind tows were included in the calculation
    geom_pointrange(aes(ymin=lower, ymax = upper), position = position_dodge2(width=0.4)) + # plot the upper and lower quantiles about the mean 
    #facet_wrap(vars({{facet_by}}), scales = "free_y") + # create sequence of panels based on SEASON variable
    #facet_grid(rows = vars(GEO_AREA), cols = vars(SEASON), scales = "free_y") + # create sequence of panels based on SEASON variable
    labs(x = "Year", y = "Stratified Mean (kg/tow)") + # edit plot labels 
    #ylim(0,NA) #+
    # theme_bw() + # black and white plot theme
    theme(legend.position="bottom", # move legend to the bottom
         legend.title = element_blank()#, # leave legend title blank
          # axis.title = element_text(size = 11), 
          # axis.text = element_text(size = 11), 
          # strip.text = element_text(size = 11), 
          # legend.text = element_text(size = 11)
          ) + 
    scale_color_manual(values = pal)
  
  return(plot)
}

#' Plot diff
#'
#' @description 
#'
#' @param data:
#' @param diff_col:
#' 
#'
#' @returns 
#' 
#'
#' @examples

plot.diff <- function(data, diff_col, ...){
  
  plot <- ggplot(data) + 
    geom_boxplot(aes(x = scenario, y = {{diff_val}})) + 
    theme_bw()
  
  return(plot)
}
