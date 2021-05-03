# Making functions for my DAPVA (Decision Analysis Population Viability
# Analysis) package. Following Chapter 6 in
# https://r-pkgs.org/package-within.html

# This file contains functions related to NLF reproduction.

# Laura Keating
# May 2021

# Example still needed

##### survivalEggs  #####

#' INSERT 
#'
#' INSERT
#' 
#' @param popSizeVector Female population sizes formatted in the style
#' of makePopSizeVector() for the relevant year.
#' @param survivalMatrix Survival matrix formatted in INSERT.
#' @param demographic_stochasticity TRUE or FALSE, defaults to TRUE
#'
#' @examples
#' # Still to do
#' 
#' @export
# unit test STILL TO DO
survivalEggs <- function(popSizeVector, 
                             survivalMatrix,
                             demographic_stochasticity = TRUE) {
  
  # for package checking; binding the variable locally to the function (see https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/)
  
  # Mask the matrix to only be for eggs.
  popSizeVector_tadpoles <- popSizeVector # initalize
  popSizeVector_tadpoles[!grepl("eggs" , rownames(popSizeVector)),] <- 0
  
  popSizeVectorFemales_new_tadpoles <- dapva::applySurvivalRates(
    survivalMatrix = survivalMatrix,
    popSizeVector = popSizeVector_tadpoles,
    demographic_stochasticity = demographic_stochasticity
  )
  
  return(popSizeVectorFemales_new_tadpoles)
}


##### survivalTadpoles  #####

#' INSERT 
#'
#' INSERT
#' 
#' @param popSizeVector Female population sizes formatted in the style
#' of makePopSizeVector() for the relevant year.
#' @param survivalMatrix Survival matrix formatted in INSERT.
#' @param demographic_stochasticity TRUE or FALSE, defaults to TRUE
#'
#' @examples
#' # Still to do
#' 
#' @export
# unit test STILL TO DO
survivalTadpoles <- function(popSizeVector, 
                             survivalMatrix,
                             demographic_stochasticity = TRUE) {
  
  # for package checking; binding the variable locally to the function (see https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/)
  
  # Mask the matrix to only be for tadpoles. 
  popSizeVector_tadpoles <- popSizeVector # initalize
  popSizeVector_tadpoles[!grepl("tadpoles" , rownames(popSizeVector)),] <- 0
  
  popSizeVectorFemales_new_yoy <- dapva::applySurvivalRates(
    survivalMatrix = survivalMatrix,
    popSizeVector = popSizeVector_tadpoles,
    demographic_stochasticity = demographic_stochasticity
  )

  return(popSizeVectorFemales_new_yoy)
}

##### survivalOverwinter  #####

#' INSERT 
#'
#' INSERT
#' 
#' @param popSizeVector Female population sizes formatted in the style
#' of makePopSizeVector() for the relevant year.
#' @param survivalMatrix Survival matrix formatted in INSERT.
#' @param demographic_stochasticity TRUE or FALSE, defaults to TRUE
#'
#' @examples
#' # Still to do
#' 
#' @export
# unit test STILL TO DO
survivalOverwinter <- function(popSizeVector_fallLastYear, 
                             survivalMatrix,
                             demographic_stochasticity = TRUE) {
  
  # for package checking; binding the variable locally to the function (see https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/)
  
  # Mask the matrix to only be for the lifestages that overwinter (yoy, juvenile, and adults). 
  popSizeVector_overwinter <- popSizeVector_fallLastYear # initalize
  popSizeVector_overwinter[grepl("eggs" , rownames(popSizeVector_overwinter)),] <- 0
  popSizeVector_overwinter[grepl("tadpoles" , rownames(popSizeVector_overwinter)),] <- 0
  
  popSizeVectorFemales_overwinter <- dapva::applySurvivalRates(
    survivalMatrix = survivalMatrix,
    popSizeVector = popSizeVector_overwinter,
    demographic_stochasticity = demographic_stochasticity
  )
  
  return(popSizeVectorFemales_overwinter)
}


