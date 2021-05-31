# Making functions for my DAPVA (Decision Analysis Population Viability
# Analysis) package. Following Chapter 6 in
# https://r-pkgs.org/package-within.html

# This file contains functions related to NLF reproduction.

# Laura Keating
# May 2021

##### survivalEggs  #####

#' Calculate egg survival.
#'
#' Applies the survival matrix to eggs after masking the other life stages 
#' in the population vector and survival matrix.
#' 
#' @param popSizeVector Female population sizes formatted in the style
#' of makePopSizeVector() for the relevant year.
#' @param survivalMatrix Survival matrix formatted in INSERT.
#' @param demographic_stochasticity TRUE or FALSE, defaults to TRUE
#'
#' @examples
#' library(dapva)
#' 
#'   popSizeVector_test <- dapva::makePopSizeVector(
#'   pop_class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
#'   num_indiv_per_pop_class = c(1000, 100, 20), sex = "female"
#'   )
#'   survivalMatrix_test <- dapva::makeSurvivalMatrix(
#'   class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
#'   survival_rates = c(0.5, 0.4, 0.9)
#'   )
#'   egg_survival_test <- dapva4nlf::survivalEggs(popSizeVector_test, 
#'   survivalMatrix_test,
#'   demographic_stochasticity = FALSE)
#' 
#' @export
# unit test in place
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

#' Calculate tadpole survival.
#'
#' Applies the survival matrix to tadpoles after masking the other life stages 
#' in the population vector and survival matrix.
#' 
#' @param popSizeVector Female population sizes formatted in the style
#' of makePopSizeVector() for the relevant year.
#' @param survivalMatrix Survival matrix formatted as per dapva::makeSurvivalMatrix().
#' @param demographic_stochasticity TRUE or FALSE, defaults to TRUE
#'
#' @examples
#' library(dapva)
#'   popSizeVector_test <- dapva::makePopSizeVector(
#'   pop_class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
#'   num_indiv_per_pop_class = c(1000, 100, 20), sex = "female"
#'   )
#'   survivalMatrix_test <- dapva::makeSurvivalMatrix(
#'   class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
#'   survival_rates = c(0.5, 0.4, 0.9)
#'   )
#'   tadpole_survival_test <- dapva4nlf::survivalTadpoles(popSizeVector_test, 
#'                                                        survivalMatrix_test,
#'                                                        demographic_stochasticity = FALSE)
#' @export
# unit test in place
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

#' Calculate overwinter survival of terrestrial (i.e. the ones that overwinter) life stages.
#'
#' Applies the survival matrix to the fall population size vector after masking eggs 
#' and tadpoles (since those are intermediate stages that are grown into young-of-year by the fall)
#' to calculate how many of each terrestrial age class survives over winter to the spring.
#'
#' @param popSizeVector_fallLastYear Female population sizes formatted in the style
#' of dapva::makePopSizeVector() for the previous year.
#' @param survivalMatrix Survival matrix formatted as per dapva::makeSurvivalMatrix().
#' @param demographic_stochasticity TRUE or FALSE, defaults to TRUE
#'
#' @examples
#' library(dapva)
#' popSizeVector_test <- dapva::makePopSizeVector(
#' pop_class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
#' num_indiv_per_pop_class = c(1000, 100, 20), sex = "female"
#' )
#' survivalMatrix_test <- dapva::makeSurvivalMatrix(
#'  class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
#'  survival_rates = c(0.5, 0.4, 0.9)
#')
#' survived_the_winter_test <- dapva4nlf::survivalOverwinter(popSizeVector_test, 
#'                                                          survivalMatrix_test,
#'                                                          demographic_stochasticity = FALSE)
#' 
#' @export
# unit test in place
survivalOverwinter <- function(popSizeVector_fallLastYear, 
                             survivalMatrix,
                             demographic_stochasticity = TRUE) {
  
  # for package checking; binding the variable locally to the function (see https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/)
  
  # Mask the matrix to only be for the life stages that overwinter (yoy, juvenile, and adults). 
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


