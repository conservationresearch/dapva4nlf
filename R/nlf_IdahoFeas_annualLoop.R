# This file contains the nlf Idaho feasability annual loop.

# Laura Keating
# April 2021

# Write the annual loop of the PVA as a function.
# This way we can call it many times with the same set of parameters to calculate 
# probability of persistence, etc.

#' This functions contains the annual loop of the Idaho Feasability 
#' Northern Leopard Frog population model.

#' The inputs to this function represent one set of parameters. This function
#' can be called inside an iteration loop and a run loop. For each iteration, a
#' different set of parameters should be chosen to simulate parametric
#' uncertainty. For each run, the same parameters are used and differences in
#' the output are due to environmental and demographic stochasticity.

#' @param parameterByIterTracking Parameters to use in each iteration in the
#' format of selectBTPDparameterByIterTracking().
#' @param yrs The number of years to run the model for (e.g. 50).
#' @param i The iteration number. For results tracking purposes.
#' @param q The run number. For results tracking purposes.
#' @param initial_year First year of the model.
#' @param wetlands Wetland cell names/abbreviations.
#' @param K Carrying capacity .
#' @param percentilesEV A vector of lenth years with percentiles to use for
#' environmental variation (EV) in each iteration (e.g. output from selectEVPercentilesNormal()).
#' @importFrom dplyr %>%
#'
#' @export
runAnnualLoopNLFIdahoPVA <- function(parameterByIterTracking, yrs, i, q,
                                 # dispersal_edge_list,dispersal_tracking, 
                                 initial_year, wetlands,
                                 K, percentilesEV) {
  # start.time <- Sys.time()
  # Rprof()    ## Turn on the profiler
  # Collect some info
  n_wetlands <- length(wetlands)

  # Make a data frame to track the results
  resultsTracking_popSize_females <- dapva::makeResultsTracking(i,
    yrs = yrs, initial_year = initial_year,
    pops = colonies, class_names = unique(initialPopSizes_female$stage),
    sex = "female"
  )


  for (j in 1:yrs) {
    print(paste("iteration", i, "run", q, "year", j))

    ######### In the first year, populate with initial conditions. #########
    # Starting Population Size and Sex & Stage Distribution

    if (j == 1) {

      # populate with initial conditions

      # Record the results - Females

    }
    ######### Second year and beyond. #########
    if (j > 1) { #  If in the second year and beyond, apply survival and reproduction logic.


      ######### Apply environmental stochasticity (EV) to select survival and reproductive rates (without supplemental feeding). #########


      ######### Summarize the survival rates for each colony into a vector. #########
    
      #### Survive from Previous Year and Age (Spring - CHEFL NLF LIFE CYCLE) #########

    
      ######### Apply Carrying Capacity  #########


      ######### Combine reproduction values into a large vector for all colonies #########
      # We will need this format to apply the reproduction values.

   

      ######### Females Reproduce #########
     

      ######### Dispersal #########
    

      ######### Implement quasi-extinction threshold rules #########

      ######### Record results and prep for next round #########

      # Record the results - Females
      rows_this_iteration <- which(resultsTracking_popSize_females$iteration == i)
      resultsTracking_popSize_females[rows_this_iteration, paste((initial_year + j - 1))] <- popSizeVectorFemales_newAfterQuasiExtinct
      resultsTracking_popSize_females[rows_this_iteration, "run"] <- q
      resultsTracking_popSize_females[rows_this_iteration, "pop"] <- sapply(strsplit(rownames(popSizeVectorFemales), " "), getElement, 1)
      resultsTracking_popSize_females[rows_this_iteration, "class"] <- sapply(strsplit(rownames(popSizeVectorFemales), " "), getElement, 2)

      # Reset the starting values for the next year

    } # end if j>1

  
  } # end annual loop

  # Rprof(NULL)    ## Turn off the profiler
  # summaryRprof()
  # end.time <- Sys.time()
  # end.time - start.time

  # Return the results
  return(resultsTracking_popSize_females)
} # end runAnnualLoop function
