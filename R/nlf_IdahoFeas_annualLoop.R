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
#' @param percentilesEV_survival_eggs_tad Columns are the different wetlands, rows 
#' are a vector of length years with percentiles to use for environmental variation (EV) 
#' in each iteration (e.g. output from selectEVPercentilesNormal()).
#' @param percentilesEV_survival_yoy_adult Columns are the different wetlands, rows 
#' are a vector of length years with percentiles to use for environmental variation (EV) 
#' in each iteration (e.g. output from selectEVPercentilesNormal())..
#' @param percentilesEV_reproduction Columns are the different wetlands, rows 
#' are a vector of length years with percentiles to use for environmental variation (EV) 
#' in each iteration (e.g. output from selectEVPercentilesNormal()).
#' 
#' @importFrom dplyr %>%
#'
#' @export
runAnnualLoopNLFIdahoPVA <- function(parameterByIterTracking, yrs, i, q,
                                 # dispersal_edge_list,dispersal_tracking, 
                                 initial_year, wetlands,stage_classes,
                                 percentilesEV_survival_eggs_tad,
                                 percentilesEV_survival_yoy_adult,
                                 percentilesEV_reproduction,
                                 alternative_details) {
  
  # STILL MISSING - DISPERSAL
  
  
  # start.time <- Sys.time()
  # Rprof()    ## Turn on the profiler
  # Collect some info
  n_wetlands <- length(wetlands)

  # Make a data frame to track the results
  resultsTracking_popSize_females <- dapva::makeResultsTracking(i,
    yrs = yrs, initial_year = initial_year,
    pops = wetlands, class_names = stage_classes,
    sex = "female"
  )


  for (j in 1:yrs) {
    print(paste("iteration", i, "run", q, "year", j))

    ######### Apply environmental stochasticity (EV) to select survival and reproductive rates. #########
    
    # Note: for now, the various wetlands are perfectly correlated. Discuss again with Lea and Rebecca.
    # If want to have different EVs for each wetland, see how did it for pdogs with lapply
    
    s_eggs_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_eggs_no_threats")])
    s_eggs_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_eggs_no_threats")])
    s_eggs <- dapva::selectPercentileBetaDistribution(mean = s_eggs_mean, sd = s_eggs_sd, EV_percentile = percentilesEV$eggs[j])
    
    s_eggs <- unlist(lapply(1:n_wetlands, function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = s_eggs_mean,
        sd = s_eggs_sd,
        EV_percentile = percentilesEV_survival_eggs_tad[j, paste(wetlands[x])]
      )
    }))
    
    
    s_tadpoles_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_tadpoles_no_threats")])
    s_tadpoles_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_tadpoles_no_threats")])
    s_tadpoles <- dapva::selectPercentileBetaDistribution(mean = s_tadpoles_mean, sd = s_tadpoles_sd, EV_percentile = percentilesEV$tadpoles[j])
    
    s_yoy_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_yoy_no_threats")])
    s_yoy_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_yoy_no_threats")])
    s_yoy <- dapva::selectPercentileBetaDistribution(mean = s_yoy_mean, sd = s_yoy_sd, EV_percentile = percentilesEV$yoy[j])
    
    s_juv_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_juv_no_threats")])
    s_juv_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_juv_no_threats")])
    s_juv <- dapva::selectPercentileBetaDistribution(mean = s_juv_mean, sd = s_juv_sd, EV_percentile = percentilesEV$adult[j])
    
    s_adult_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_adult_no_threats")])
    s_adult_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_adult_no_threats")])
    s_adult <- dapva::selectPercentileBetaDistribution(mean = s_adult_mean, sd = s_adult_sd, EV_percentile = percentilesEV$adult[j])
    
    
    # NEXT TO DO - do this for reproduction parameters
    
    
    ######### In the first year, add any translocated tadpoles #########

    # No NLF there at the moment so no other initial condition
    
    if (j == 1) {

      # populate with initial conditions
      # Tadpoles get translocated in!
      
      if(alternative_details$release_location == "cell7"){
        rows <- which(resultsTracking_popSize_females$class == "tadpoles") &&
          which(resultsTracking_popSize_females$pop == "cell7")
      }
      if(alternative_details$release_location == "all_three"){
        rows <- intersect(which(resultsTracking_popSize_females$class == "tadpoles"),
          (which(resultsTracking_popSize_females$pop == "cell7" | 
                   resultsTracking_popSize_females$pop == "cell3" |
                           resultsTracking_popSize_females$pop == "cell4")
                   )
        )
      }

      resultsTracking_popSize_females[rows, "1"] <- as.numeric(alternative_details$n_tadpoles_per_year)/length(rows)

    }
    
    ######### Then move through the life cycle for this year #########
    
    # tadpoles survive (or not) to become yoy
    resultsTracking_popSize_females[paste(j)]
    
    
    
    ### OLD - from pdog model, left her for now for reference
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
