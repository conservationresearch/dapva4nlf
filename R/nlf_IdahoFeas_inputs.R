# This file contains functions related to BTPD PVA inputs.

# Laura Keating
# January 2021

##### getNLFIdahoFeasinputs  #####

#' Get inputs for the NLF Idaho Feasability assessment.
#'
#' This is an all-encompassing function that gets the inputs needed
#' to run the NLF Idaho Feasability Assessment.
#'
#' @return Returns a list of all the inputs needed. See example.
#'
#' @examples
#' test <- getNLFIdahoFeasinputs()
#' # no unit test since very specific; appears to be working correctly
#' @export
getNLFIdahoFeasinputs <- function() {


  #---- Years to run the model and initial year.  -------------

  # need it here stand alone in addition to including as an input here
  # as used elsewhere also in this function
  yrs <- 50
  initial_year <- 1 #arbitrary, nothing is year specific really

  # Include it in the inputs list for tracking, clarity, and reference
  year_inputs <- list(
    c(input = "initial_year", type = "first year of the model", best_guess = initial_year,
      best_guess_type = "no uncertainty", source = "NA",
      comments = "None of the data relies on the year so  just call it year 1"
    ),
    c(input = "yrs", type = "years to run the model", best_guess = yrs,
      best_guess_type = "no uncertainty", source = "Team decision",
      comments = "As discussed with the larger group from Jan-March 2021"
    )
  )

  #---- Reproduction: proportion of females reproductively active for each stage.  -------------
  
  # LK PLACEHOLDERS FOR TESTING
  
  repro_prop_active <- list(
    c(input = "p_females_lay_eggs_mean_A2", type = "reproduction", 
      lcl = 0.25, best_guess = 0.4, ucl = 0.6, confidence = 0.95,
      lower_bound = 0, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = ""),
    c(input = "p_females_lay_eggs_mean_A3", type = "reproduction", 
      lcl = 0.5, best_guess = 0.6, ucl = 0.7, confidence = 0.95,
      lower_bound = 0, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = "" ),
    c(input = "p_females_lay_eggs_mean_A4plus", type = "reproduction", 
      lcl = 0.5, best_guess = 0.6, ucl = 0.7, confidence = 0.95,
      lower_bound = 0, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = "")
  )
  
  #---- Reproduction: number of eggs per female.  -------------
  
  repro_clutch_size <- list(
    c(input = "num_eggs_per_active_female_mean_A2", type = "reproduction", 
      lcl = 2000, best_guess = 3000, ucl = 4000, confidence = 0.95,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = ""),
    c(input = "num_eggs_per_active_female_mean_A3", type = "reproduction", 
      lcl = 3000, best_guess = 4000, ucl = 5000, confidence = 0.95,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = ""),
    c(input = "num_eggs_per_active_female_mean_A4plus", type = "reproduction", 
      lcl = 6000, best_guess = 7000, ucl = 8000, confidence = 0.95,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = "")
  )

  #---- Compile the inputs and set up tracking objects.  -------------
  inputs <- dapva::makeInputsDF(parameters = c(
    year_inputs,
    repro_prop_active,
    repro_clutch_size
  ))


  #---- Make the dispersal edge list and tracking object.  -------------
  # dispersal_edge_list <- createDispersalEdgeList(
  #   distance_btwn_pops = distance_btwn_colonies_m,
  #   max_dispersal_distance = as.numeric(inputs$best_guess[which(inputs$input == "max_dispersal_distance_m")])
  # ) # only needs to be made once; distances in m
  # dispersal_tracking <- createDispersalTracking(dispersal_edge_list, class_names = unique(initialPopSizes_female$stage))

  #---- Return the results.  -------------

  # Address warning in devtools::check() (i.e. RMD check) re 'no visible binding for global variable'.
  # dat_distance_btwn_colonies_m <- NULL




  # return(list(inputs, dispersal_edge_list, dispersal_tracking))
  return(list(inputs))
  
}


##### selectNLFIdahoFeasparameterByIterTracking  #####

# no unit test since very specific; appears to be working correctly

#' Select NLF Idaho Feasability parameters for each iteration.
#'
#' Select parameters for each iteration of the NLF IDaho Feasability PVA
#' and organize them in a format for easy tracking and future use.
#'
#' @param inputs model inputs formatted using getNLFIdahoFeasinputs()
#' and choose the first output in the list (i.e. singular number outputs as this
#'  is where parametric uncertainty is specified).

#' @return Returns one value for each parameter, representing the inputs for one
#'  iteration. Put in a foreach loop and rbind to get it for multiple iterations.
#'
#' @examples
#'
#' inputs_all <- getNLFIdahoFeasinputs()
#'
#' # Singular number inputs
#' inputs <- inputs_all[[1]]
#'
#' selectNLFIdahoParameterByIterTracking(inputs)
#' @export
selectNLFIdahoParameterByIterTracking <- function(inputs) {

  # Initialize an empty tracking sheet
  parameterByIterTracking_empty <- dapva::makeParameterByIterTracking(inputsDF = inputs, n_iter = 1) # just one at a time now that it is inside a foreach loop

  i <- 1 # now that doing it in a foreach loop, each time just want it to fill in the one col and then bind them all together at the end
  parameterByIterTracking <- parameterByIterTracking_empty # initialize with the empty one each time for clarity (perhaps not necessary)
  # rownames(parameterByIterTracking) <- m # record which iteration it is with the col number

  ######### Select the survival parameters for this iteration. #########

  # Choose and record the survival parameters for this iteration - mean

  ######### Select the reproduction parameters for this iteration - proportion of repro active females. #########

  # Choose and record the reproduction parameters for this iteration - mean
  parameterByIterTracking[i, "p_females_lay_eggs_mean_A2"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A2", inputsDF = inputs, term = 3)
  parameterByIterTracking[i, "p_females_lay_eggs_mean_A3"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A2", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_lay_eggs_mean_A4plus"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A2", inputsDF = inputs)
  
  ######### Select the reproduction parameters for this iteration - number of offspring per female. #########
  parameterByIterTracking[i, "num_eggs_per_active_female_mean_A2"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A2", inputsDF = inputs)
  parameterByIterTracking[i, "num_eggs_per_active_female_mean_A3"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A2", inputsDF = inputs)
  parameterByIterTracking[i, "num_eggs_per_active_female_mean_A4plus"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A2", inputsDF = inputs)
  
  
  
  ######### Select the carrying capacity parameters for this iteration. #########
  # parameterByIterTracking[i, "colony_extent_K_scenario"] <- selectBTPDColonyExtentScenario(inputs)
  # parameterByIterTracking[i, "density_K_med_high_env_stress"] <- max(3.81, dapva::selectParamNormalDistribution(input_name = "density_K_med_high_env_stress", inputsDF = inputs)) # 3.81 is a placeholder for a lower bound, from Sian 2013 visitual counts, may want to revisit shape of this distribution
  # parameterByIterTracking[i, "density_K_low_env_stress"] <- max(3.81, dapva::selectParamNormalDistribution(input_name = "density_K_low_env_stress", inputsDF = inputs)) # 3.81 is a placeholder for a lower bound, from Sian 2013 visitual counts, may want to revisit shape of this distribution

  ######### Fill in some inputs that will stay the same across iterations. Important to show for clarity and for use in the tornados (no uncertainty in these but if we put multiple runs with different inputs together it may be useful to have them in here) #########

  # parameterByIterTracking[i, "max_dispersal_distance_m"] <- as.numeric(inputs$best_guess[which(inputs$input == "max_dispersal_distance_m")])
  # parameterByIterTracking[i, "dispersal_threshold_prop_of_carrying_capacity"] <- as.numeric(inputs$best_guess[which(inputs$input == "dispersal_threshold_prop_of_carrying_capacity")])
  # parameterByIterTracking[i, "climate_high_carbon_scenario_prob"] <- as.numeric(inputs$best_guess[which(inputs$input == "climate_high_carbon_scenario_prob")])
  # parameterByIterTracking[i, "scenario_climate"] <- inputs$best_guess[which(inputs$input == "scenario_climate")]
  # parameterByIterTracking[i, "scenario_plague"] <- inputs$best_guess[which(inputs$input == "scenario_plague")]
  # parameterByIterTracking[i, "p_colony_extent_natural_exp_K"] <- as.numeric(inputs$best_guess[which(inputs$input == "p_colony_extent_natural_exp_K")])

  parameterByIterTracking[i, "yrs"] <- as.numeric(inputs$best_guess[which(inputs$input == "yrs")])
  parameterByIterTracking[i, "initial_year"] <- as.numeric(inputs$best_guess[which(inputs$input == "initial_year")])

  ######### Return the results #########
  return(parameterByIterTracking)

  # for package checking; binding the variable locally to the function (see https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/)
  # do at end since these are dat files used in this function so can't have a the beginningx
  # dat_N_initial_by_colony <- dat_age_structure_initial <- dat_K_colony_extent_by_colony <- dat_distance_btwn_colonies_m <- NULL
}

