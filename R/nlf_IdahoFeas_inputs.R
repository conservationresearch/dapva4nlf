# This file contains functions related to BTPD PVA inputs.

# Laura Keating
# January 2021

##### getBTPDinputs_statusQuo  #####

#' Get inputs for the NLF Idaho Feasability assessment.
#'
#' This is an all-encompassing function that gets the inputs needed
#' to run the NLF Idaho Feasability Assessment.
#'
#' @param initial_year First year of the model
#' @param yrs Number of years to run the model
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


##### selectBTPDparameterByIterTracking  #####

# no unit test since very specific; appears to be working correctly

#' Select BTPD PVA parameters for each iteration.
#'
#' Select parameters for each iteration of the BTPD PVA and organize them in a
#' format for easy tracking and future use.
#'
#' @param inputs BTPD PVA model inputs formatted using getBTPDinputs_statusQuo()
#' and choose the first output in the list (i.e. singular number outputs as this
#'  is where parametric uncertainty is specified).
#' @param dat_climate_future_lowCarbon_climate_scenarios List element 9 from
#'  getBTPDinputs_statusQuo()
#' @param dat_climate_future_highCarbon_climate_scenarios List element 10 from
#'  getBTPDinputs_statusQuo()
#'
#' @return Returns one value for each parameter, representing the inputs for one
#'  iteration. Put in a foreach loop and rbind to get it for multiple iterations.
#'
#' @examples
#'
#' inputs_all <- getBTPDinputs_statusQuo(
#'   scenario_climate = "Best Guess",
#'   scenario_plague = "Best Guess",
#'   initial_year = 2017, yrs = 50
#' )
#'
#' # Singular number inputs
#' inputs <- inputs_all[[1]]
#'
#' # Climate inputs
#' dat_climate_future_lowCarbon_climate_scenarios <- inputs_all[[9]]
#' dat_climate_future_highCarbon_climate_scenarios <- inputs_all[[10]]
#'
#' selectBTPDparameterByIterTracking(
#'   inputs,
#'   dat_climate_future_lowCarbon_climate_scenarios,
#'   dat_climate_future_highCarbon_climate_scenarios
#' )
#' @export
selectBTPDparameterByIterTracking <- function(inputs,
                                              dat_climate_future_lowCarbon_climate_scenarios,
                                              dat_climate_future_highCarbon_climate_scenarios) {

  # Initialize an empty tracking sheet
  parameterByIterTracking_empty <- dapva::makeParameterByIterTracking(inputsDF = inputs, n_iter = 1) # just one at a time now that it is inside a foreach loop

  i <- 1 # now that doing it in a foreach loop, each time just want it to fill in the one col and then bind them all together at the end
  parameterByIterTracking <- parameterByIterTracking_empty # initialize with the empty one each time for clarity (perhaps not necessary)
  # rownames(parameterByIterTracking) <- m # record which iteration it is with the col number

  ######### Select the survival parameters for this iteration. #########

  # Choose and record the survival parameters for this iteration - mean
  # Low environmental stress
  parameterByIterTracking[i, "p_females_survival_juv_lowEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_juv_lowEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_juv_lowEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_juv_lowEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_survival_adult_lowEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_adult_lowEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_adult_lowEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_adult_lowEnvStress_mean", inputsDF = inputs)

  # Medium environmental stress
  parameterByIterTracking[i, "p_females_survival_juv_medEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_juv_medEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_juv_medEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_juv_medEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_survival_adult_medEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_adult_medEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_adult_medEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_adult_medEnvStress_mean", inputsDF = inputs)

  # High environmental stress
  parameterByIterTracking[i, "p_females_survival_juv_highEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_juv_highEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_juv_highEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_juv_highEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_survival_adult_highEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_adult_highEnvStress_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_adult_highEnvStress_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_adult_highEnvStress_mean", inputsDF = inputs)

  # Choose and record the survival parameters for this iteration - var
  # Low environmental stress
  parameterByIterTracking[i, "p_females_survival_juv_lowEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_juv_lowEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_juv_lowEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_juv_lowEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_survival_adult_lowEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_adult_lowEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_adult_lowEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_adult_lowEnvStress_var", inputsDF = inputs)

  # Medium environmental stress
  parameterByIterTracking[i, "p_females_survival_juv_medEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_juv_medEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_juv_medEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_juv_medEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_survival_adult_medEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_adult_medEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_adult_medEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_adult_medEnvStress_var", inputsDF = inputs)

  # High environmental stress
  parameterByIterTracking[i, "p_females_survival_juv_highEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_juv_highEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_juv_highEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_juv_highEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_survival_adult_highEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_survival_adult_highEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_males_survival_adult_highEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_males_survival_adult_highEnvStress_var", inputsDF = inputs)

  ######### Select the reproduction parameters for this iteration - proportion of repro active females. #########

  # Choose and record the reproduction parameters for this iteration - mean
  # Low environmental stress
  parameterByIterTracking[i, "p_females_repro_yearling_lowEnvStress_high_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_lowEnvStress_high_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_yearling_lowEnvStress_low_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_lowEnvStress_low_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_lowEnvStress_high_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_lowEnvStress_high_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_lowEnvStress_low_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_lowEnvStress_low_mean", inputsDF = inputs)

  # Medium environmental stress
  parameterByIterTracking[i, "p_females_repro_yearling_medEnvStress_high_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_medEnvStress_high_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_yearling_medEnvStress_low_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_medEnvStress_low_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_medEnvStress_high_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_medEnvStress_high_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_medEnvStress_low_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_medEnvStress_low_mean", inputsDF = inputs)

  # High environmental stress
  parameterByIterTracking[i, "p_females_repro_yearling_highEnvStress_high_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_highEnvStress_high_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_yearling_highEnvStress_low_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_highEnvStress_low_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_highEnvStress_high_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_highEnvStress_high_mean", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_highEnvStress_low_mean"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_highEnvStress_low_mean", inputsDF = inputs)

  # Choose and record the reproduction parameters for this iteration - var
  # Low environmental stress
  parameterByIterTracking[i, "p_females_repro_yearling_lowEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_lowEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_lowEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_lowEnvStress_var", inputsDF = inputs)

  # Medium environmental stress
  parameterByIterTracking[i, "p_females_repro_yearling_medEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_medEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_medEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_medEnvStress_var", inputsDF = inputs)

  # High environmental stress
  parameterByIterTracking[i, "p_females_repro_yearling_highEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_yearling_highEnvStress_var", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_repro_adult_highEnvStress_var"] <- dapva::selectParamBetaDistribution(input_name = "p_females_repro_adult_highEnvStress_var", inputsDF = inputs)

  ######### Select the reproduction parameters for this iteration - number of offspring per female. #########
  parameterByIterTracking[i, "num_offspring_per_female_mean"] <- max(0, dapva::selectParamNormalDistribution(input_name = "num_offspring_per_female_mean", inputsDF = inputs)) # can't be neg, shortcut with max 0 is fine because distribution rarely, if ever, gets down there
  parameterByIterTracking[i, "num_offspring_per_female_sd_estimate"] <- max(0, dapva::selectParamNormalDistribution(input_name = "num_offspring_per_female_sd_estimate", inputsDF = inputs)) # can't be neg, shortcut with max 0 is fine because distribution rarely, if ever, gets down there

  parameterByIterTracking[i, "sex_ratio_males_birth"] <- dapva::selectParamBetaDistribution(input_name = "sex_ratio_males_birth", inputsDF = inputs, other_as_sd = TRUE) # To do, update the select from beta distribution function to take the other argument and

  ######### Select the climate parameters and data for this iteration. #########

  # Pull out the climate data specific to the requested climate scenario and
  # randomly choose one of the relevant models to use in this iteration
  chosen_scenario <- chooseClimateModel(
    scenario = inputs$best_guess[which(inputs$input == "scenario_climate")],
    dat_climate_future_lowCarbon_climate_scenarios,
    dat_climate_future_highCarbon_climate_scenarios,
    climate_high_carbon_scenario_prob = as.numeric(inputs$best_guess[which(inputs$input == "climate_high_carbon_scenario_prob")])
  )

  parameterByIterTracking[i, "climate_carbon_scenario"] <- chosen_scenario[[1]]
  parameterByIterTracking[i, "climate_model_num"] <- chosen_scenario[[2]] # no need to track this

  # Record which climate scenario this gets classed as
  if (parameterByIterTracking[i, "climate_carbon_scenario"] == "high") {
    parameterByIterTracking[i, "climate_scenario"] <- as.character(dat_climate_future_highCarbon_climate_scenarios$climate_scenario[as.numeric(parameterByIterTracking[i, "climate_model_num"])])
  }
  if (parameterByIterTracking[i, "climate_carbon_scenario"] == "low") {
    parameterByIterTracking[i, "climate_scenario"] <- as.character(dat_climate_future_lowCarbon_climate_scenarios$climate_scenario[as.numeric(parameterByIterTracking[i, "climate_model_num"])])
  }
  if (parameterByIterTracking[i, "climate_carbon_scenario"] == "NA - historical_Val_Marie") {
    parameterByIterTracking[i, "climate_scenario"] <- "NA - historical_Val_Marie"
  }

  ######### Select the carrying capacity parameters for this iteration. #########
  parameterByIterTracking[i, "colony_extent_K_scenario"] <- selectBTPDColonyExtentScenario(inputs)
  parameterByIterTracking[i, "density_K_med_high_env_stress"] <- max(3.81, dapva::selectParamNormalDistribution(input_name = "density_K_med_high_env_stress", inputsDF = inputs)) # 3.81 is a placeholder for a lower bound, from Sian 2013 visitual counts, may want to revisit shape of this distribution
  parameterByIterTracking[i, "density_K_low_env_stress"] <- max(3.81, dapva::selectParamNormalDistribution(input_name = "density_K_low_env_stress", inputsDF = inputs)) # 3.81 is a placeholder for a lower bound, from Sian 2013 visitual counts, may want to revisit shape of this distribution

  ######### Select the plague scenario for this iteration. #########
  if (inputs$best_guess[which(inputs$input == "scenario_plague")] == "best guess" ||
    inputs$best_guess[which(inputs$input == "scenario_plague")] == "Best Guess") {
    parameterByIterTracking[i, "plague_scenario"] <- sample(c("Conservative", "Moderate", "Extreme"),
      size = 1,
      prob = c(
        as.numeric(inputs$best_guess[which(inputs$input == "p_plague_Conservative")]),
        as.numeric(inputs$best_guess[which(inputs$input == "p_plague_Moderate")]),
        as.numeric(inputs$best_guess[which(inputs$input == "p_plague_Extreme")])
      ),
      replace = T
    )
  }

  if (inputs$best_guess[which(inputs$input == "scenario_plague")] == "Conservative") {
    parameterByIterTracking[i, "plague_scenario"] <- "Conservative"
  }
  if (inputs$best_guess[which(inputs$input == "scenario_plague")] == "Moderate") {
    parameterByIterTracking[i, "plague_scenario"] <- "Moderate"
  }
  if (inputs$best_guess[which(inputs$input == "scenario_plague")] == "Extreme") {
    parameterByIterTracking[i, "plague_scenario"] <- "Extreme"
  }

  ######### Select the plague parameters for this iteration. #########

  # Regular plague rates - depends on plague scenario, applied using paste0 in the input name
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_mean"] <- dapva::selectParamBetaDistribution(input_name = paste0("plague_rate_per_colony_per_year_", parameterByIterTracking[i, "plague_scenario"], "_mean"), inputsDF = inputs)
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_sd"] <- dapva::selectParamBetaDistribution(input_name = paste0("plague_rate_per_colony_per_year_", parameterByIterTracking[i, "plague_scenario"], "_sd"), inputsDF = inputs)

  # Increased plague rates if a colony is flagged because it was close to one that had plague the previous year - depends on plague scenario, applied using paste0 in the input name
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_increased_mean"] <- dapva::selectParamBetaDistribution(input_name = paste0("plague_rate_per_colony_per_year_", parameterByIterTracking[i, "plague_scenario"], "_increased_mean"), inputsDF = inputs)
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_increased_sd"] <- dapva::selectParamBetaDistribution(input_name = paste0("plague_rate_per_colony_per_year_", parameterByIterTracking[i, "plague_scenario"], "_increased_sd"), inputsDF = inputs)

  # Select plague survival rates
  parameterByIterTracking[i, "plague_survival_rate_mean"] <- dapva::selectParamBetaDistribution(input_name = "plague_survival_rate_mean", inputsDF = inputs)
  parameterByIterTracking[i, "plague_survival_rate_sd"] <- dapva::selectParamBetaDistribution(input_name = "plague_survival_rate_sd", inputsDF = inputs)

  ######### Select the 'plague parameters for this iteration'number of years too stressful' for calculating effective stress level from climate. #########

  int <- sample(c(
    as.numeric(inputs$lcl[which(inputs$input == "num_years_too_stressful_threshold")]),
    as.numeric(inputs$best_guess[which(inputs$input == "num_years_too_stressful_threshold")]),
    as.numeric(inputs$ucl[which(inputs$input == "num_years_too_stressful_threshold")])
  ), 1, prob = c(0.25, 0.5, 0.25))
  parameterByIterTracking[i, "num_years_too_stressful_threshold"] <- int


  ######### Fill in some inputs that will stay the same across iterations. Important to show for clarity and for use in the tornados (no uncertainty in these but if we put multiple runs with different inputs together it may be useful to have them in here) #########

  parameterByIterTracking[i, "max_dispersal_distance_m"] <- as.numeric(inputs$best_guess[which(inputs$input == "max_dispersal_distance_m")])
  parameterByIterTracking[i, "dispersal_threshold_prop_of_carrying_capacity"] <- as.numeric(inputs$best_guess[which(inputs$input == "dispersal_threshold_prop_of_carrying_capacity")])
  parameterByIterTracking[i, "climate_high_carbon_scenario_prob"] <- as.numeric(inputs$best_guess[which(inputs$input == "climate_high_carbon_scenario_prob")])
  parameterByIterTracking[i, "scenario_climate"] <- inputs$best_guess[which(inputs$input == "scenario_climate")]
  parameterByIterTracking[i, "scenario_plague"] <- inputs$best_guess[which(inputs$input == "scenario_plague")]
  parameterByIterTracking[i, "p_colony_extent_natural_exp_K"] <- as.numeric(inputs$best_guess[which(inputs$input == "p_colony_extent_natural_exp_K")])

  parameterByIterTracking[i, "p_plague_Conservative"] <- as.numeric(inputs$best_guess[which(inputs$input == "p_plague_Conservative")])
  parameterByIterTracking[i, "p_plague_Moderate"] <- as.numeric(inputs$best_guess[which(inputs$input == "p_plague_Moderate")])
  parameterByIterTracking[i, "p_plague_Extreme"] <- as.numeric(inputs$best_guess[which(inputs$input == "p_plague_Extreme")])
  parameterByIterTracking[i, "plague_distance_of_possible_spread_km"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_distance_of_possible_spread_km")])

  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Conservative_sd"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Conservative_sd")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Conservative_mean"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Conservative_mean")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Conservative_increased_sd"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Conservative_increased_sd")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Conservative_increased_mean"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Conservative_increased_mean")])

  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Moderate_sd"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Moderate_sd")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Moderate_mean"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Moderate_mean")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Moderate_increased_sd"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Moderate_increased_sd")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Moderate_increased_mean"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Moderate_increased_mean")])

  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Extreme_sd"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Extreme_sd")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Extreme_mean"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Extreme_mean")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Extreme_increased_sd"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Extreme_increased_sd")])
  parameterByIterTracking[i, "plague_rate_per_colony_per_year_Extreme_increased_mean"] <- as.numeric(inputs$best_guess[which(inputs$input == "plague_rate_per_colony_per_year_Extreme_increased_mean")])

  parameterByIterTracking[i, "quasi_extinction_threshold_num_indiv"] <- as.numeric(inputs$best_guess[which(inputs$input == "quasi_extinction_threshold_num_indiv")])

  parameterByIterTracking[i, "yrs"] <- as.numeric(inputs$best_guess[which(inputs$input == "yrs")])
  parameterByIterTracking[i, "initial_year"] <- as.numeric(inputs$best_guess[which(inputs$input == "initial_year")])

  ######### Return the results #########
  return(parameterByIterTracking)

  # for package checking; binding the variable locally to the function (see https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/)
  # do at end since these are dat files used in this function so can't have a the beginningx
  dat_N_initial_by_colony <- dat_age_structure_initial <- dat_K_colony_extent_by_colony <- dat_distance_btwn_colonies_m <- NULL
}

