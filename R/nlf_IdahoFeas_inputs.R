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

  #---- Reproduction: proportion of females reproductively active for each stage - mean.  -------------
  
  # LK PLACEHOLDERS FOR TESTING
  
  repro_prop_active_mean <- list(
    c(input = "p_females_lay_eggs_mean_A2", type = "reproduction", 
      lcl = 0.25, best_guess = 0.4, ucl = 0.6, confidence = 95,
      lower_bound = 0, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = ""),
    c(input = "p_females_lay_eggs_mean_A3", type = "reproduction", 
      lcl = 0.5, best_guess = 0.6, ucl = 0.7, confidence = 95,
      lower_bound = 0, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = "" ),
    c(input = "p_females_lay_eggs_mean_A4plus", type = "reproduction", 
      lcl = 0.5, best_guess = 0.6, ucl = 0.7, confidence = 95,
      lower_bound = 0, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = "")
  )
  
  #---- Reproduction: proportion of females reproductively active for each stage - sd (temporal variation).  -------------
  
  repro_prop_active_sd <- list(
    c(input = "p_females_lay_eggs_sd_A2", type = "survival", 
      lcl = 0, ucl = 0.28,
      best_guess_type = "NA", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 2021", comments = "no information on this; use uniform distribution from 0 to 0.28, where 0.28 is the largest std dev for a beta distribution"),
    
    c(input = "p_females_lay_eggs_sd_A3", type = "survival", 
      lcl = 0, ucl = 0.28,
      best_guess_type = "NA", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 2021", comments = "no information on this; use uniform distribution from 0 to 0.28, where 0.28 is the largest std dev for a beta distribution"),
    
    c(input = "p_females_lay_eggs_sd_A4plus", type = "survival", 
      lcl = 0, ucl = 0.28,
      best_guess_type = "NA", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 2021", comments = "no information on this; use uniform distribution from 0 to 0.28, where 0.28 is the largest std dev for a beta distribution")
  )
  
  
  #---- Reproduction: number of eggs per female.  -------------
  
  repro_clutch_size <- list(
    c(input = "num_eggs_per_active_female_mean_A2", type = "reproduction", 
      lcl = 2000, best_guess = 3000, ucl = 4000, confidence = 95,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = ""),
    c(input = "num_eggs_per_active_female_mean_A3", type = "reproduction", 
      lcl = 3000, best_guess = 4000, ucl = 5000, confidence = 95,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = ""),
    c(input = "num_eggs_per_active_female_mean_A4plus", type = "reproduction", 
      lcl = 6000, best_guess = 7000, ucl = 8000, confidence = 95,
      best_guess_type = "median", management_alternative = "status_quo", source = "", comments = "")
  )
  
  #---- Survival: mean, no threats.  -------------
  
  survival_rate_mean_no_threats <- list(
    c(input = "s_mean_eggs_no_threats", type = "survival", 
      lcl = 0.12, best_guess = 0.75, ucl = 0.9, confidence = 95,
      lower_bound = 0, upper_bound = 0.95,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "survival from eggs to tadpoles"),
    
    c(input = "s_mean_tadpoles_no_threats", type = "survival", 
      lcl = 0.01, best_guess = 0.04, ucl = 0.1, confidence = 80,
      lower_bound = 0, upper_bound = 0.25,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "survival from tadpoles to yoy"),
    
    c(input = "s_mean_yoy_no_threats", type = "survival", 
      lcl = 0.05, best_guess = 0.1, ucl = 0.31, confidence = 80,
      lower_bound = 0, upper_bound = 0.5,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "annual survival from yoy to juv (one year old adult), includes overwinter survival"),
    
    c(input = "s_mean_juv_no_threats", type = "survival", 
      lcl = 0.2, best_guess = 0.4, ucl = 0.6, confidence = 80,
      lower_bound = 0, upper_bound = 0.9,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "annual survival from juv (one year old adult) to adult (2 years), includes overwinter survival"),
    
    c(input = "s_mean_adult_no_threats", type = "survival", 
      lcl = 0.36, best_guess = 0.6, ucl = 0.7, confidence = 80,
      lower_bound = 0, upper_bound = 0.9,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "annual survival for adults (age 2 and older), includes overwinter survival")
    
  )
  
  #---- Survival: standard deviation (temporal variation), no threats.  -------------
  
  survival_rate_sd_no_threats <- list(
    c(input = "s_sd_eggs_no_threats", type = "survival", 
      lcl = 0, ucl = 0.28,
      best_guess_type = "NA", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 2021", comments = "no information on this; use uniform distribution from 0 to 0.28, where 0.28 is the largest std dev for a beta distribution"),
    
    c(input = "s_sd_tadpoles_no_threats", type = "survival", 
      lcl = 0, ucl = 0.28,
      best_guess_type = "NA", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 2021", comments = "no information on this; use uniform distribution from 0 to 0.28, where 0.28 is the largest std dev for a beta distribution"),
    
    c(input = "s_sd_yoy_no_threats", type = "survival", 
      lcl = 0, ucl = 0.28,
      best_guess_type = "NA", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 2021", comments = "no information on this; use uniform distribution from 0 to 0.28, where 0.28 is the largest std dev for a beta distribution"),
    
    c(input = "s_sd_juv_no_threats", type = "survival", 
      lcl = 0, ucl = 0.28,
      best_guess_type = "NA", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 2021", comments = "no information on this; use uniform distribution from 0 to 0.28, where 0.28 is the largest std dev for a beta distribution"),
    
    c(input = "s_sd_adult_no_threats", type = "survival", 
      lcl = 0, ucl = 0.28,
      best_guess_type = "NA", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 2021", comments = "no information on this; use uniform distribution from 0 to 0.28, where 0.28 is the largest std dev for a beta distribution")
    
  )
  
  #---- Survival: % reduction due to bullfrogs.  -------------
  
  survival_pctReduction_bullfrogs <- list(
    c(input = "s_pct_reduced_eggs_bullfrogs", type = "survival", 
      lcl = 10, best_guess = 50, ucl = 90, confidence = 95,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "LK PLACEHOLDER", comments = "% reduction in survival due to bullfrog presence"),

    c(input = "s_pct_reduced_tadpoles_bullfrogs", type = "survival", 
      lcl = 15, best_guess = 35, ucl = 50, confidence = 70,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "% reduction in survival due to bullfrog presence"),
    
    c(input = "s_pct_reduced_yoy_bullfrogs", type = "survival", 
      lcl = 12, best_guess = 22, ucl = 35, confidence = 70,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "% reduction in survival due to bullfrog presence"),

    c(input = "s_pct_reduced_juvenile_bullfrogs", type = "survival", 
      lcl = 10, best_guess = 20, ucl = 30, confidence = 70,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "% reduction in survival due to bullfrog presence"),
    
    c(input = "s_pct_reduced_adult_bullfrogs", type = "survival", 
      lcl = 5, best_guess = 10, ucl = 15, confidence = 70,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "% reduction in survival due to bullfrog presence")
  )
  
  #---- Human management: probability that bullfrog management will be effective.  -------------
  
  prob_bullfrog_mgmt_effective <- list(
    c(input = "bullfrogMgmt_effective", type = "human_management", 
      best_guess = 0.8, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "8/10 chance that bullfrog managmenet efforts will keep bullfrog #s low enough that they will not affect NLF survival rates")
    )
  

  #---- Compile the inputs and set up tracking objects.  -------------
  inputs <- dapva::makeInputsDF(parameters = c(
    year_inputs,
    repro_prop_active_mean,
    repro_prop_active_sd,
    repro_clutch_size,
    survival_rate_mean_no_threats,
    survival_rate_sd_no_threats,
    survival_pctReduction_bullfrogs,
    prob_bullfrog_mgmt_effective
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

  # Choose and record the reproduction parameters for this iteration - mean proportion of females who lay eggs
  parameterByIterTracking[i, "p_females_lay_eggs_mean_A2"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A2", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_lay_eggs_mean_A3"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A3", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_lay_eggs_mean_A4plus"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A4plus", inputsDF = inputs)
  
  # Choose and record the reproduction parameters for this iteration - sd proportion of females who lay eggs
  parameterByIterTracking[i, "p_females_lay_eggs_sd_A2"] <- dapva::selectParamUniformDistribution(input_name = "p_females_lay_eggs_sd_A2", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_lay_eggs_sd_A3"] <- dapva::selectParamUniformDistribution(input_name = "p_females_lay_eggs_sd_A3", inputsDF = inputs)
  parameterByIterTracking[i, "p_females_lay_eggs_sd_A4plus"] <- dapva::selectParamUniformDistribution(input_name = "p_females_lay_eggs_sd_A4plus", inputsDF = inputs)
  
  ######### Select the reproduction parameters for this iteration - number of offspring per female. #########
  parameterByIterTracking[i, "num_eggs_per_active_female_mean_A2"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A2", inputsDF = inputs)
  parameterByIterTracking[i, "num_eggs_per_active_female_mean_A3"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A3", inputsDF = inputs)
  parameterByIterTracking[i, "num_eggs_per_active_female_mean_A4plus"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A4plus", inputsDF = inputs)
  
  ######### Select the survival parameters for this iteration - mean survival rates no threats. #########
  parameterByIterTracking[i, "s_mean_eggs_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_eggs_no_threats", inputsDF = inputs)
  parameterByIterTracking[i, "s_mean_tadpoles_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_tadpoles_no_threats", inputsDF = inputs)
  parameterByIterTracking[i, "s_mean_yoy_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_yoy_no_threats", inputsDF = inputs)
  parameterByIterTracking[i, "s_mean_juv_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_juv_no_threats", inputsDF = inputs)
  parameterByIterTracking[i, "s_mean_adult_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_adult_no_threats", inputsDF = inputs)
  
  ######### Select the survival parameters for this iteration - temporal variance in survival rates no threats. #########
  parameterByIterTracking[i, "s_sd_eggs_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_eggs_no_threats", inputsDF = inputs)
  parameterByIterTracking[i, "s_sd_tadpoles_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_tadpoles_no_threats", inputsDF = inputs)
  parameterByIterTracking[i, "s_sd_yoy_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_yoy_no_threats", inputsDF = inputs)
  parameterByIterTracking[i, "s_sd_juv_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_juv_no_threats", inputsDF = inputs)
  parameterByIterTracking[i, "s_sd_adult_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_adult_no_threats", inputsDF = inputs)
  
  
  ######### Select the survival parameters for this iteration - % reduction in survival due to bullfrogs. #########
  parameterByIterTracking[i, "s_pct_reduced_eggs_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_eggs_bullfrogs", inputsDF = inputs)
  parameterByIterTracking[i, "s_pct_reduced_tadpoles_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_tadpoles_bullfrogs", inputsDF = inputs)
  parameterByIterTracking[i, "s_pct_reduced_yoy_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_yoy_bullfrogs", inputsDF = inputs)
  parameterByIterTracking[i, "s_pct_reduced_juvenile_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_juvenile_bullfrogs", inputsDF = inputs)
  parameterByIterTracking[i, "s_pct_reduced_adult_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_adult_bullfrogs", inputsDF = inputs)
  
  
  
  ######### Select the human management parameters for this iteration - bullfrog management effective or not. #########
  
  parameterByIterTracking[i, "bullfrogMgmt_effective"] <- sample(c("yes", "no"),
                                                          size = 1,
                                                          prob = c(as.numeric(inputs$best_guess[which(inputs$input == "bullfrogMgmt_effective")]),
                                                                   (1-as.numeric(inputs$best_guess[which(inputs$input == "bullfrogMgmt_effective")]))
                                                                   ), replace = T)
  
  
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

