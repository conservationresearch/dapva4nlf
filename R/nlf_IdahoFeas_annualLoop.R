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
#'   format of selectBTPDparameterByIterTracking().
#' @param yrs The number of years to run the model for (e.g. 50).
#' @param i The iteration number. For results tracking purposes.
#' @param q The run number. For results tracking purposes.
#' @param wetland_distances_km Matrix of distances between wetlands in
#'   kilometers.
#' @param initial_year First year of the model.
#' @param wetlands Wetland cell names/abbreviations.
#' @param stage_classes ADD DESCRIPTION LATER
#' @param alternative_details ADD DESCRIPTION LATER
#' @param percentilesEV_survival_eggs_tad Columns are the different wetlands,
#'   rows are a vector of length years with percentiles to use for environmental
#'   variation (EV) in each iteration (e.g. output from
#'   selectEVPercentilesNormal()).
#' @param percentilesEV_survival_yoy_adult Columns are the different wetlands,
#'   rows are a vector of length years with percentiles to use for environmental
#'   variation (EV) in each iteration (e.g. output from
#'   selectEVPercentilesNormal())..
#' @param percentilesEV_reproduction Columns are the different wetlands, rows
#'   are a vector of length years with percentiles to use for environmental
#'   variation (EV) in each iteration (e.g. output from
#'   selectEVPercentilesNormal()).
#' @param exisiting_pop TRUE or FALSE if want to run it as if there is an
#'   existing population there. Defaults to FALSE. Turn to TRUE only when
#'   exploring the implications of having an established population from the
#'   start.
#' @param dispersal_allowed_outside TRUE OR FALSE, defaults to TRUE
#'
#' @importFrom dplyr %>%
#'
#' @export
runAnnualLoopNLFIdahoPVA <- function(parameterByIterTracking, yrs, i, q,
                                     wetland_distances_km,
                                 initial_year, wetlands,stage_classes,
                                 percentilesEV_survival_eggs_tad,
                                 percentilesEV_survival_yoy_adult,
                                 percentilesEV_reproduction,
                                 alternative_details, 
                                 exisiting_pop = FALSE, 
                                 dispersal_allowed_outside = TRUE) {
  
    # CHECK - DO WE ACCOUNT FOR ALTERNATIVE WITH AND WITHOUT EPHEMERAL WETLANDS
  # WHEN IMPLEMENTING DISPERSAL
  
  
  # start.time <- Sys.time()
  # Rprof()    ## Turn on the profiler
  
  # Collect some info
  n_wetlands <- length(wetlands)
  wetlands_without_outside <- wetlands[which(wetlands != "outside")]

  # Make a data frame to track the results
  # Don't include the 'outside' bucket because we are not going to track those ones
  resultsTracking_popSize_females <- dapva::makeResultsTracking(i,
    yrs = yrs, initial_year = initial_year,
    pops = wetlands_without_outside , class_names = stage_classes,
    sex = "female"
  )
  
  resultsTracking_popSize_females$run <- q # this isn't included in the original makeResultsTracking function, could update later

  for (j in 1:yrs) { # start the annual loop
    print(paste("iteration", i, "run", q, "year", j))

    ######### Apply environmental stochasticity (EV) to select survival rates in the absence of threats. #########
    # Note that the wetlands have different rates for each life stage

    s_eggs_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_eggs_no_threats")])
    s_eggs_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_eggs_no_threats")])
    s_eggs <- unlist(lapply(1:length(wetlands_without_outside), function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = s_eggs_mean,
        sd = s_eggs_sd,
        EV_percentile = percentilesEV_survival_eggs_tad[j, paste(wetlands_without_outside[x])]
      )
      #   neg_shape_param_update_sd = TRUE,
      #   neg_shape_param_update_sd_pct_mean = 10 # need this because the uniform SD means that sometimes the SD is too big for the mean, the 10% is from Karen and Travis' NLF pop modelling paper
      #   
      # )
    }))

    s_tadpoles_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_tadpoles_no_threats")])
    s_tadpoles_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_tadpoles_no_threats")])
    s_tadpoles <- unlist(lapply(1:length(wetlands_without_outside), function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = s_tadpoles_mean,
        sd = s_tadpoles_sd,
        EV_percentile = percentilesEV_survival_eggs_tad[j, paste(wetlands_without_outside[x])]
        # neg_shape_param_update_sd = TRUE,
        # neg_shape_param_update_sd_pct_mean = 10 # need this because the uniform SD means that sometimes the SD is too big for the mean, the 10% is from Karen and Travis' NLF pop modelling paper
        # 
      )
    }))    
    
    # Lea's insight is that in every translocation experience so far, there are always some tadpoles that become metamorophs
    # So doesn't make sense for there to ever be none that survive
    # We are getting that quite a bit because we have the uniform distribution on survival
    # TEST: Try this rule: survival can not be less than 1%
    
    s_tadpoles <- pmax(0.01, s_tadpoles)  # See if that helps make it more realistic
    
   
    
    
    s_yoy_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_yoy_no_threats")])
    s_yoy_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_yoy_no_threats")])
    s_yoy <- unlist(lapply(1:length(wetlands_without_outside), function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = s_yoy_mean,
        sd = s_yoy_sd,
        EV_percentile = percentilesEV_survival_yoy_adult$all_wetlands[j]
        # neg_shape_param_update_sd = TRUE,
        # neg_shape_param_update_sd_pct_mean = 10 # need this because the uniform SD means that sometimes the SD is too big for the mean, the 10% is from Karen and Travis' NLF pop modelling paper
        # 
      )
    }))
    
    s_juv_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_juv_no_threats")])
    s_juv_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_juv_no_threats")])
    s_juv <- unlist(lapply(1:length(wetlands_without_outside), function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = s_juv_mean,
        sd = s_juv_sd,
        EV_percentile = percentilesEV_survival_yoy_adult$all_wetlands[j]
        # neg_shape_param_update_sd = TRUE,
        # neg_shape_param_update_sd_pct_mean = 10 # need this because the uniform SD means that sometimes the SD is too big for the mean, the 10% is from Karen and Travis' NLF pop modelling paper
        # 
      )
    }))
    
    s_adult_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_adult_no_threats")])
    s_adult_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_adult_no_threats")])
    s_adult <- unlist(lapply(1:length(wetlands_without_outside), function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = s_adult_mean,
        sd = s_adult_sd,
        EV_percentile = percentilesEV_survival_yoy_adult$all_wetlands[j]
        # neg_shape_param_update_sd = TRUE,
        # neg_shape_param_update_sd_pct_mean = 10 # need this because the uniform SD means that sometimes the SD is too big for the mean, the 10% is from Karen and Travis' NLF pop modelling paper
        # 
      )
    }))    
    
    # Pull all together into one dataframe for tidiness and label them by wetland
    # repeat adult survival 3 times for each of the 3 adult stage classes that we have for reproduction
    survival_rates_noThreats <- as.data.frame(cbind(s_eggs, s_tadpoles, s_yoy, s_juv, s_adult, s_adult, s_adult))
    rownames(survival_rates_noThreats) <- wetlands_without_outside
    colnames(survival_rates_noThreats) <- stage_classes
    
    
    ######### Identify if bullfrogs are a threat for this iteration. #########
    bullfrog_management_happening <- alternative_details$bullfrog_management
    
    if(bullfrog_management_happening == "no"){
      # effectiveness has been decided at the iteration level
      bullfrogMgmt_effective <- "no"
    }
    
    if(bullfrog_management_happening == "yes"){
      # effectiveness has been decided at the iteration level
      bullfrogMgmt_effective <- parameterByIterTracking[i, paste0("bullfrogMgmt_effective")]  
    }
    
    if(bullfrogMgmt_effective == "no"){
      s_pct_reduced_eggs_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_bullfrogs")])
      s_pct_reduced_tadpoles_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_bullfrogs")])
      s_pct_reduced_yoy_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_bullfrogs")])
      s_pct_reduced_juv_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_bullfrogs")])
      s_pct_reduced_adult_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_bullfrogs")])
    }
    
    if(bullfrogMgmt_effective == "yes"){ # then this threat is eliminated
      s_pct_reduced_eggs_bullfrogs <- 0
      s_pct_reduced_tadpoles_bullfrogs <- 0
      s_pct_reduced_yoy_bullfrogs <- 0
      s_pct_reduced_juv_bullfrogs <- 0
      s_pct_reduced_adult_bullfrogs <- 0
    }
    
    ######### Identify if wetland drawdowns after mid-July are a threat for this iteration and year. #########
    drawdown_beforeMidJuly <- parameterByIterTracking[i, paste0("drawdown_beforeMidJuly")]  
    if(drawdown_beforeMidJuly == "no"){
      drawdown_relevant <- "no"
    }
    if(drawdown_beforeMidJuly == "yes"){
      drawdown_completeVSpartial_freq <- parameterByIterTracking[i, paste0("drawdown_completeVSpartial_freq")]  
      drawdown_status_this_year <- sample(c("complete", "partial"), size = 1, 
                                            prob = c(drawdown_completeVSpartial_freq/100, 
                                                     1-drawdown_completeVSpartial_freq/100))
  
    
      # Specify the drawdown schedule
      # Each wetland is drawndown once ever 7 yeras 
      drawdown_schedule <- sample(c("cell1and2", "cell3", "cell4", "cell5", "cell6", "cell7", "cell8"), 
             size = 50, replace = TRUE) # TO DO - fix size to be nyears
      
      # This year, is one of our nlf wetlands being drawn down AND is it after translocations are done?
      drawdown_location_this_year <- drawdown_schedule[j]
      if (j <= as.numeric(alternative_details$n_years_transplocation)){translocation_year <- "yes"} # to do - also use this naming later on in the code when adding tadpoles or not
      if (j > as.numeric(alternative_details$n_years_transplocation)){translocation_year <- "no"}
      
      # NLF wetland affected
      nlf_wetland_affected <- "no" # initalize
      int2 <- match(drawdown_location_this_year, wetlands_without_outside)
      if(is.na(int2) == FALSE){ nlf_wetland_affected <- "yes"}
      
      if(nlf_wetland_affected == "no"| translocation_year == "yes"){
        drawdown_relevant <- "no"
      }
      if(nlf_wetland_affected == "yes"  && translocation_year == "no"){
        drawdown_relevant <- "yes"
      }
    }
    
    if(drawdown_relevant == "no"){
      s_pct_reduced_tadpoles_drawdown <- 0
    }
    
    if(drawdown_relevant == "yes"){
      if(drawdown_status_this_year == "partial"){
        s_pct_reduced_tadpoles_drawdown <- parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_drawdownComplete")] 
      }
      if(drawdown_status_this_year == "complete"){
        s_pct_reduced_tadpoles_drawdown <- parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_drawdownPartial")] 
      }
    }
    
    ######### Identify if the ephemeral wetland goes dry this year. #########
    ephemeral_freq_dry <- parameterByIterTracking[i, paste0("ephemeral_freq_dry")] 
    ephemeral_wetlands_dry <- sample(c("yes", "no"), size = 1, prob = (c(ephemeral_freq_dry, 1-ephemeral_freq_dry)))

    if(ephemeral_wetlands_dry == "no"){ s_pct_reduced_tadpoles_epehemeral_dry <- 0}
    if(ephemeral_wetlands_dry == "yes"){
      s_pct_reduced_tadpoles_epehemeral_dry <- parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_drawdownComplete")] 
    }
      
    
    ######### Apply threat reductions to survival rates. #########
    # treat multiple threats multiplicitively
    # we know roads, are all present all the time
    survival_rates_afterThreats <- survival_rates_noThreats# initalize
    
    # Calculate the percent reduction due to threats
    s_eggs_afterThreats <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_roads")])/100)*
      (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_chytrid")])/100)*
      (1-s_pct_reduced_eggs_bullfrogs/100)*
      s_eggs

    s_tadpoles_afterThreats <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_roads")])/100)*
      (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_chytrid")])/100)*
      (1-s_pct_reduced_tadpoles_bullfrogs/100)*
      s_tadpoles
    
    s_yoy_afterThreats <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_roads")])/100)*
      (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_chytrid")])/100)*
      (1-s_pct_reduced_yoy_bullfrogs/100)*
      s_yoy
    
    s_juv_afterThreats <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_roads")])/100)*
      (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_adult_chytrid")])/100)*
      (1-s_pct_reduced_juv_bullfrogs/100)*
      s_juv
    
    s_adult_afterThreats <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_roads")])/100)*
      (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_adult_chytrid")])/100)*
      (1-s_pct_reduced_adult_bullfrogs/100)*
      s_adult
    
    # Update tadpoles if drawdown is a problem this year
    if(drawdown_relevant == "yes"){
      wetland_id <- which(wetlands_without_outside == drawdown_location_this_year)
      s_tadpoles_afterThreats[wetland_id] <- s_tadpoles_afterThreats[wetland_id]*
        (1-s_pct_reduced_tadpoles_drawdown/100)
    }
    
    # ALso update tadpoles for ephemeral wetlands potentially being dry
    ephWetland_id <- which(wetlands_without_outside == "ephemeral_wetlands")
    s_tadpoles_afterThreats[ephWetland_id] <- s_tadpoles_afterThreats[ephWetland_id]*
      (1-s_pct_reduced_tadpoles_epehemeral_dry/100)
    
    # Combine all the survival rates for use going forward
    survival_rates_afterThreats <- as.data.frame(cbind(s_eggs_afterThreats,
                                                       s_tadpoles_afterThreats, 
                                                       s_yoy_afterThreats, 
                                                       s_juv_afterThreats,
                                                       s_adult_afterThreats, 
                                                       s_adult_afterThreats, 
                                                       s_adult_afterThreats))
    rownames(survival_rates_afterThreats) <- wetlands_without_outside
    colnames(survival_rates_afterThreats) <- stage_classes
    
    ######### Make the survival matrix. #########
    # Make the overall survival matrix. Note that survival from eggs to tadpoles 
    # and tadpoles to yoy will need to be treated seperately from the others becaue they happen within a timestep (year)
    survivalMatrix <- dapva::makeSurvivalMatrixMultiplePop(class_names = stage_classes, 
                                                    pop_names = wetlands_without_outside, 
                                                    survival_rates_afterThreats)
    
    ######### Apply environmental stochasticity (EV) to select reproductive rates - prop of females who lay eggs. #########
    # Note that the wetlands have different rates for each life stage    
    
    p_females_lay_eggs_mean_A2 <- as.numeric(parameterByIterTracking[i, paste0("p_females_lay_eggs_mean_A2")])
    p_females_lay_eggs_sd_A2 <- as.numeric(parameterByIterTracking[i, paste0("p_females_lay_eggs_sd_A2")])
    p_females_lay_eggs_A2 <- unlist(lapply(1:length(wetlands_without_outside), function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = p_females_lay_eggs_mean_A2,
        sd = p_females_lay_eggs_sd_A2,
        EV_percentile = percentilesEV_reproduction$all_wetlands[j],
          neg_shape_param_update_sd = TRUE,
          neg_shape_param_update_sd_pct_mean = 10 # need this because the uniform SD means that sometimes the SD is too big for the mean, the 10% is from Karen and Travis' NLF pop modelling paper
      )
        })) 
    
    p_females_lay_eggs_mean_A3 <- as.numeric(parameterByIterTracking[i, paste0("p_females_lay_eggs_mean_A3")])
    p_females_lay_eggs_sd_A3 <- as.numeric(parameterByIterTracking[i, paste0("p_females_lay_eggs_sd_A3")])
    p_females_lay_eggs_A3 <- unlist(lapply(1:length(wetlands_without_outside), function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = p_females_lay_eggs_mean_A3,
        sd = p_females_lay_eggs_sd_A3,
        EV_percentile = percentilesEV_reproduction$all_wetlands[j],
        neg_shape_param_update_sd = TRUE,
        neg_shape_param_update_sd_pct_mean = 10 # need this because the uniform SD means that sometimes the SD is too big for the mean, the 10% is from Karen and Travis' NLF pop modelling paper
        
      )
    })) 
    
    p_females_lay_eggs_mean_A4plus <- as.numeric(parameterByIterTracking[i, paste0("p_females_lay_eggs_mean_A4plus")])
    p_females_lay_eggs_sd_A4plus <- as.numeric(parameterByIterTracking[i, paste0("p_females_lay_eggs_sd_A4plus")])
    p_females_lay_eggs_A4plus <- unlist(lapply(1:length(wetlands_without_outside), function(x) {
      dapva::selectPercentileBetaDistribution(
        mean = p_females_lay_eggs_mean_A4plus,
        sd = p_females_lay_eggs_sd_A4plus,
        EV_percentile = percentilesEV_reproduction$all_wetlands[j],
        neg_shape_param_update_sd = TRUE,
        neg_shape_param_update_sd_pct_mean = 10 # need this because the uniform SD means that sometimes the SD is too big for the mean, the 10% is from Karen and Travis' NLF pop modelling paper
        
      )
    })) 
    
    # Pull all together into one dataframe for tidiness and label them by wetland
    p_females_lay_eggs <- as.data.frame(cbind(0,0,0,0, p_females_lay_eggs_A2, p_females_lay_eggs_A3,p_females_lay_eggs_A4plus))
    rownames(p_females_lay_eggs) <- wetlands_without_outside
    colnames(p_females_lay_eggs) <- stage_classes

    ######### Clarify # of eggs per female - using mean, no temporal variation applied. #########
    
    # No SD assessed by experts; agreed to just use mean estimates with the parametric variation
    # Therefore also the same across wetlands
    # Reasonable since some temporal variation expressed in the proportion of reproductively active females anyways
    # Could adjust here in the future if estimates of temporal variation become available
    num_eggs_per_active_female_mean_A2 <- round(as.numeric(parameterByIterTracking[i, paste0("num_eggs_per_active_female_mean_A2")]))
    num_eggs_per_active_female_A2 <- rep(num_eggs_per_active_female_mean_A2, length(wetlands_without_outside))
    
    num_eggs_per_active_female_mean_A3 <- round(as.numeric(parameterByIterTracking[i, paste0("num_eggs_per_active_female_mean_A3")]))
    num_eggs_per_active_female_A3 <- rep(num_eggs_per_active_female_mean_A3, length(wetlands_without_outside))
    
    num_eggs_per_active_female_mean_A4plus <- round(as.numeric(parameterByIterTracking[i, paste0("num_eggs_per_active_female_mean_A4plus")]))
    num_eggs_per_active_female_A4plus <- rep(num_eggs_per_active_female_mean_A4plus, length(wetlands_without_outside))
    
    # Pull all together into one dataframe for tidiness and label them by wetland
    num_eggs_per_active_female <- as.data.frame(cbind(0,0,0,0, num_eggs_per_active_female_A2, num_eggs_per_active_female_A3, num_eggs_per_active_female_A4plus))
    rownames(num_eggs_per_active_female) <- wetlands_without_outside
    colnames(num_eggs_per_active_female) <- stage_classes
    
    ######### Combine reproduction values into a large vector for all wetlands #########
    # We will need this format to apply the reproduction values.
    
    pop_class_names <- paste(resultsTracking_popSize_females$pop, resultsTracking_popSize_females$class)
      
    propFemalesReproVector <- dapva::makePropFemalesReproVector(
      pop_class_names, 
      prop_females_repro = as.list(t(p_females_lay_eggs)))
    
    meanNumOffspringPerFemaleVector <- dapva::makeMeanNumOffspringPerFemaleVector(
      pop_class_names,
      mean_num_offspring_per_female = as.list(t(num_eggs_per_active_female)))
    
    
  
    
    ######### In the first year, add any translocated tadpoles and see how many survive to yoy #########
    # No NLF there at the moment so no other initial condition
    if (j == 1) {
      
      # Initialize with no frogs since currently extirpated
      resultsTracking_popSize_females[, "1"] <- 0
      
      # populate with initial conditions
      # Tadpoles get translocated in!
      
      if(alternative_details$release_location == "cell7"){
        rows_for_trans <- intersect(which(resultsTracking_popSize_females$class == "tadpoles"),
          which(resultsTracking_popSize_females$pop == "cell7"))
      }
      if(alternative_details$release_location == "all_three"){
        rows_for_trans <- intersect(which(resultsTracking_popSize_females$class == "tadpoles"),
          (which(resultsTracking_popSize_females$pop == "cell7" | 
                   resultsTracking_popSize_females$pop == "cell3" |
                           resultsTracking_popSize_females$pop == "cell4")
                   )
        )
      }
      
      if(alternative_details$release_location == "none"){
        rows_for_trans <- NA
      }
      

      n_female_tadpoles_released <- round(as.numeric(alternative_details$n_tadpoles_per_year)/2) # assume 50/50 sex ratio, round so release whole individuals :)
      
      if(is.na(rows_for_trans)[1] == FALSE){
        resultsTracking_popSize_females[rows_for_trans, "1"] <- round(n_female_tadpoles_released /length(rows_for_trans)) # round so that releasing whole individuals :)
      }
        
      
      
      # Then apply survival rates to see how many of the tadpoles survive to be yoy
      
      # Pull out the population sizes for this year
      popSizeVector <- dapva::makePopSizeVector(
        pop_class_names = paste(resultsTracking_popSize_females$pop, resultsTracking_popSize_females$class),
        num_indiv_per_pop_class = resultsTracking_popSize_females[,paste(j)],
        sex = "female"
      )
      # Apply the survival matrix to the tadpoles to see how many survive to be yoy
      tapoles_survive_to_yoy <- survivalTadpoles(popSizeVector, 
                               survivalMatrix,
                               demographic_stochasticity = TRUE)
      yoy_rows <- which(resultsTracking_popSize_females$class == "yoy")
      resultsTracking_popSize_females[yoy_rows, paste(j)] <- tapoles_survive_to_yoy[grepl("yoy" , rownames(tapoles_survive_to_yoy))]

      # And now those yoy disperse
      dispersal_results <- dapva4nlf::dispersalTracking(resultsTracking_popSize_females, yoy_rows, i, j,
                                             wetlands,
                                             wetland_distances_km,
                                             parameterByIterTracking,
                                             allow_outside = dispersal_allowed_outside)

      # for each wetland, remove the # that left and then add them to the wetland they went to
      for(z in 1:length(yoy_rows)){
        wetland <- resultsTracking_popSize_females$pop[yoy_rows[z]]
        n_left <- sum(dispersal_results[paste(wetland),]) # rowsum for this wetland
        n_arrived <- sum(dispersal_results[,paste(wetland)]) # colsum for this wetland
        resultsTracking_popSize_females[yoy_rows[z], paste(j)] <- resultsTracking_popSize_females[yoy_rows[z], paste(j)] - n_left + n_arrived
      }
      
      if(exisiting_pop == TRUE){
        # Add in existing population sizes
        # Assume starting out at carrying capacity some proportion of the carrying capacity
        total_n_terr_fem <- as.numeric(parameterByIterTracking[i, paste0("starting_pop_terr_fem")])

        # Age distribution from Rebecca's email: 
        #Based on Leclair and Castanet and Merrell 1977 I think that in late summer the ratio of 
        # YOY:A1:A2:A3:A4 (based on 17:1) would be 17:0.56:0.40:0.02: 0.02
        # Or 94% YOY, 3% A1, 2% A2, 0.1% A3, 0.1% A4
        
        n_yoy <- round((17/18)*total_n_terr_fem) # round so whole individuals :)
        n_juv <- round((0.56/18)*total_n_terr_fem) 
        n_A2 <- round((0.40/18)*total_n_terr_fem) 
        n_A3 <- round((0.02/18)*total_n_terr_fem) 
        n_A4plus <- round((0.02/18)*total_n_terr_fem)  #0.009, close enough to Rebecca's 0.1%
        
        n_wetlands <- length(wetlands_without_outside)
        
        rows_yoy <- which(resultsTracking_popSize_females$class == "yoy")
        resultsTracking_popSize_females[rows_yoy, "1"] <- resultsTracking_popSize_females[rows_yoy, "1"] + round(n_yoy/n_wetlands)
        
        rows_juv <- which(resultsTracking_popSize_females$class == "juv")
        resultsTracking_popSize_females[rows_juv, "1"] <- resultsTracking_popSize_females[rows_juv, "1"] + round(n_juv/n_wetlands)
        
        rows_A2 <- which(resultsTracking_popSize_females$class == "A2")
        resultsTracking_popSize_females[rows_A2, "1"] <- resultsTracking_popSize_females[rows_A2, "1"] + round(n_A2/n_wetlands)
        
        rows_A3 <- which(resultsTracking_popSize_females$class == "A3")
        resultsTracking_popSize_females[rows_A3, "1"] <- resultsTracking_popSize_females[rows_A3, "1"] + round(n_A3/n_wetlands)
        
        rows_A4plus <- which(resultsTracking_popSize_females$class == "A4plus")
        resultsTracking_popSize_females[rows_A4plus, "1"] <- resultsTracking_popSize_females[rows_A4plus, "1"] + round(n_A4plus/n_wetlands)
        
      }
      
    } # end of if j = 1

    ######### For the second year and beyond #########
    if (j != 1)  { 
      
      ######### Get the population sizes from the fall last year #########
      popSizeVector_fallLastYear <- dapva::makePopSizeVector(
        pop_class_names = paste(resultsTracking_popSize_females$pop, resultsTracking_popSize_females$class),
        num_indiv_per_pop_class = as.numeric(resultsTracking_popSize_females[,paste(j-1)]),
        sex = "female"
      )
      
      ######### Each spring, check to see how many of each stage class survived the winter #########
      survived_the_winter <- survivalOverwinter(popSizeVector_fallLastYear, 
                                                survivalMatrix,
                                                demographic_stochasticity = TRUE)

      # Add it to our tracking matrix for this year
      resultsTracking_popSize_females[,paste(j)] <- survived_the_winter
      
      # Make a population size vector for the situation in the spring
      popSizeVector_springThisYear <- dapva::makePopSizeVector(
        pop_class_names = paste(resultsTracking_popSize_females$pop, resultsTracking_popSize_females$class),
        num_indiv_per_pop_class =  as.numeric(resultsTracking_popSize_females[,paste(j)]),
        sex = "female")
      
      ######### Then see how many eggs are laid #########
      
      # future to do, update this function so don't need the number of males
      # future to do - fix so that wetland names don't need to be in alphabetical order to work...
      int <- dapva::applyReproduction(
        popSizeVectorFemales = popSizeVector_springThisYear, popSizeVectorMales = popSizeVector_springThisYear,
        propFemalesReproVector, meanNumOffspringPerFemaleVector,
        sex_ratio_prop_female_at_birth = 0.5, # assume 50 50 sex ratio, to do later - update so this is an input
        demographic_stochasticity_NumOffspringPerFemale = TRUE,
        demographic_stochasticity_sex_ratio = TRUE, 
        name_of_baby_class = "eggs"
      )
      int[[1]]
      
      # Add it to our tracking matrix for this year
      resultsTracking_popSize_females[,paste(j)] <- int[[1]]
      
      # Update the pop size vector
      popSizeVector_withEggs <- dapva::makePopSizeVector(
        pop_class_names = paste(resultsTracking_popSize_females$pop, resultsTracking_popSize_females$class),
        num_indiv_per_pop_class =  as.numeric(resultsTracking_popSize_females[,paste(j)]),
        sex = "female")
      
      ######### See how many eggs hatch into tadpoles #########
      eggs_survive_to_tadpoles <- survivalEggs(round(popSizeVector_withEggs), 
                                                 survivalMatrix,
                                                 demographic_stochasticity = TRUE)
      tadpole_rows <- which(resultsTracking_popSize_females$class == "tadpoles")
      resultsTracking_popSize_females[tadpole_rows, paste(j)] <- eggs_survive_to_tadpoles[grepl("tadpole" , rownames(eggs_survive_to_tadpoles))]
      
      # If it is a translocation year, add the translocated tadpoles
      if(j <= as.numeric(alternative_details$n_years_transplocation)){ # then it is a transloication year
        # see n_female_tadpoles_released from year 1 above
        resultsTracking_popSize_females[rows_for_trans, paste(j)] <- resultsTracking_popSize_females[rows_for_trans, paste(j)] + round(n_female_tadpoles_released /length(rows_for_trans)) # round so that releasing whole individuals :)
      }

      # Update the pop size vector
      popSizeVector_withTadpoles <- dapva::makePopSizeVector(
        pop_class_names = paste(resultsTracking_popSize_females$pop, resultsTracking_popSize_females$class),
        num_indiv_per_pop_class =  as.numeric(resultsTracking_popSize_females[,paste(j)]),
        sex = "female")
      
      ######### See how many tadpoles survive to be yoy #########
      tapoles_survive_to_yoy <- survivalTadpoles(round(popSizeVector_withTadpoles), 
                                                 survivalMatrix,
                                                 demographic_stochasticity = TRUE)
      yoy_rows <- which(resultsTracking_popSize_females$class == "yoy")
      resultsTracking_popSize_females[yoy_rows, paste(j)] <- tapoles_survive_to_yoy[grepl("yoy" , rownames(tapoles_survive_to_yoy))]
      
      ######### And now those yoy disperse  #########

      dispersal_results <- dapva4nlf::dispersalTracking(resultsTracking_popSize_females, yoy_rows, i, j,
                                             wetlands,
                                             wetland_distances_km,
                                             parameterByIterTracking,
                                             allow_outside = dispersal_allowed_outside)
      
      # for each wetland, remove the # that left and then add them to the wetland they went to
      for(z in 1:length(yoy_rows)){
        wetland <- resultsTracking_popSize_females$pop[yoy_rows[z]]
        n_left <- sum(dispersal_results[paste(wetland),]) # rowsum for this wetland
        n_arrived <- sum(dispersal_results[,paste(wetland)]) # colsum for this wetland
        resultsTracking_popSize_females[yoy_rows[z], paste(j)] <- resultsTracking_popSize_females[yoy_rows[z], paste(j)] - n_left + n_arrived
      }
      
      
      ######### Then apply carrying capacity #########
      carrying_capacity <- as.numeric(parameterByIterTracking[i, paste0("carrying_capacity_BSCWMA")])
      
      yoy_rows <- which(resultsTracking_popSize_females$class == "yoy")
      juv_rows <- which(resultsTracking_popSize_females$class == "juv")
      A2_rows <- which(resultsTracking_popSize_females$class == "A2")
      A3_rows <- which(resultsTracking_popSize_females$class == "A3")
      A4plus_rows <- which(resultsTracking_popSize_females$class == "A4plus")
      # outside_rows <- which(resultsTracking_popSize_females$pop == "outside")
      # Note: not tracking numbers outside anyways so don't need to exclude here
      
      terrestrial_rows <- c(yoy_rows, juv_rows, A2_rows, A3_rows, A4plus_rows)
      terrestrial_pop_size_each_stage_wetland <- resultsTracking_popSize_females[terrestrial_rows, paste(j)]
      terrestrial_pop_size <- sum(terrestrial_pop_size_each_stage_wetland)
      
      above_K <- "no" # initalize
      if(terrestrial_pop_size > carrying_capacity){
        above_K <- "yes"
      }
      
      if(above_K == "yes"){
        # then reduce all terrestrial age classes proportionally 
        prop_of_total_each_stage <- terrestrial_pop_size_each_stage_wetland/terrestrial_pop_size
        new_pop_terrestrial_each_stage_wetland <- prop_of_total_each_stage*carrying_capacity
        resultsTracking_popSize_females[terrestrial_rows, paste(j)] <- round(new_pop_terrestrial_each_stage_wetland) # round so don't have partial individuals
      }

      ######### Record results and prep for next round #########
      # Nothing to do here I don't think as it is coded above

    }
    
  # Rprof(NULL)    ## Turn off the profiler
  # summaryRprof()
  # end.time <- Sys.time()
  # end.time - start.time
    
  } # end the annual loop

  # Return the results
  return(resultsTracking_popSize_females)
} # end runAnnualLoop function
