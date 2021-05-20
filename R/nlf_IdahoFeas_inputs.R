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
  
  repro_prop_active_mean <- list(
    c(input = "p_females_lay_eggs_mean_A2", type = "reproduction", 
      lcl = 0.1, best_guess = 0.75, ucl = 0.9, confidence = 90,
      lower_bound = 0.01, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "Lea and Rebecca on April 26, 2021", comments = "Absolute min of 0 is that in captivity don't ever get A2s breeding (at least in Vancouver aquariam, I guess we'll see with Edmonton Valley zoo); Hine et al. 1981 found that 23% of A2 females did not lay eggs in Wisconsin, as you get further north things take longer to grow but Wisconson should be pretty similar to Idaho; at the low end if the previous year was tough and the A1s didn't grow fully to full A2, high end not much higher than 77"),
    c(input = "p_females_lay_eggs_mean_A3", type = "reproduction", 
      lcl = 0.5, best_guess = 0.9, ucl = 0.95, confidence = 90,
      lower_bound = 0.05, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "Lea and Rebecca on April 26, 2021", comments = "Most of the older females lay eggs" ),
    c(input = "p_females_lay_eggs_mean_A4plus", type = "reproduction", 
      lcl = 0.5, best_guess = 0.9, ucl = 0.95, confidence = 90,
      lower_bound = 0.05, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", source = "Lea and Rebecca on April 26, 2021", comments = "Most of the older females lay eggs" )
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
      lcl = 750, best_guess = 1729, ucl = 2750, confidence = 95,
      lower_bound = 300, upper_bound = 3000,
      best_guess_type = "median", management_alternative = "status_quo", source = "Lea and Rebecca on April 26, 2021", comments = "Reading off the graph in Corn and Liveo 1989 - study from Colorado and Wyoming"),
    c(input = "num_eggs_per_active_female_mean_A3", type = "reproduction", 
      lcl = 1850, best_guess = 3418, ucl = 4733, confidence = 95,
      lower_bound = 1000, upper_bound = 5500,
      best_guess_type = "median", management_alternative = "status_quo", source = "Lea and Rebecca on April 26, 2021", comments = "Reading off the graph in Corn and Liveo 1989 - study from Colorado and Wyoming"),
    c(input = "num_eggs_per_active_female_mean_A4plus", type = "reproduction", 
      lcl = 2500, best_guess = 4733, ucl = 6700, confidence = 95,
      lower_bound = 1700, upper_bound = 7500,
      best_guess_type = "median", management_alternative = "status_quo", source = "Lea and Rebecca on April 26, 2021", comments = "Reading off the graph in Corn and Liveo 1989 - study from Colorado and Wyoming")
  )
  
  #---- Survival: mean, no threats.  -------------
  
  survival_rate_mean_no_threats <- list(
    c(input = "s_mean_eggs_no_threats", type = "survival", 
      lcl = 0.12, best_guess = 0.75, ucl = 0.9, confidence = 95,
      lower_bound = 0.05, upper_bound = 0.95,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "survival from eggs to tadpoles"),
    
    c(input = "s_mean_tadpoles_no_threats", type = "survival", 
      lcl = 0.02, best_guess = 0.04, ucl = 0.1, confidence = 80,
      lower_bound = 0.01, upper_bound = 0.25,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "survival from tadpoles to yoy"),
    
    c(input = "s_mean_yoy_no_threats", type = "survival", 
      lcl = 0.05, best_guess = 0.1, ucl = 0.31, confidence = 80,
      lower_bound = 0.01, upper_bound = 0.5,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "annual survival from yoy to juv (one year old adult), includes overwinter survival"),
    
    c(input = "s_mean_juv_no_threats", type = "survival", 
      lcl = 0.2, best_guess = 0.4, ucl = 0.6, confidence = 80,
      lower_bound = 0.075, upper_bound = 0.9,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "annual survival from juv (one year old adult) to adult (2 years), includes overwinter survival"),
    
    c(input = "s_mean_adult_no_threats", type = "survival", 
      lcl = 0.36, best_guess = 0.6, ucl = 0.7, confidence = 80,
      lower_bound = 0.1, upper_bound = 0.9,
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
      lcl = 1, best_guess = 7.5, ucl = 50, confidence = 75,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca on April 26, 2021", comments = "% reduction in survival due to bullfrog presence; Basically no info in the literature on this, bullfrogs do breed later than NLF so they might not be hanging out there at the same time, could be some from the previous year, bet guess is between 5 and 10%"),

    c(input = "s_pct_reduced_tadpoles_bullfrogs", type = "survival", 
      lcl = 15, best_guess = 35, ucl = 50, confidence = 70,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "% reduction in survival due to bullfrog presence; Redlegged frog tadpoles survival was reduced from 40% in ponds without bullfrogs to 5% with bullfrogs (35% preduction), another one said 48% reduction of yellow legged frogs in presence of bullfrogs; redlegged frogs are similar size and yellow legged are smaller to leopard frogs; another redlegged frog paper had a 15% reduction of tadpoles"),
    
    c(input = "s_pct_reduced_yoy_bullfrogs", type = "survival", 
      lcl = 12, best_guess = 22, ucl = 35, confidence = 70,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "% reduction in survival due to bullfrog presence"),

    c(input = "s_pct_reduced_juvenile_bullfrogs", type = "survival", 
      lcl = 10, best_guess = 20, ucl = 30, confidence = 70,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "% reduction in survival due to bullfrog presence; Slightly bigger than yoy so harder to eat; reduction in survival for other species 0.17 to 0.27"),
    
    c(input = "s_pct_reduced_adult_bullfrogs", type = "survival", 
      lcl = 5, best_guess = 10, ucl = 15, confidence = 70,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "% reduction in survival due to bullfrog presence; Half of the rate for juveniles given that adults are bigger")
  )
  
  #---- Survival: % reduction due to chytrid.  -------------
  
  survival_pctReduction_chytrid <- list(
    c(input = "s_pct_reduced_eggs_chytrid", type = "survival",
      lcl = 0, ucl = 0, confidence = 100,
      best_guess_type = "N/A", management_alternative = "status_quo", 
      source = "Lea and Rebecca on April 26, 2021", comments = "Shouldn't affect the eggs"),
    
    c(input = "s_pct_reduced_tadpoles_chytrid", type = "survival", 
      lcl = 0, ucl = 0, confidence = 100,
      best_guess_type = "N/A", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "Can affect the mouthparts of tadpoles but shouldn't affect the mortality; literature suggests mortality was post-metamorphisis (Marantelli et al. 2004)"),
    
    c(input = "s_pct_reduced_yoy_chytrid", type = "survival", 
      lcl = 1, best_guess = 10, ucl = 20, confidence = 60,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "In Brisco, the worst they saw was 30% of metamorphis had evidence of it (but did not necessarily die from it; literature suggests that mortalities of chytrid may be restricted to metamorphis (and juv, but overweintering?) (see one note page on Chytrid for references); For absolute max - In a lab settiung shown that it can wipe them out entirely, some other examples in Arizona (word of mouth) where it was causing their sites to fail"),
    
    c(input = "s_pct_reduced_juvenile_chytrid", type = "survival", 
      lcl = 1, best_guess = 8, ucl = 20, confidence = 60,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "Literature suggests that chytrid mortality in adults is generally low (see SDM one note page on chytrid)"),
    
    c(input = "s_pct_reduced_adult_chytrid", type = "survival", 
      lcl = 1, best_guess = 8, ucl = 20, confidence = 60,
      lower_bound = 0, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "Literature suggests that chytrid mortality in adults is generally low (see SDM one note page on chytrid)")
    )
  
  #---- Survival: % reduction due to roads.  -------------
  
  # Only affects terrestrial life stages, so yoy up
  survival_pctReduction_roads <- list(
    c(input = "s_pct_reduced_eggs_roads", type = "survival", 
      lcl = 0, ucl = 0, confidence = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca on April 26, 2021", comments = "These life stages aren’t on roads :)"),
    
    c(input = "s_pct_reduced_tadpoles_roads", type = "survival", 
      lcl = 0, ucl = 0, confidence = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca on April 26, 2021", comments = "These life stages aren’t on roads :)"),
    
    c(input = "s_pct_reduced_yoy_roads", type = "survival", 
      lcl = 8, best_guess = 12, ucl = 28, confidence = 80,
      lower_bound = 0, upper_bound = 50,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "Based on number of road mortalities in Creston; same for all mobile life stages; can't imagine would wack more than half of the pop"),
    
    c(input = "s_pct_reduced_juvenile_roads", type = "survival", 
      lcl = 8, best_guess = 12, ucl = 28, confidence = 80,
      lower_bound = 0, upper_bound = 50,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "Based on number of road mortalities in Creston; same for all mobile life stages; can't imagine would wack more than half of the pop"),
    
    c(input = "s_pct_reduced_adult_roads", type = "survival", 
      lcl = 8, best_guess = 12, ucl = 28, confidence = 80,
      lower_bound = 0, upper_bound = 50,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "Based on number of road mortalities in Creston; same for all mobile life stages; can't imagine would wack more than half of the pop")
    )
  
  #---- Survival: % reduction due to drawdown.  -------------

  # Only affects tadpoles since they can't move to get out of the wetland as it dries up;
  # drawdowns start after the eggs have hatched
  
  
  survival_pctReduction_drawdowns <- list(

    c(input = "s_pct_reduced_tadpoles_drawdownPartial", type = "survival", 
      lcl = 5, best_guess = 20, ucl = 35, confidence = 70,
      lower_bound = 0, upper_bound = 75,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "If complete drawdown happens, all die. But if just a partial, good chance that most survive. These are pretty big drawdowns so even with some water left. Ome will proibably get stuck behing the vegetation or get stranded as the water is drawing down though. Depends on how fast the water leaves the wetland, fast water means that more could die."),
    
    c(input = "s_pct_reduced_tadpoles_drawdownComplete", type = "survival", 
      lcl = 100, ucl = 100, confidence = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "If complete drawdown happens, all die. "),
    
    c(input = "drawdown_completeVSpartial_freq", type = "survival", 
      lcl = 50, best_guess = 80, ucl = 90, confidence = 70,
      lower_bound = 20, upper_bound = 100,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 26, 2021", comments = "Reasons for low: for the deeper wetlands, wouldn't need to do a complete drawdown for the cattail controls; Trying for the complete every year so that is more likely (80/20)")
    
    )
  
  #---- Human management: probability that bullfrog management will be effective.  -------------
  
  prob_bullfrog_mgmt_effective <- list(
    c(input = "bullfrogMgmt_effective", type = "human_management", 
      best_guess = 0.8, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca, April 23, 2021", comments = "8/10 chance that bullfrog managmenet efforts will keep bullfrog #s low enough that they will not affect NLF survival rates")
    )
  
  #---- Human management: probability of an early drawdown.  -------------
  
  prob_drawdown_beforeMidJuly <- list(
    c(input = "drawdown_beforeMidJuly", type = "human_management", 
      best_guess = 0.2, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Casey McCormack on April 29, 2021", comments = "1/5 chance best guess, general vibe from the managers is they would want to and try to postpone it but there may be unforseen circumstances that make it difficult.")
  )
  
  
  #---- Ephemeral wetlands: frequency of dry years.  -------------
  
  ephemeral_freq_dry <- list(
    
    c(input = "ephemeral_freq_dry", type = "epehmeral_wetlands", 
      lcl = 1/5,
      best_guess = 1/3,
      ucl = 1/2, confidence = 75,
      lower_bound = 0, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Casey McCormack on April 29, 2021", comments = "Low: one out of every other year is possible, especially 50 years out, in big trouble for other reasons too if more than that!; High - one out of every 5 years; best guess - stick ")
  )

  #---- Carrying Capacity for the BSCWMA.  -------------
  
  carrying_capacity <- list(
    c(input = "carrying_capacity_BSCWMA", type = "carrying capacity", 
      lcl = 500000, best_guess = 2000000, ucl = 5000000, confidence = 80,
      lower_bound = 5000, upper_bound = 11000000,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca on April 26, 2021", comments = "These numbers were guided by the available literature above; in general we don't think the populations will ever get this big!! Absolute min, decinitly can support some!")
  )
  
  #---- Proportion of carrying capacity to start an existing population at.  -------------
  # OLD APPROACH
  # prop_K_starting_pop <- list(
  #   c(input = "prop_K_starting_pop_exisiting_pop", type = "population size for existing population", 
  #     lcl = 0.02, best_guess = 0.05, ucl = 0.1, confidence = 80,
  #     lower_bound = 0.01, upper_bound = 0.5,
  #     best_guess_type = "median", management_alternative = "status_quo", 
  #     source = "LK Placeholder", comments = "Exploring results if started with an existing population")
  # )
  # 
  #---- Terrestrial population size to start an existing population at.  -------------
  
  starting_pop_terr_fem <- list(
    c(input = "starting_pop_terr_fem", type = " starting terrestrial population size if testing existing population", 
      lcl = 1000, best_guess = 100000, ucl = 200000, confidence = 80,
      lower_bound = 100, upper_bound = 2000000,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "LK Placeholder", comments = "Exploring results if started with an existing population; low is sort of from Travis and Karens paper, where they found 800 metamorphs one year and if we apply the 0.94 metamorphs it is rougly 850 individuals, use 1000 as nice round number, high i 10% of best guess K, best guess is 5% of best guess K, abs max is best guess K")
  )
  
  
  #---- Quasi-extinction threshold.  -------------
  
  quasi_extinction_threshold <- list(
    
    c(input = "quasi_extinction_threshold", type = "quasi-extinction threshold", 
      lcl = 0, ucl = 0, confidence = 100,
      best_guess_type = "n/a, uniform distribution", management_alternative = "status_quo", 
      source = "Lea and Rebecca on April 23, 2021", comments = "decided not to use a quasi-extinction threshold for now, instead model actual extinction (i.e. quasi extinction threshold of 0)")
  )
  
  #---- Inter-wetland vital rate correlation.  -------------
  
  # The survival rates for eggs and tadpoles will be 100% correlated.
  # The survival rates for you to adult will be 100% correlated.
  # No correlation between survival and reproduction.
  # These 100% and 0% correlations will be achieved by either using the same or different EV percentile draw
  # However, the wetlands themselves may not have exactly the same situation and should vary from each other year to year.
  # That is what this correlation range is for
  # Imagine this as a degree of similarity between wetlands
  # correlation of 1 is that the conditions in each of the wetlands are the same
  # correlation of 0 is that conditions in each of the wetlands are not related
  

  # Survival for eggs and tadpoles correlated 100% (i.e. use the same percentiles)
  # Survival for the other life stages correlated (but not correlated with eggs/tadpoles)  (i.e. use the same percentiles)
  # No correlation between survival and reproduction
  # Wetland quality (which is partially correlated) will affect the egg/tadpole stage but not the terrestrial life stages or reproduction since that depends on 
  # female body condition coming out of winter. 
  # Re partially correlated EV for the egg/tadpole stage, no reason to think cells 3 and 4 will be any different so use the same there.
  # Cell 7 may be different because closer to agriculture. Ephemeral wetlands is a different type of system so definitly have the potential to be different.
  
  # Current approach re ephemeral wetlands is to have a different range on correlation inputs for the alternatives where the ephemeral wetlands have been restored
  
  
  wetland_vitalrate_correlations <- list(
    c(input = "wetland_eggTadSurv_TempCor_noEph", type = "wetland_vitalrate_EVcorrelations", 
      lcl = 0.4, best_guess = 0.7, ucl = 0.9, confidence = 70,
      lower_bound = 0.2, upper_bound = 1,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "Cells 3 and 4 are lumped here so it is the difference between them and Cell 7 in the south, not sure about how agriculture will affect Cell 7, other than that they will all be pretty similar. The south wetland is 200m away from an agriculture field. Depends on what pesticide they are using and how they are applying them. HIghly dependent on the pesticide application method. If use aerial spreaders that can really drift, then that could decimate the quality in that south wetland.")
    # c(input = "wetland_eggTadSurv_TempCor_wEph", type = "wetland_vitalrate_EVcorrelations", 
    #   lcl = 0.2, best_guess = 0.6, ucl = 0.8, confidence = 90,
    #   lower_bound = 0.1, upper_bound = 0.9,
    #   best_guess_type = "median", management_alternative = "status_quo", 
    #   source = "Lea and Rebecca on May 5, 2021", comments = "Here it is really the difference with the epehemral wetlands driving it.")
  )
  
  #---- Dispersal inputs.  -------------
  dispersal_inputs <- list(
    c(input = "p_yoy_disperse", type = "dispersal", 
      lcl = 0.05, best_guess = 0.11, ucl = 0.3, confidence = 90,
      lower_bound = 0, upper_bound = 0.4,
      best_guess_type = "median", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "At the low end, prob not too low because seems reasonable that they would move around between wetlands in the WMA. Go with the 11% from the Seburn 1997 paper as the best guess. Lower the low end to a bit lower than the Cayela paper.."),
    
    c(input = "dispersal_CSF_vs_MoreGoShort", type = "dispersal", 
      best_guess = 0.4, # probability of CSF, prob of MoreGoSHort is 1-0.4 = 0.6
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "CSF is short for Columbia spotted frogs, based on the Funk et al. 2005. We think it is more likely that they will go shorter distances than longer ones."),
    
    c(input = "dispersal_CSFmodel_lessEqual1km", type = "dispersal", 
      best_guess = 0.44, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "CSF is short for Columbia spotted frogs, based on the Funk et al. 2005."),
    c(input = "dispersal_CSFmodel_greater1kmlessequal2km", type = "dispersal", 
      best_guess = 0.2, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "CSF is short for Columbia spotted frogs, based on the Funk et al. 2005."),
    c(input = "dispersal_CSFmodel_greater2km", type = "dispersal", 
      best_guess = 0.36, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "CSF is short for Columbia spotted frogs, based on the Funk et al. 2005."),
    
    c(input = "dispersal_MoreGoShortmodel_lessEqual1km", type = "dispersal", 
      best_guess = 0.8, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "We think it is more likely that they will go shorter distances than longer ones."),
    c(input = "dispersal_MoreGoShortmodel_greater1kmlessequal2km", type = "dispersal", 
      best_guess = 0.12, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "We think it is more likely that they will go shorter distances than longer ones."),
    c(input = "dispersal_MoreGoShortmodel_greater2km", type = "dispersal", 
      best_guess = 0.08, 
      best_guess_type = "probability", management_alternative = "status_quo", 
      source = "Lea and Rebecca on May 5, 2021", comments = "We think it is more likely that they will go shorter distances than longer ones.")
    
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
    survival_pctReduction_roads,
    survival_pctReduction_chytrid,
    survival_pctReduction_drawdowns,
    prob_bullfrog_mgmt_effective,
    prob_drawdown_beforeMidJuly,
    ephemeral_freq_dry,
    carrying_capacity,
    quasi_extinction_threshold,
    wetland_vitalrate_correlations,
    dispersal_inputs,
    starting_pop_terr_fem
  ))
  

  
  #---- Make the wetland distance matrix for dispersal.  -------------
  wetland_distances_km <- as.data.frame(matrix(nrow = 5, ncol = 5))
  colnames(wetland_distances_km) <- c("cell7", "cell3", "cell4", "outside", "ephemeral_wetlands")
  rownames(wetland_distances_km) <- c("cell7", "cell3", "cell4", "outside", "ephemeral_wetlands")
  wetland_distances_km[1,] <- c(0, 2050/1000, 1230/1000, 150/1000, 350/1000)
  wetland_distances_km[2,] <- c(2050/1000, 0, 300/1000, 260/1000, 3170/1000)
  wetland_distances_km[3,] <- c(1230/1000, 300/1000, 0, 400/1000, 2370/1000)
  wetland_distances_km[4,] <- c(150/1000, 260/1000, 400/1000, 0, 400/1000)
  wetland_distances_km[5,] <- c(350/1000, 3170/1000, 2370/1000, 400/1000, 0)
  
  #---- Return the results.  -------------

  # Address warning in devtools::check() (i.e. RMD check) re 'no visible binding for global variable'.
  # dat_distance_btwn_colonies_m <- NULL




  # return(list(inputs, dispersal_edge_list, dispersal_tracking))
  return(list(inputs, wetland_distances_km))
  
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
#'  
#' @param base_case TRUE or FALSE if you want all of the parameters just to reflect 
#' the best guess (for for uniform distributions, half way between the low and 
#' upper bounds). Defaults to FALSE to allow for probabalistic analysis. TRUE 
#' can be useful for model troubleshooting or to illustrate deterministic results.
#' 
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
selectNLFIdahoParameterByIterTracking <- function(inputs, base_case = FALSE) {

  # Initialize an empty tracking sheet
  parameterByIterTracking_empty <- dapva::makeParameterByIterTracking(inputsDF = inputs, n_iter = 1) # just one at a time now that it is inside a foreach loop

  i <- 1 # now that doing it in a foreach loop, each time just want it to fill in the one col and then bind them all together at the end
  parameterByIterTracking <- parameterByIterTracking_empty # initialize with the empty one each time for clarity (perhaps not necessary)
  # rownames(parameterByIterTracking) <- m # record which iteration it is with the col number

  ######### Select the reproduction parameters for this iteration - proportion of repro active females. #########
  
  if(base_case == FALSE){
    # Choose and record the reproduction parameters for this iteration - mean proportion of females who lay eggs
    parameterByIterTracking[i, "p_females_lay_eggs_mean_A2"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A2", inputsDF = inputs)
    parameterByIterTracking[i, "p_females_lay_eggs_mean_A3"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A3", inputsDF = inputs)
    parameterByIterTracking[i, "p_females_lay_eggs_mean_A4plus"] <- dapva::selectParamMetalogDistribution(input_name = "p_females_lay_eggs_mean_A4plus", inputsDF = inputs)
    
    # Choose and record the reproduction parameters for this iteration - sd proportion of females who lay eggs
    # UPDATE: use just one SD for all age classes to reflect level of temporal variation across all age classes
    # Otherwise get some unintended consequences where in some years could be much better repro for the younger ones, which is counterintiutive
    parameterByIterTracking[i, "p_females_lay_eggs_sd_A2"] <- dapva::selectParamUniformDistribution(input_name = "p_females_lay_eggs_sd_A2", inputsDF = inputs)
    parameterByIterTracking[i, "p_females_lay_eggs_sd_A3"] <- parameterByIterTracking[i, "p_females_lay_eggs_sd_A2"]
    parameterByIterTracking[i, "p_females_lay_eggs_sd_A4plus"] <- parameterByIterTracking[i, "p_females_lay_eggs_sd_A2"]
    
    # OLD - if each age class had a separate sd
    # parameterByIterTracking[i, "p_females_lay_eggs_sd_A2"] <- dapva::selectParamUniformDistribution(input_name = "p_females_lay_eggs_sd_A2", inputsDF = inputs)
    # parameterByIterTracking[i, "p_females_lay_eggs_sd_A3"] <- dapva::selectParamUniformDistribution(input_name = "p_females_lay_eggs_sd_A3", inputsDF = inputs)
    # parameterByIterTracking[i, "p_females_lay_eggs_sd_A4plus"] <- dapva::selectParamUniformDistribution(input_name = "p_females_lay_eggs_sd_A4plus", inputsDF = inputs)
    # 
  }
  if(base_case == TRUE){
    # Choose and record the reproduction parameters for this iteration - mean proportion of females who lay eggs
    parameterByIterTracking[i, "p_females_lay_eggs_mean_A2"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "p_females_lay_eggs_mean_A2")]))
    parameterByIterTracking[i, "p_females_lay_eggs_mean_A3"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "p_females_lay_eggs_mean_A3")]))
    parameterByIterTracking[i, "p_females_lay_eggs_mean_A4plus"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "p_females_lay_eggs_mean_A4plus")]))
    
    # Choose and record the reproduction parameters for this iteration - sd proportion of females who lay eggs
    # UPDATE: use just one SD for all age classes to reflect level of temporal variation across all age classes
    # Otherwise get some unintended consequences where in some years could be much better repro for the younger ones, which is counterintiutive
    # For base case, use halfway between lower and upper of uniform distribution
    parameterByIterTracking[i, "p_females_lay_eggs_sd_A2"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "p_females_lay_eggs_sd_A2")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "p_females_lay_eggs_sd_A2")])))/2
    parameterByIterTracking[i, "p_females_lay_eggs_sd_A3"] <- parameterByIterTracking[i, "p_females_lay_eggs_sd_A2"]
    parameterByIterTracking[i, "p_females_lay_eggs_sd_A4plus"] <- parameterByIterTracking[i, "p_females_lay_eggs_sd_A2"]
    
    
  }
  
  ######### Select the reproduction parameters for this iteration - number of offspring per female. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "num_eggs_per_active_female_mean_A2"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A2", inputsDF = inputs)
    parameterByIterTracking[i, "num_eggs_per_active_female_mean_A3"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A3", inputsDF = inputs)
    parameterByIterTracking[i, "num_eggs_per_active_female_mean_A4plus"] <- dapva::selectParamMetalogDistribution(input_name = "num_eggs_per_active_female_mean_A4plus", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "num_eggs_per_active_female_mean_A2"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "num_eggs_per_active_female_mean_A2")]))
    parameterByIterTracking[i, "num_eggs_per_active_female_mean_A3"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "num_eggs_per_active_female_mean_A3")]))
    parameterByIterTracking[i, "num_eggs_per_active_female_mean_A4plus"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "num_eggs_per_active_female_mean_A4plus")]))
  }
  ######### Select the survival parameters for this iteration - mean survival rates no threats. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "s_mean_eggs_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_eggs_no_threats", inputsDF = inputs)
    parameterByIterTracking[i, "s_mean_tadpoles_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_tadpoles_no_threats", inputsDF = inputs)
    parameterByIterTracking[i, "s_mean_yoy_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_yoy_no_threats", inputsDF = inputs)
    parameterByIterTracking[i, "s_mean_juv_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_juv_no_threats", inputsDF = inputs)
    parameterByIterTracking[i, "s_mean_adult_no_threats"] <- dapva::selectParamMetalogDistribution(input_name = "s_mean_adult_no_threats", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "s_mean_eggs_no_threats"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_eggs_no_threats")]))
    parameterByIterTracking[i, "s_mean_tadpoles_no_threats"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_tadpoles_no_threats")]))
    parameterByIterTracking[i, "s_mean_yoy_no_threats"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_yoy_no_threats")]))
    parameterByIterTracking[i, "s_mean_juv_no_threats"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_juv_no_threats")]))
    parameterByIterTracking[i, "s_mean_adult_no_threats"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_adult_no_threats")]))
  }
  
  ######### Select the survival parameters for this iteration - temporal variance in survival rates no threats. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "s_sd_eggs_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_eggs_no_threats", inputsDF = inputs)
    parameterByIterTracking[i, "s_sd_tadpoles_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_tadpoles_no_threats", inputsDF = inputs)
    parameterByIterTracking[i, "s_sd_yoy_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_yoy_no_threats", inputsDF = inputs)
    parameterByIterTracking[i, "s_sd_juv_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_juv_no_threats", inputsDF = inputs)
    parameterByIterTracking[i, "s_sd_adult_no_threats"] <- dapva::selectParamUniformDistribution(input_name = "s_sd_adult_no_threats", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "s_sd_eggs_no_threats"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_sd_eggs_no_threats")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_sd_eggs_no_threats")])))/2
    parameterByIterTracking[i, "s_sd_tadpoles_no_threats"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_sd_tadpoles_no_threats")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_sd_tadpoles_no_threats")])))/2
    parameterByIterTracking[i, "s_sd_yoy_no_threats"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_sd_yoy_no_threats")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_sd_yoy_no_threats")])))/2
    parameterByIterTracking[i, "s_sd_juv_no_threats"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_sd_juv_no_threats")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_sd_juv_no_threats")])))/2
    parameterByIterTracking[i, "s_sd_adult_no_threats"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_sd_adult_no_threats")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_sd_adult_no_threats")])))/2
  }
  
  ######### Select the survival parameters for this iteration - % reduction in survival due to bullfrogs. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "s_pct_reduced_eggs_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_eggs_bullfrogs", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_tadpoles_bullfrogs", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_yoy_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_yoy_bullfrogs", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_juvenile_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_juvenile_bullfrogs", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_adult_bullfrogs"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_adult_bullfrogs", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "s_pct_reduced_eggs_bullfrogs"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_eggs_bullfrogs")]))
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_bullfrogs"] <-  as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_tadpoles_bullfrogs")]))
    parameterByIterTracking[i, "s_pct_reduced_yoy_bullfrogs"] <-  as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_yoy_bullfrogs")]))
    parameterByIterTracking[i, "s_pct_reduced_juvenile_bullfrogs"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_juvenile_bullfrogs")]))
    parameterByIterTracking[i, "s_pct_reduced_adult_bullfrogs"] <-  as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_adult_bullfrogs")]))
  }
  

  
  ######### Select the survival parameters for this iteration - % reduction in survival due to chytrid. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "s_pct_reduced_eggs_chytrid"] <- dapva::selectParamUniformDistribution(input_name = "s_pct_reduced_eggs_chytrid", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_chytrid"] <- dapva::selectParamUniformDistribution(input_name = "s_pct_reduced_tadpoles_chytrid", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_yoy_chytrid"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_yoy_chytrid", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_juvenile_chytrid"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_juvenile_chytrid", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_adult_chytrid"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_adult_chytrid", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "s_pct_reduced_eggs_chytrid"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_pct_reduced_eggs_chytrid")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_pct_reduced_eggs_chytrid")])))/2
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_chytrid"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_pct_reduced_tadpoles_chytrid")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_pct_reduced_tadpoles_chytrid")])))/2
    parameterByIterTracking[i, "s_pct_reduced_yoy_chytrid"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_yoy_chytrid")]))
    parameterByIterTracking[i, "s_pct_reduced_juvenile_chytrid"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_juvenile_chytrid")]))
    parameterByIterTracking[i, "s_pct_reduced_adult_chytrid"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_adult_chytrid")]))
  }
  
  ######### Select the survival parameters for this iteration - % reduction in survival due to roads. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "s_pct_reduced_eggs_roads"] <- dapva::selectParamUniformDistribution(input_name = "s_pct_reduced_eggs_roads", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_roads"] <- dapva::selectParamUniformDistribution(input_name = "s_pct_reduced_tadpoles_roads", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_yoy_roads"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_yoy_roads", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_juvenile_roads"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_juvenile_roads", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_adult_roads"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_adult_roads", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "s_pct_reduced_eggs_roads"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_pct_reduced_eggs_roads")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_pct_reduced_eggs_roads")])))/2
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_roads"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_pct_reduced_tadpoles_roads")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_pct_reduced_tadpoles_roads")])))/2
    parameterByIterTracking[i, "s_pct_reduced_yoy_roads"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_yoy_roads")]))
    parameterByIterTracking[i, "s_pct_reduced_juvenile_roads"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_juvenile_roads")]))
    parameterByIterTracking[i, "s_pct_reduced_adult_roads"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_adult_roads")]))
  }
  
  ######### Select the survival parameters for this iteration - % reduction in survival due to drawdown. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "drawdown_beforeMidJuly"] <- sample(c("yes", "no"),
                                                                 size = 1,
                                                                 prob = c(as.numeric(inputs$best_guess[which(inputs$input == "drawdown_beforeMidJuly")]),
                                                                          (1-as.numeric(inputs$best_guess[which(inputs$input == "drawdown_beforeMidJuly")]))
                                                                 ), replace = T)
  
    parameterByIterTracking[i, "drawdown_completeVSpartial_freq"] <- dapva::selectParamMetalogDistribution(input_name = "drawdown_completeVSpartial_freq", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_drawdownPartial"] <- dapva::selectParamMetalogDistribution(input_name = "s_pct_reduced_tadpoles_drawdownPartial", inputsDF = inputs)
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_drawdownComplete"] <- dapva::selectParamUniformDistribution(input_name = "s_pct_reduced_tadpoles_drawdownComplete", inputsDF = inputs)
  
  }
  
  if(base_case == TRUE){
    
    if(as.numeric(inputs$best_guess[which(inputs$input == "drawdown_beforeMidJuly")]) < 0.5)
    {parameterByIterTracking[i, "drawdown_beforeMidJuly"] <- "no"}
    if(as.numeric(inputs$best_guess[which(inputs$input == "drawdown_beforeMidJuly")]) >= 0.5)
    {parameterByIterTracking[i, "drawdown_beforeMidJuly"] <- "yes"}
      
    parameterByIterTracking[i, "drawdown_completeVSpartial_freq"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "drawdown_completeVSpartial_freq")]))
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_drawdownPartial"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_pct_reduced_tadpoles_drawdownPartial")]))
    parameterByIterTracking[i, "s_pct_reduced_tadpoles_drawdownComplete"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "s_pct_reduced_tadpoles_drawdownComplete")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "s_pct_reduced_tadpoles_drawdownComplete")])))/2
    
  }
  
  ######### Select the human management parameters for this iteration - bullfrog management effective or not. #########
  if(base_case == FALSE){
    
  parameterByIterTracking[i, "bullfrogMgmt_effective"] <- sample(c("yes", "no"),
                                                          size = 1,
                                                          prob = c(as.numeric(inputs$best_guess[which(inputs$input == "bullfrogMgmt_effective")]),
                                                                   (1-as.numeric(inputs$best_guess[which(inputs$input == "bullfrogMgmt_effective")]))
                                                                   ), replace = T)
  }
  
  if(base_case == TRUE){
    
    if(as.numeric(inputs$best_guess[which(inputs$input == "bullfrogMgmt_effective")]) < 0.5)
    {parameterByIterTracking[i, "bullfrogMgmt_effective"] <- "no"}
    if(as.numeric(inputs$best_guess[which(inputs$input == "bullfrogMgmt_effective")]) >= 0.5)
    {parameterByIterTracking[i, "bullfrogMgmt_effective"] <- "yes"}
    
  }
  
  ######### Select the parameters for this iteration - freq ephemeral wetlands dry. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "ephemeral_freq_dry"] <- dapva::selectParamMetalogDistribution(input_name = "ephemeral_freq_dry", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "ephemeral_freq_dry"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "ephemeral_freq_dry")]))
  }
  
  ######### Select the parameters for this iteration - carrying capacity (cap). #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "carrying_capacity_BSCWMA"] <- dapva::selectParamMetalogDistribution(input_name = "carrying_capacity_BSCWMA", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "carrying_capacity_BSCWMA"] <-  as.numeric(as.character(inputs$best_guess[which(inputs$input == "carrying_capacity_BSCWMA")]))
  }
  
  ######### Select the parameters for this iteration - population size of a hypothetical existing population. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "starting_pop_terr_fem"] <- dapva::selectParamMetalogDistribution(input_name = "starting_pop_terr_fem", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "starting_pop_terr_fem"] <-  as.numeric(as.character(inputs$best_guess[which(inputs$input == "starting_pop_terr_fem")]))
  }
  
  
  
  
  
  ######### Select the parameters for this iteration - correlation between wetlands for vital rates. #########
  if(base_case == FALSE){
   parameterByIterTracking[i, "wetland_eggTadSurv_TempCor_noEph"] <- dapva::selectParamMetalogDistribution(input_name = "wetland_eggTadSurv_TempCor_noEph", inputsDF = inputs)
   # parameterByIterTracking[i, "wetland_eggTadSurv_TempCor_wEph"] <- dapva::selectParamMetalogDistribution(input_name = "wetland_eggTadSurv_TempCor_wEph", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "wetland_eggTadSurv_TempCor_noEph"] <-  as.numeric(as.character(inputs$best_guess[which(inputs$input == "wetland_eggTadSurv_TempCor_noEph")]))
    # parameterByIterTracking[i, "wetland_eggTadSurv_TempCor_wEph"] <-   as.numeric(as.character(inputs$best_guess[which(inputs$input == "wetland_eggTadSurv_TempCor_wEph")]))
  }
  
  ######### Select the parameters for this iteration - quasi extinction threshold. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "quasi_extinction_threshold"] <- dapva::selectParamUniformDistribution(input_name = "quasi_extinction_threshold", inputsDF = inputs)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "quasi_extinction_threshold"] <- (as.numeric(as.character(inputs$lcl[which(inputs$input == "quasi_extinction_threshold")])) + as.numeric(as.character(inputs$ucl[which(inputs$input == "quasi_extinction_threshold")])))/2
  }
  
  ######### Select the parameters for this iteration - dispersal. #########
  if(base_case == FALSE){
    parameterByIterTracking[i, "p_yoy_disperse"] <- dapva::selectParamMetalogDistribution(input_name = "p_yoy_disperse", inputsDF = inputs)
  
    parameterByIterTracking[i, "dispersal_CSF_vs_MoreGoShort"] <- sample(c("CSF", "MoreGoShort"),
                                                                 size = 1,
                                                                 prob = c(as.numeric(inputs$best_guess[which(inputs$input == "dispersal_CSF_vs_MoreGoShort")]),
                                                                          (1-as.numeric(inputs$best_guess[which(inputs$input == "dispersal_CSF_vs_MoreGoShort")]))
                                                                 ), replace = T)
  }
  
  if(base_case == TRUE){
    parameterByIterTracking[i, "p_yoy_disperse"] <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "p_yoy_disperse")]))
    
    if(as.numeric(inputs$best_guess[which(inputs$input == "dispersal_CSF_vs_MoreGoShort")]) < 0.5)
    {parameterByIterTracking[i, "dispersal_CSF_vs_MoreGoShort"] <- "MoreGoShort"}
    if(as.numeric(inputs$best_guess[which(inputs$input == "dispersal_CSF_vs_MoreGoShort")]) >= 0.5)
    {parameterByIterTracking[i, "dispersal_CSF_vs_MoreGoShort"] <- "CSF"}
  }
  
  # No uncertainty in these but record for easy access through parameterByIterTracking
  
  parameterByIterTracking[i, "dispersal_CSFmodel_lessEqual1km"] <- as.numeric(inputs$best_guess[which(inputs$input == "dispersal_CSFmodel_lessEqual1km")])
  parameterByIterTracking[i, "dispersal_CSFmodel_greater1kmlessequal2km"] <- as.numeric(inputs$best_guess[which(inputs$input == "dispersal_CSFmodel_greater1kmlessequal2km")])
  parameterByIterTracking[i, "dispersal_CSFmodel_greater2km"] <- as.numeric(inputs$best_guess[which(inputs$input == "dispersal_CSFmodel_greater2km")])
  
  parameterByIterTracking[i, "dispersal_MoreGoShortmodel_lessEqual1km"] <- as.numeric(inputs$best_guess[which(inputs$input == "dispersal_MoreGoShortmodel_lessEqual1km")])
  parameterByIterTracking[i, "dispersal_MoreGoShortmodel_greater1kmlessequal2km"] <- as.numeric(inputs$best_guess[which(inputs$input == "dispersal_MoreGoShortmodel_greater1kmlessequal2km")])
  parameterByIterTracking[i, "dispersal_MoreGoShortmodel_greater2km"] <- as.numeric(inputs$best_guess[which(inputs$input == "dispersal_MoreGoShortmodel_greater2km")])
  
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

