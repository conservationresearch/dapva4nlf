# This script runs a decision-analytic population viability analysis for
# a Northern Leopard Frog feasibility analysis. 

# The goal statement for this feasibility analysis is: The Idaho Department of
# Fish and Game, Wildlife Bureau is trying to gain insight into the likelihood
# of success of reintroducing northern leopard frogs to achieve local recovery
# of the species in the Boundary Smith Creek Wildlife Management Area in
# northern Idaho given that they are currently extirpated from the region.

# Main coder: Laura Keating
# Modeling subcommittee: 
# April-June 2021

system.time({ # turn on the timer

#---- Clear the workspace. ----
rm(list = ls())
version <- "_v1test10_1200it" # insert short description to append to results to help identify

#---- Load libraries, and set the random seed.  -------------
## Import libraries
library(dapva) # this is our own internal library that houses functions used here
library(dapva4nlf) # this is our own internal library that houses functions used here
library(dplyr) # for summarise
library(foreach) # for %do% nd %dopar%; this might come as part of DoParallel which is loaded later, test this later by trying without
library(R.utils) # for withTimeout



# Set up for parallel computing - e.g. https://privefl.github.io/blog/a-guide-to-parallelism-in-r/
library(doParallel)  # see https://www.r-bloggers.com/2016/07/lets-be-faster-and-more-parallel-in-r-with-doparallel-package/
no_cores <- detectCores() - 1  # one less than max to be safe and prevent crashing
cl <- parallel::makeCluster(no_cores, type="FORK") # if on windows, try this instead: cl <- parallel::makeCluster(no_cores)
# cl <- parallel::makeCluster(no_cores)
doParallel::registerDoParallel(cl)

# Set the seed in a way that works for parallel computing (each core needs a separate random seed) (e.g. https://www.r-bloggers.com/2020/09/future-1-19-1-making-sure-proper-random-numbers-are-produced-in-parallel-processing/)
clusterSetRNGStream(cl, iseed = 29) # without parallel computing can just do set.seed(1234)

#---- Specify the alternatives to run.  -------------
alternatives_to_run <- dapva4nlf::dat_alternatives_to_run # some scenarios are preloaded in for easy calling

rows_to_run <- c(10) # note that can't call 1 but just 0s anyways; all the rest seem to run fine; 3 got stuck in batches of 2 but works with more batches

#---- Specify number of iterations and number of runs per iterations.  -------------
n_iter  <- 1200 # 2000# 500
flexible_convergence_iteration_on <- "yes" # 'yes' or 'no', generally choose yes unless you are running a tornado and want to specify a # of iter

max_n_runs_per_iter <- 1000 # flexible convergence is always on at the run level


#---- Start the scenario loop.  -------------
for(m in 1:length(rows_to_run)){ # loop through the different scenarios requested in the scenarios_to_run file
  row_to_run <- rows_to_run[m]
  print(paste0("Running alternative ", alternatives_to_run$alt_name_short[row_to_run]))
  alternative_details <- alternatives_to_run[row_to_run,]
  
#---- Get the inputs.  -------------

  inputs_all <- dapva4nlf::getNLFIdahoFeasinputs()
  inputs <- inputs_all[[1]]
  wetland_distances_km <- inputs_all[[2]]
  
#---- Choose the parameters for each iteration (i.e. parametric uncertainty). ----
  print("Selecting the parameters for each iteration in parallel.")
  parameterByIterTracking <- foreach::foreach(m=1:n_iter,  .combine=rbind,
                                              .packages='dapva4nlf'
                                               ) %dopar% {
                                                # ) %do% {

                                                print(paste("Choosing parameters for iteration # ", m))
                                                parameterByIterTracking <- dapva4nlf::selectNLFIdahoParameterByIterTracking(inputs)
                                                return(parameterByIterTracking)
                                                
                                               }
  
  # If running using just base case inputs (i.e. one iteration with best guess values),
  # then uncomment the lines below.
  # parameterByIterTracking_baseCase <- selectNLFIdahoParameterByIterTracking(inputs, base_case = TRUE)
  # parameterByIterTracking <-  parameterByIterTracking_baseCase
  # n_iter <- 1
  
  
  # Replace any iterations where bullfrog management is effective to NA for all the bullfrog threat related survival inputs.
  rows_bullfrogMgt_effective <- which(parameterByIterTracking$bullfrogMgmt_effective == "yes")
  parameterByIterTracking$s_pct_reduced_eggs_bullfrogs[rows_bullfrogMgt_effective] <- NA
  parameterByIterTracking$s_pct_reduced_tadpoles_bullfrogs[rows_bullfrogMgt_effective] <- NA
  parameterByIterTracking$s_pct_reduced_yoy_bullfrogs[rows_bullfrogMgt_effective] <- NA
  parameterByIterTracking$s_pct_reduced_juvenile_bullfrogs[rows_bullfrogMgt_effective] <- NA
  parameterByIterTracking$s_pct_reduced_adult_bullfrogs[rows_bullfrogMgt_effective] <- NA
  
  
  # Identify any survival inputs that don't have valid beta distribution shape paramaters 
  # and relplace the standard deviation with 10% of the mean (something small) to make it work.
  # NOTE: IF THIS WORKS, CAN TAKE OUT OF dapva::selectPercentileBetaDistribution in the annual loop.
  
  # MIGHT BE ABLE TO TAKE THIS OUTNOW THAT HAVE IMPROVED STANDRAD DEVIATION INPUTS, DOUBLE CHECK THAT
  # THIS ISN"T PULLING ANY OUT ANYMORE

  parameterByIterTracking_replace_invalid_beta_shapeParam <- function(parameterByIterTracking, mean, sd){
    
    test <- do.call("rbind",lapply(1:nrow(parameterByIterTracking), 
                                   function(x)dapva::estBetaParams(mean = parameterByIterTracking[x, paste(mean)], 
                                                                   sd = parameterByIterTracking[x, paste(sd)])
    )
    )
    
    rows_with_invalid_alpha <- which(test[,1] <=0)
    rows_with_invalid_beta <- which(test[,2] <=0)
    rows_with_invalid_shapeParam <- unique(c(rows_with_invalid_alpha, rows_with_invalid_beta))
    parameterByIterTracking_updated <- parameterByIterTracking # initalize
    if(length(rows_with_invalid_shapeParam)>0){
      print(paste("Replacing for ", length(rows_with_invalid_shapeParam), "iterations: sd with 10 pct of the mean"))
      parameterByIterTracking_updated[rows_with_invalid_shapeParam, paste(sd)] <- parameterByIterTracking[rows_with_invalid_shapeParam, paste(mean)]*0.1 
      
    }

    return(parameterByIterTracking_updated)
  }
  
  # Check and update reproduction if needed
  
  parameterByIterTracking <- parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking, 
                                                                                     mean = "p_females_lay_eggs_mean_A2", 
                                                                                     sd = "p_females_lay_eggs_sd_A2")
  
  parameterByIterTracking <- parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking, 
                                                                                     mean = "p_females_lay_eggs_mean_A3_A4plus", 
                                                                                     sd = "p_females_lay_eggs_sd_A3_A4plus")
  # 
  # parameterByIterTracking <- parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking, 
  #                                                                                    mean = "p_females_lay_eggs_mean_A4plus", 
  #                                                                                    sd = "p_females_lay_eggs_sd_A4plus")
  # Check and update survival if needed
  
  parameterByIterTracking <- parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking, 
                                                                                    mean = "s_mean_eggs_no_threats", 
                                                                                    sd = "s_sd_eggs_no_threats")

  parameterByIterTracking <- parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking, 
                                                                                     mean = "s_mean_tadpoles_no_threats", 
                                                                                     sd = "s_sd_tadpoles_no_threats")
  
  parameterByIterTracking <- parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking, 
                                                                                     mean = "s_mean_yoy_no_threats", 
                                                                                     sd = "s_sd_yoy_no_threats")
  
  parameterByIterTracking <- parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking, 
                                                                                     mean = "s_mean_juv_no_threats", 
                                                                                     sd = "s_sd_juv_no_threats")
  
  parameterByIterTracking <- parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking, 
                                                                                     mean = "s_mean_adult_no_threats", 
                                                                                     sd = "s_sd_adult_no_threats")
  
  
  
  
#---- Run the PVA. ----
  # Need to have run the foreach loop for parameterByIterTracking first
  print("Running the PVA with each iteration in parallel.")
  
  # set it up to run in batches of 100 so we can monitor progress, frustrated by the lack of ability to have a progress bar
  
  results_summary_all_iterations_overall_int  <- list() # initialize
  results_summary_all_iterations_by_pop_int  <- list() # initialize
  results_all_iterations  <- list() # initialize
  
  batch_size <-  100 # always at least one batch
  batches <- split(1:n_iter, ceiling(seq_along(1:n_iter)/batch_size ))
  
  
  # Inputs for convergence criteria at the iteration level
  convergence_band_halfwidth_iter <- 0.025 # between 0 and 1 since will base it on the probability objective metrics
  convergence_band_length_n_batches <- 1 # number of batches
  convergence_band_length_n_iter <- 50 # number of batches
  
  min_n_batches <- 2 # 
  convergence_tracking_persis_iter <- c(vector(), rep(NA, convergence_band_length_n_iter)) # initalize
  convergence_tracking_selfsustain_iter <- c(vector(), rep(NA, convergence_band_length_n_iter)) # initalize
  converged_for_persist_iter <- "no"
  converged_for_selfsustain_iter <- "no"

  for(batch in 1:length(batches)){
    print(paste("Running batch ", batch, "; i.e. iterations", first(batches[[batch]]), "to ", last(batches[[batch]]), " of ", n_iter))
    
    results_summary_all_iterations <- foreach::foreach(i=batches[[batch]],
                                                       .combine=rbind,
                                                       # .verbose = T, # print out which iterations have been completed
                                                       .errorhandling = c("remove"), # remove/skip if the result has an error
                                                       .packages=c('foreach', 
                                                                   'dapva',# need foreach in here as per https://stackoverflow.com/questions/21128122/function-do-not-found-on-win-platform-only-when-using-plyr-and-doparallel
                                                                   'dapva4nlf',
                                                                   'R.utils')) %dopar% {  # change 'dopar' to 'do' if don't want to do the parallel computing
           
                                                                     
                                                                     results_annual <- list() # initalize
                                                                     finish <- FALSE # initalize
                                                                     
                                                                     # Inputs for convergence criteria at the run level
                                                                     convergence_band_halfwidth <- 0.025 # between 0 and 1 since will base it on the probability objective metrics
                                                                     convergence_band_length <- 50 # number of run
                                                                     burnin <- 100 # min number of runs (should rename, not true burn in because we do not discard these)
                                                                     convergence_tracking_persis <- c(vector(), rep(NA, max_n_runs_per_iter)) # initalize
                                                                     convergence_tracking_selfsustain <- c(vector(), rep(NA, max_n_runs_per_iter)) # initalize
                                                                     converged_for_persist <- "no"
                                                                     converged_for_selfsustain <- "no"
                                                                     
                                                                     for(q in 1:max_n_runs_per_iter){# not in parallel here; in parallel at the iteration level
                                                                       
                                                                       # print("Test - checking if got back up here to start next iteration")
                                                                       
                                                                       # Specify a few more inputs for this iteration
                                                                       initial_year <- parameterByIterTracking$initial_year[i]
                                                                       yrs <- parameterByIterTracking$yrs[i]
                                                                       stage_classes <- c("eggs", "tadpoles", "yoy", "juv", "A2", "A3", "A4plus")
                                                                       
                                                                       # print("Test1")
                                                                       if(alternative_details$restore_ephemeralWetlands == "yes"){
                                                                         wetlands <- c("cell3", "cell4", "cell7", "ephemeral_wetlands", "outside")
                                                                       }
                                                                       if(alternative_details$restore_ephemeralWetlands == "no"){
                                                                         wetlands <- c("cell3", "cell4", "cell7", "outside")
                                                                       } 
                                                                       
                                                              
                                                                       #Select the EV percentiles for each year in this run.
 
                                                                       # Survival for eggs and tadpoles correlated 100% (i.e. use the same percentiles)
                                                                       # Survival for the other life stages correlated (but not correlated with eggs/tadpoles)  (i.e. use the same percentiles)
                                                                       # No correlation between survival and reproduction
                                                                       # Wetland quality (which is partially correlated) will affect the egg/tadpole stage but not the terrestrial life stages or reproduction since that depends on 
                                                                       # female body condition coming out of winter. 
                                                                       # Re partially correlated EV for the egg/tadpole stage, no reason to think cells 3 and 4 will be any different so use the same there.
                                                                       # Cell 7 may be different because closer to agriculture. 
                                                                       # Ephemeral wetlands is a different type of system so definitely have the potential to be different, treat as uncorrelated.
             
                                                                       # print("Test2")
                                                                       
                                                                       # Select correlated EVs for wetlands cells 3/4 and 7
                                                                       # print("Test2a")
                                                                       percentilesEV_survival_eggs_tad <- dapva::selectEVPercentilesNormal(input_names_w_EV = c("cells3and4", "cell7"),
                                                                                                                                           correlation = parameterByIterTracking$wetland_eggTadSurv_TempCor_noEph[i],
                                                                                                                                           n_years = yrs)
                                                                       # Separate out the EVs so that each wetland has its own col that can be called later
                                                                       percentilesEV_survival_eggs_tad$cell3 <- percentilesEV_survival_eggs_tad$cells3and4
                                                                       percentilesEV_survival_eggs_tad$cell4 <- percentilesEV_survival_eggs_tad$cells3and4
                                                                       percentilesEV_survival_eggs_tad <- percentilesEV_survival_eggs_tad[,c("cell3", "cell4", "cell7")]
                                                                       
                                                                       
                                                                       # Select EVs for ephemeral wetlands, will only be applied for alternatives w

                                                                       if(alternative_details$restore_ephemeralWetlands == "yes"){
                                                                         # print("Test2c")
                                                                         percentilesEV_survival_eggs_tad_ephemeral <- dapva::selectEVPercentilesNormal(input_names_w_EV = c("ephemeral_wetlands"),
                                                                                                                                             correlation = 1,
                                                                                                                                             n_years = yrs)
                                       
                                                                         percentilesEV_survival_eggs_tad <- cbind( percentilesEV_survival_eggs_tad, percentilesEV_survival_eggs_tad_ephemeral)
                                                                         
                                                                         }
                                                             
                                                                       # print("Test3")
                                                                       
                                                                       percentilesEV_survival_yoy_adult <- dapva::selectEVPercentilesNormal(input_names_w_EV = c("all_wetlands"),
                                                                                                                                    correlation = 1, n_years = parameterByIterTracking$yrs[i])
                                                                       
                                                                       # print("Test4")
                                                                       
                                                                       percentilesEV_reproduction <- dapva::selectEVPercentilesNormal(input_names_w_EV = c("all_wetlands"),
                                                                                                                              correlation = 1,
                                                                                                                              n_years = parameterByIterTracking$yrs[i])

                                                                       
                                                                       
                                                                       
                                                                                                                             
                                                                       # print("Test5")
                                                                       
                                                                       # Check if there is an existing pop in this alternative or not
                                                                       if(alternative_details$assume_existing_pop == "yes"){
                                                                         exisiting_pop <- TRUE
                                                                       }
                                                                       if(alternative_details$assume_existing_pop == "no"){
                                                                         exisiting_pop <- FALSE
                                                                       }
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       # Run the annual loop
                                                                       results_annual[[q]] <- dapva4nlf::runAnnualLoopNLFIdahoPVA(parameterByIterTracking, yrs, i, q,
                                                                                                                                  wetland_distances_km,
                                                                                                                                  initial_year, wetlands,stage_classes,
                                                                                                                                  percentilesEV_survival_eggs_tad,
                                                                                                                                  percentilesEV_survival_yoy_adult,
                                                                                                                                  percentilesEV_reproduction,
                                                                                                                                  alternative_details,
                                                                                                                                  exisiting_pop,
                                                                                                                                  dispersal_allowed_outside = parameterByIterTracking$dispersal_allowed_outside[i])

                                                                       # Then check to see what the probability metric is
                                                                       
                                                                       results_all_so_far <- plyr::rbind.fill(results_annual)
                                                                       
                                                                       check_if_enough_runs <- makeResultsSummaryOneIteration(results_all_so_far,
                                                                                                                              by_pop = 'no',
                                                                                                                              initial_year = parameterByIterTracking$initial_year[i],
                                                                                                                              yrs = parameterByIterTracking$yrs[i],
                                                                                                                              n_iter,
                                                                                                                              n_runs_per_iter = q,
                                                                                                                              alternative = paste0(alternative_details$alt_name_full),
                                                                                                                              prob_self_sustain = TRUE,
                                                                                                                              iteration_number = i)
                                                                       
                                                                       prob_of_persis_so_far <- check_if_enough_runs[which(check_if_enough_runs$metric == "probability of persistence"),
                                                                                                                     paste(parameterByIterTracking$initial_year[i] + parameterByIterTracking$yrs[i]-1)]
                                                                       
                                                                       
                                                                       prob_of_selfsustain_so_far <- check_if_enough_runs[which(check_if_enough_runs$metric == "probability of self-sustaining population"),
                                                                                                                          paste(parameterByIterTracking$initial_year[i] + parameterByIterTracking$yrs[i]-1)]
                                                                       
                                                                       
                                                                       convergence_tracking_persis[q] <- as.numeric(prob_of_persis_so_far)
                                                                       convergence_tracking_selfsustain[q] <- as.numeric(prob_of_selfsustain_so_far)
                                                                       
                                                                       # if(q >= burnin){ # for every iteration after the burn in + convergence band length 
                                                                         
                                                                         
                                                                         if(q >= (burnin + convergence_band_length)){
                                                                           
                                                                           CB_persis_upper_limit <- convergence_tracking_persis[q - convergence_band_length] + convergence_band_halfwidth
                                                                           CB_persis_lower_limit <- convergence_tracking_persis[q - convergence_band_length] - convergence_band_halfwidth
                                                                           
                                                                           CB_selfsustain_upper_limit <- convergence_tracking_selfsustain[q - convergence_band_length] + convergence_band_halfwidth
                                                                           CB_selfsustain_lower_limit <- convergence_tracking_selfsustain[q - convergence_band_length] - convergence_band_halfwidth
                                                                           
                                                                           # if within the convergence criteria for both metrics of interest, can stop
                                                                           if(sum(convergence_tracking_persis[(burnin+1):q] <= CB_persis_upper_limit) >= convergence_band_length & 
                                                                             sum(convergence_tracking_persis[(burnin+1):q] >= CB_persis_lower_limit) >= convergence_band_length){
                                                                             converged_for_persist <- "yes"
                                                                           }
                                                                           
                                                                           if(sum(convergence_tracking_selfsustain[(burnin+1):q] <= CB_selfsustain_upper_limit) >= convergence_band_length & 
                                                                              sum(convergence_tracking_selfsustain[(burnin+1):q] >= CB_selfsustain_lower_limit) >= convergence_band_length){
                                                                             converged_for_selfsustain  <- "yes"
                                                                           }
  
                                                                           if(converged_for_persist == "yes" & 
                                                                              converged_for_selfsustain == "yes") {
                                                                             finish <- TRUE
                                                                           }      
                                                                         }
                                                                       # }
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       # if(q == max_n_runs_per_iter*0.1){ # if we have run 10% of the max number of runs per iterations
                                                                       # 
                                                                       #    # Check to see if within 5% of either 0 or 1 since we noticed from covergence
                                                                       #    # plots that if it is at either of these two extremes, more iterations are typically not necessary
                                                                       # 
                                                                       #    results_all_so_far <- plyr::rbind.fill(results_annual)
                                                                       # 
                                                                       #    check_if_enough_runs <- makeResultsSummaryOneIteration(results_all_so_far,
                                                                       #                                                          by_pop = 'no',
                                                                       #                                                           initial_year = parameterByIterTracking$initial_year[i],
                                                                       #                                                           yrs = parameterByIterTracking$yrs[i],
                                                                       #                                                           n_iter,
                                                                       #                                                           n_runs_per_iter = q,
                                                                       #                                                           alternative = paste0(alternative_details$alt_name_full),
                                                                       #                                                           iteration_number = i)
                                                                       # 
                                                                       #    prob_of_persis_so_far <- check_if_enough_runs[which(check_if_enough_runs$metric == "probability of persistence"),
                                                                       #                                                  paste(parameterByIterTracking$initial_year[i] + parameterByIterTracking$yrs[i]-1)]
                                                                       # 
                                                                       #    # if with 5% of 0 or 1, then no need to do more runs
                                                                       #   if(prob_of_persis_so_far <= 0.05){finish <- TRUE}
                                                                       #    if(prob_of_persis_so_far >= 0.95){finish <- TRUE}
                                                                       #  }

                                                                        if(finish == TRUE){
                                                                          print(paste('Iteration', i, "stopped at", q, "runs"))
                                                                        break
                                                                        }
                                                                       
                                                                        # Return the results for this run
                                                                       # return(results_annual)
                                                                     
                                                                     }
                                                                     
                                                                     
                                                                      results_all_for_this_iteration <- plyr::rbind.fill(results_annual)
                                                                      
                                                               
                                                                      # Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
                                                                      results_all_for_this_iteration_fall <- results_all_for_this_iteration # initalize
                                                                      results_all_for_this_iteration_fall[which(results_all_for_this_iteration_fall$class == "eggs"),paste(1:yrs)] <- 0
                                                                      results_all_for_this_iteration_fall[which(results_all_for_this_iteration_fall$class == "tadpoles"),paste(1:yrs)] <- 0
                                                                      
                                                                     # Overall results
                                                                      results_summary_for_this_iteration_overall <- dapva::makeResultsSummaryOneIteration(results_all_for_this_iteration_fall,
                                                                                                                                                   by_pop = 'no',
                                                                                                                                                   initial_year = parameterByIterTracking$initial_year[i],
                                                                                                                                                   yrs = parameterByIterTracking$yrs[i],
                                                                                                                                                   n_iter,
                                                                                                                                                   n_runs_per_iter = q,
                                                                                                                                                   alternative = paste0(alternative_details$alt_name_full),
                                                                                                                                                   iteration_number = i,
                                                                                                                                                   prob_self_sustain = TRUE,
                                                                                                                                                   lambda_over_x_years = 10)

                                                                     # Results by colony
                                                                      results_summary_for_this_iteration_by_pop <- dapva::makeResultsSummaryOneIteration( results_all_for_this_iteration_fall,
                                                                                                                                                  by_pop = 'yes',
                                                                                                                                                  initial_year = parameterByIterTracking$initial_year[i],
                                                                                                                                                  yrs = parameterByIterTracking$yrs[i],
                                                                                                                                                  n_iter,
                                                                                                                                                  n_runs_per_iter = q,
                                                                                                                                                  alternative = paste0(alternative_details$alt_name_full),
                                                                                                                                                  iteration_number = i,
                                                                                                                                                  prob_self_sustain = TRUE,
                                                                                                                                                  lambda_over_x_years = 10)

                                                                      return(list(results_summary_for_this_iteration_overall, results_summary_for_this_iteration_by_pop,
                                                                                  results_all_for_this_iteration))
                                                                   
                                                                   # return(results_all_for_this_iteration)   
                                                                      } # end iteration loop
    
    n_it_w_results <- dim(results_summary_all_iterations)[1] # not the same as batch size as we removed those where there was an error
    if(n_it_w_results > 1){
      results_summary_all_iterations_overall_int[[batch]] <- do.call("rbind", results_summary_all_iterations[1:n_it_w_results])
      results_summary_all_iterations_by_pop_int[[batch]] <- do.call("rbind", results_summary_all_iterations[(n_it_w_results+1):(n_it_w_results*2)])
      results_all_iterations[[batch]] <- do.call("rbind", results_summary_all_iterations[(n_it_w_results*2+1):(n_it_w_results*3)])
    }
    if(n_it_w_results == 1 || n_iter == 1){ # e.g. when you are running a baseline case and so just have one iteration
      results_summary_all_iterations_overall_int[[batch]] <- results_summary_all_iterations[[1]]
      results_summary_all_iterations_by_pop_int[[batch]] <- results_summary_all_iterations[[2]]
      results_all_iterations[[batch]] <- results_summary_all_iterations[[3]]
    }
    
    if(flexible_convergence_iteration_on == 'yes'){
      # At the end of each batch, check to see if we have converged and can stop the number of iteration
      results_summary_all_iterations_overall <- do.call("rbind", results_summary_all_iterations_overall_int)
      
      int_persis <- makeResultsSummary(results_summary_all_iterations = results_summary_all_iterations_overall,
                                       metric = "probability of persistence", initial_year = 1, credible_interval = 0.95)
      
      # Calulate how many iterations have returned results so far (not the same as i necessaryily since the parallal computing throws out results if it gives an error)
      # Also renumber the iteraitons outside of the dataframe so can use it below
      n_iter_so_far <- length(unique(results_summary_all_iterations_overall$iteration))
      n_iter_so_far - convergence_band_length_n_iter
      iteration_renumbered  <- rep(1:n_iter_so_far, each = 3)

      int_persis <-  do.call("rbind",lapply((n_iter_so_far - convergence_band_length_n_iter+1):n_iter_so_far, # for the last x iterations, where x is the convergence band length
                     function(x) makeResultsSummary(results_summary_all_iterations = results_summary_all_iterations_overall[which(iteration_renumbered <= x),],
                                              metric = "probability of persistence", initial_year = 1, credible_interval = 0.95))
      )

      int_selfsustain <-  do.call("rbind",lapply((n_iter_so_far - convergence_band_length_n_iter+1):n_iter_so_far, # for the last x iterations, where x is the convergence band length
                                            function(x) makeResultsSummary(results_summary_all_iterations = results_summary_all_iterations_overall[which(iteration_renumbered <= x),],
                                                                           metric = "probability of self-sustaining population", initial_year = 1, credible_interval = 0.95))
      )
      
      convergence_tracking_persis_iter <- int_persis$mean[which(int_persis$year == 50)]
      convergence_tracking_selfsustain_iter <- int_selfsustain$mean[which(int_selfsustain$year == 50)]
      
      if(batch >= min_n_batches){
        
        CB_persis_upper_limit_iter <- convergence_tracking_persis_iter[batch - convergence_band_length_n_batches] + convergence_band_halfwidth_iter
        CB_persis_lower_limit_iter <- convergence_tracking_persis_iter[batch - convergence_band_length_n_batches] - convergence_band_halfwidth_iter
        
        CB_selfsustain_upper_limit_iter <- convergence_tracking_selfsustain_iter[batch - convergence_band_length_n_batches] + convergence_band_halfwidth_iter
        CB_selfsustain_lower_limit_iter <- convergence_tracking_selfsustain_iter[batch - convergence_band_length_n_batches] - convergence_band_halfwidth_iter
        
        # if within the convergence criteria for both metrics of interest, can stop
        if(sum(convergence_tracking_persis_iter <= CB_persis_upper_limit_iter) >= convergence_band_length_n_iter & 
           sum(convergence_tracking_persis_iter >= CB_persis_lower_limit_iter) >= convergence_band_length_n_iter){
          converged_for_persist_iter <- "yes"
        }
        if(sum(convergence_tracking_selfsustain_iter <= CB_selfsustain_upper_limit_iter) >= convergence_band_length_n_iter & 
           sum(convergence_tracking_selfsustain_iter >= CB_selfsustain_lower_limit_iter) >= convergence_band_length_n_iter){
          converged_for_selfsustain_iter <- "yes"
        }
        
        if(converged_for_persist_iter == "yes" & 
           converged_for_selfsustain_iter == "yes") {
          print("Reached enough iterations, breaking out of iteration batch for loop")
          break # break out of the for loop
        }     
        
      }
    }
    

    
  }
  
  
  # If have to stop code early, can continue on here to use what we have so far
  
  results_summary_all_iterations_overall <- do.call("rbind", results_summary_all_iterations_overall_int)
  results_summary_all_iterations_by_pop <- do.call("rbind", results_summary_all_iterations_by_pop_int)
  results_all_iterations <- do.call("rbind", results_all_iterations)
  
  
  
  
#---- Do the sensitivity analysis on the overall results. ----

  # TO DO: Add something in here so that it checks what the actual number of 
  #iterations used was and then update the parameter by tracking accordingly
  # Need to do this now that have added in this flexible way of determining the # of iterations
  
  
  # Not set up to do it by populations/colony
  print("Running the sensitivity analysis.")
  
  # Specify which variables don't make sense to include in the sensitivity analysis
  drops <- c("yrs",#no uncertainty here; 
             "initial_year",#no uncertainty here; 
             "dispersal_CSFmodel_lessEqual1km", #no uncertainty here; part of larger model uncertainty
             "dispersal_CSFmodel_greater1kmlessequal2km", #no uncertainty here; part of larger model uncertainty
             "dispersal_CSFmodel_greater1kmlessequal2km", #no uncertainty here; part of larger model uncertainty
             "dispersal_MoreGoShortmodel_lessEqual1km", #no uncertainty here; part of larger model uncertainty
             "dispersal_MoreGoShortmodel_greater1kmlessequal2km", #no uncertainty here; part of larger model uncertainty
             "dispersal_MoreGoShortmodel_greater1kmlessequal2km"#no uncertainty here; part of larger model uncertainty
             )
  
  # Do the sensitivity analysis and make a tornado for each alternative
  
  parameterByIterTracking_this_alt <- parameterByIterTracking
  results_all_this_alt  <- results_summary_all_iterations_overall
  
  # Clean up parameterByIterTracking_this_alt and remove any variables that don't
  # make sense to include in the sensitivity analysis
  parameterByIterTracking_this_alt_clean <- parameterByIterTracking_this_alt[ , !(names(parameterByIterTracking_this_alt) %in% drops)]
  
  # Add order to the ones that are factors
  parameterByIterTracking_this_alt_clean$drawdown_beforeMidJuly <- ordered(parameterByIterTracking_this_alt_clean$drawdown_beforeMidJuly, 
                                                                           levels = c("no","yes"))
  parameterByIterTracking_this_alt_clean$dispersal_CSF_vs_MoreGoShort <- ordered(parameterByIterTracking_this_alt_clean$dispersal_CSF_vs_MoreGoShort, 
                                                                                 levels = c("CSF", "MoreGoShort"))
  parameterByIterTracking_this_alt_clean$bullfrogMgmt_effective <- ordered(parameterByIterTracking_this_alt_clean$bullfrogMgmt_effective, 
                                                                           levels = c("no", "yes"))
  
  parameterByIterTracking_this_alt_clean$dispersal_allowed_outside <- ordered(parameterByIterTracking_this_alt_clean$dispersal_allowed_outside, 
                                                                           levels = c("no", "yes"))
  
  tornado_parameter_labels <- as.data.frame(matrix(nrow = ncol(parameterByIterTracking_this_alt_clean), ncol = 2))
  colnames(tornado_parameter_labels) <- c("name", "label")
  tornado_parameter_labels$name <- colnames(parameterByIterTracking_this_alt_clean)
  tornado_parameter_labels$label <- paste(tornado_parameter_labels$name) # for now, same as name. Can tweak later if desired.
  
  # Update a few tornado labels - could tidy this up by putting it in a function and moving it to a different file, do later if time
  
  # Reprodcution - proportion of females who lay eggs
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "p_females_lay_eggs_mean_A2")] <- "proportion of A2 lay eggs - mean"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "p_females_lay_eggs_mean_A3_A4plus")] <- "proportion of A3 and A4plus lay eggs - mean"
  # tornado_parameter_labels$label[which(tornado_parameter_labels$name == "p_females_lay_eggs_mean_A4plus")] <- "proportion of A4plus lay eggs - mean"
  
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "p_females_lay_eggs_sd_A2")] <- "proportion of A2 lay eggs - temporal variation (sd)"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "p_females_lay_eggs_sd_A3_A4plus")] <- "proportion of A3 and A4plus lay eggs - temporal variation (sd)"
  # tornado_parameter_labels$label[which(tornado_parameter_labels$name == "p_females_lay_eggs_sd_A4plus")] <- "proportion of A4plus lay eggs - temporal variation (sd)"
  
  # Reproduction - number of eggs per reproductively active female
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "num_eggs_per_active_female_mean_A2")] <- "number of eggs per A2 if lay eggs  - mean"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "num_eggs_per_active_female_mean_A3")] <- "number of eggs per A3 if lay eggs  - mean"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "num_eggs_per_active_female_mean_A4plus")] <- "number of eggs per A4plus if lay eggs  - mean"
  
  # Survival
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_mean_eggs_no_threats")] <- "egg survival - mean"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_mean_tadpoles_no_threats")] <- "tadpole survival - mean"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_mean_yoy_no_threats")] <- "yoy survival - mean"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_mean_juv_no_threats")] <- "juv survival - mean"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_mean_adult_no_threats")] <- "adult survival - mean"
  
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_sd_eggs_no_threats")] <- "egg survival - temporal variation (sd)"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_sd_tadpoles_no_threats")] <- "tadpole survival - temporal variation (sd)"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_sd_yoy_no_threats")] <- "yoy survival - temporal variation (sd)"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_sd_juv_no_threats")] <- "juv survival - temporal variation (sd)"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_sd_adult_no_threats")] <- "adult survival - temporal variation (sd)"
  
  
  # Survival - pct reduced, bullfrogs
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_eggs_bullfrogs")] <- "egg survival reduction if bullfrogMgt not effective"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_tadpoles_bullfrogs")] <- "tadpoles survival reduction if bullfrogMgt not effective"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_yoy_bullfrogs")] <- "yoy survival reduction if bullfrogMgt not effective"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_juvenile_bullfrogs")] <- "juvenile survival reduction if bullfrogMgt not effective"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_adult_bullfrogs")] <- "adult survival reduction if bullfrogMgt not effective"
  
  # Survival - pct reduced, roads
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_eggs_roads")] <- "egg survival reduction from roads"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_tadpoles_roads")] <- "tadpoles survival reduction from roads"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_yoy_roads")] <- "yoy survival reduction from roads"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_juvenile_roads")] <- "juvenile survival reduction from roads"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_adult_roads")] <- "adult survival reduction from roads"
  
  # Survival - pct reduced, chytrid
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_eggs_chytrid")] <- "egg survival reduction from chytrid"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_tadpoles_chytrid")] <- "tadpoles survival reduction from chytrid"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_yoy_chytrid")] <- "yoy survival reduction from chytrid"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_juvenile_adult_chytrid")] <- "juvenile/adult survival reduction from chytrid"

  # Survival - pct reduced, drawdown
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_tadpoles_drawdownPartial")] <- "tadpoles survival reduction from partial drawdown"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "s_pct_reduced_tadpoles_drawdownComplete")] <- "tadpoles survival reduction from complete drawdown"
  
  # Other
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "drawdown_completeVSpartial_freq")] <- "frequency of complete vs. partial drawdowns"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "bullfrogMgmt_effective")] <- "bullfrog mgmt. effective (yes = high, no = low)"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "drawdown_beforeMidJuly")] <- "drawdowns before mid-July (yes = high, no = low)"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "ephemeral_freq_dry")] <- "frequency of ephemeral wetland dry years"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "carrying_capacity_BSCWMA")] <- "carrying capacity"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "quasi_extinction_threshold")] <- "quasi-extinction threshold"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "wetland_eggTadSurv_TempCor_noEph")] <- "wetland correlation for egg and tadpole survival"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "p_yoy_disperse")] <- "proportion of yoy that disperse"
  tornado_parameter_labels$label[which(tornado_parameter_labels$name == "dispersal_CSF_vs_MoreGoShort")] <- "dispersal model (CSF = low, MoreGoShort = high)"
  
  
  # Do the sensitivity analysis
  paramSens_persist <- dapva::makeParameterSens(parameterByIterTracking = parameterByIterTracking_this_alt_clean,
                                 results_all_this_alt = results_all_this_alt,
                                 metric = "probability of persistence",
                                 start_year = 1,
                                 nyrs = 50,
                                 parameter_labels = tornado_parameter_labels)
  
  paramSens_selfsustain <- dapva::makeParameterSens(parameterByIterTracking = parameterByIterTracking_this_alt_clean,
                                                results_all_this_alt = results_all_this_alt,
                                                metric = "probability of self-sustaining population",
                                                start_year = 1,
                                                nyrs = 50,
                                                parameter_labels = tornado_parameter_labels)
  
#---- Store the name of the scenario. ----
  # Store the name as an object we know which scenario these results belong to
  name <- paste0(alternative_details$alt_name_short)
#---- Save the results for this scenario. ----
  save.image(file=paste0(name, version,".RData"))
  
#---- Close the scenarios loop. ----
}
#---- Stop the parallel computing and stop the timing. ----
parallel::stopCluster(cl)


}) # turn off sys.time


# Next step: Move the resulting Rdata files to the results folder on your
# local computer.If computing on a virtual machine, will need to export them.

# Then use the results wrapper script to extract the results information and graphs.
