# This script runs a decision-analytic population viability analysis for
# a Northern Leopard Frog reintroduction feasibility analysis. 

# The goal statement for this feasibility analysis is: The Idaho Department of
# Fish and Game, Wildlife Bureau is trying to gain insight into the likelihood
# of success of reintroducing northern leopard frogs to achieve local recovery
# of the species in the Boundary Smith Creek Wildlife Management Area in
# northern Idaho given that they are currently extirpated from the region.

# Main coder: Laura Keating
# Modeling subcommittee: Lea Randall,	Rebecca Stanton, Casey McCormack, Travis Seaborn
# April-June 2021

system.time({ # turn on the timer

#---- Clear the workspace. ----
rm(list = ls())
version <- "_v1test13" # insert short description to append to results to help identify

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
clusterSetRNGStream(cl, iseed = 30) # without parallel computing can just do set.seed(1234)

#---- Specify the alternatives to run.  -------------
alternatives_to_run <- dapva4nlf::dat_alternatives_to_run # some scenarios are preloaded in for easy calling

#rows_to_run <- c(10) # note that can't call 1 but just 0s anyways; all the rest seem to run fine; 3 got stuck in batches of 2 but works with more batches
# rows_to_run <- c(2:9) # note that can't call 1 but just 0s anyways; all the rest seem to run fine; 3 got stuck in batches of 2 but works with more batches
# rows_to_run <- c(6, 10, 11, 12) # run No bullfrog plus the three hypothetical scenarios
rows_to_run <- c(2)
#---- Specify number of iterations and number of runs per iterations.  -------------
n_iter  <- 2500# 500
flexible_convergence_iteration_on <- "no" # 'yes' or 'no', generally choose yes unless you are running a tornado and want to specify a # of iter
max_n_runs_per_iter <- 1000 # flexible convergence is always on at the run level

baseCase <- "no" # 'yes' or 'no'

doingRunConvTest <- "no"  # 'yes' or 'no'

if(baseCase == 'yes'){
  print(paste("Using one iteration of basecase parameters"))
  n_iter  <- 1
  flexible_convergence_iteration_on <- "no"
}

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
  
  if(baseCase == 'no'){
    parameterByIterTracking <- foreach::foreach(m=1:n_iter, .combine=rbind, 
                                                .packages='dapva4nlf') %dopar% {
      print(paste("Choosing parameters for iteration # ", m))
      parameterByIterTracking <- dapva4nlf::selectNLFIdahoParameterByIterTracking(inputs)
      return(parameterByIterTracking)
    }
  }
  
  if(baseCase == 'yes'){
    # Then just use the base case values
    parameterByIterTracking_baseCase <- dapva4nlf::selectNLFIdahoParameterByIterTracking(inputs, base_case = TRUE)
    parameterByIterTracking <-  parameterByIterTracking_baseCase
  }
  if(doingRunConvTest == 'yes'){
    # load("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/goBig_v1test12w2500it_parameterByIterTracking_forRunConvTest.RData")
    load("/Users/laurakeating/Documents/R/R_scripts/NLF_PVA/Results/goBig_v1test12w2500it_parameterByIterTracking_forRunConvTest.RData")# on my mac
  
    flexible_convergence_run_on <- "no" 
    parameterByIterTracking <-  parameterByIterTracking_forRunConvTest
    n_iter  <- nrow(parameterByIterTracking_forRunConvTest)
    flexible_convergence_iteration_on <- "no"
  }
  
  # Trying to better understand wetland correlations
  # using alternative 10 - testing existing pop
  # This is going the way I expected so appears to be working - less correlated is better
  #parameterByIterTracking_baseCase$wetland_eggTadSurv_TempCor_noEph <- 0.01 # prob of persist is 0.6117, self sustain is 0.3529; new random seed - prob of persist is 0.60927 , self sustain is 0.3311258
  # parameterByIterTracking_baseCase$wetland_eggTadSurv_TempCor_noEph[1] <- 1 # prob of persist 50 is 0.5125, self sustain is 0.2750
  #parameterByIterTracking <-  parameterByIterTracking_baseCase
  
  
  # Add in fix for when bullfrog management is no. Then the parameter for bullfrog management effective should always also be no.
  if(alternative_details$bullfrog_management == "no"){
    parameterByIterTracking$bullfrogMgmt_effective <- "no"
  }
  
  
  #---- Update  parameters with the condition of bullfrog management being effective. ----
  # Replace any iterations where bullfrog management is effective to NA for all the bullfrog threat related survival inputs.
  # This allows sensitivity to be calculated properly for the tornado
  rows_bullfrogMgt_effective <- which(parameterByIterTracking$bullfrogMgmt_effective == "yes")
  parameterByIterTracking$s_pct_reduced_eggs_bullfrogs[rows_bullfrogMgt_effective] <- NA
  parameterByIterTracking$s_pct_reduced_tadpoles_bullfrogs[rows_bullfrogMgt_effective] <- NA
  parameterByIterTracking$s_pct_reduced_yoy_bullfrogs[rows_bullfrogMgt_effective] <- NA
  parameterByIterTracking$s_pct_reduced_juvenile_bullfrogs[rows_bullfrogMgt_effective] <- NA
  parameterByIterTracking$s_pct_reduced_adult_bullfrogs[rows_bullfrogMgt_effective] <- NA
  
  #---- Update reproduction and survival parameters that don't have valid beta distribution shape parameters. ----
  
  # Identify any survival and reproduction inputs that don't have valid beta distribution shape parameters 
  # and replace the standard deviation with 10% of the mean (something small) to make it work.
  # This happens rarely but still once and a while (e.g. p of A2 females laying eggs 
  # really small with relatively large sd)

  parameterByIterTracking_orig <- parameterByIterTracking # keep the orig in case want to refer back later
  parameterByIterTracking <- dapva::parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking,
                                                                                     mean = "p_females_lay_eggs_mean_A2",
                                                                                     sd = "p_females_lay_eggs_sd_A2")

  parameterByIterTracking <- dapva::parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking,
                                                                                     mean = "p_females_lay_eggs_mean_A3_A4plus",
                                                                                     sd = "p_females_lay_eggs_sd_A3_A4plus")

  parameterByIterTracking <- dapva::parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking,
                                                                                    mean = "s_mean_eggs_no_threats",
                                                                                    sd = "s_sd_eggs_no_threats")

  parameterByIterTracking <- dapva::parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking,
                                                                                     mean = "s_mean_tadpoles_no_threats",
                                                                                     sd = "s_sd_tadpoles_no_threats")

  parameterByIterTracking <- dapva::parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking,
                                                                                     mean = "s_mean_yoy_no_threats",
                                                                                     sd = "s_sd_yoy_no_threats")

  parameterByIterTracking <- dapva::parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking,
                                                                                     mean = "s_mean_juv_no_threats",
                                                                                     sd = "s_sd_juv_no_threats")

  parameterByIterTracking <- dapva::parameterByIterTracking_replace_invalid_beta_shapeParam(parameterByIterTracking,
                                                                                     mean = "s_mean_adult_no_threats",
                                                                                     sd = "s_sd_adult_no_threats")
  #---- Run the PVA. ----
  # set it up to run in batches of 100 so we can monitor progress, frustrated by the lack of ability to have a progress bar
  
  print("Running the PVA with each iteration in parallel.")
  
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
                                                                     convergence_band_length <- 100 #50 # number of run
                                                                     burnin <- 200 #100 # this plus convergence_band_length is min number of runs (should rename, not true burn in because we do not discard these)
                                                                     convergence_tracking_persis <- c(vector(), rep(NA, max_n_runs_per_iter)) # initalize
                                                                     convergence_tracking_selfsustain <- c(vector(), rep(NA, max_n_runs_per_iter)) # initalize
                                                                     converged_for_persist <- "no"
                                                                     converged_for_selfsustain <- "no"
                                                                     
                                                                     # Start the run loop
                                                                     for(q in 1:max_n_runs_per_iter){# not in parallel here; in parallel at the iteration level

                                                                       # Specify a few more inputs for this iteration
                                                                       initial_year <- parameterByIterTracking$initial_year[i]
                                                                       yrs <- parameterByIterTracking$yrs[i]
                                                                       stage_classes <- c("eggs", "tadpoles", "yoy", "juv", "A2", "A3", "A4plus")
                                                                       
                                                                       # No longer using the 'outside' bucket in dispersal, leave functionality in in case change mind later
                                                                       if(alternative_details$restore_ephemeralWetlands == "yes"){
                                                                         wetlands <- c("cell3", "cell4", "cell7", "ephemeral_wetlands", "outside")
                                                                       }
                                                                       if(alternative_details$restore_ephemeralWetlands == "no"){
                                                                         wetlands <- c("cell3", "cell4", "cell7", "outside")
                                                                       } 
                                                                       
                                                              
                                                                       # Select the EV percentiles for each year in this run.
 
                                                                       # Survival for eggs and tadpoles correlated 100% (i.e. use the same percentiles)
                                                                       # Survival for the other life stages correlated (but not correlated with eggs/tadpoles)  (i.e. use the same percentiles)
                                                                       # No correlation between survival and reproduction
                                                                       # Wetland quality (which is partially correlated) will affect the egg/tadpole stage but not the terrestrial life stages or reproduction since that depends on 
                                                                       # female body condition coming out of winter. 
                                                                       # Re partially correlated EV for the egg/tadpole stage, no reason to think cells 3 and 4 will be any different so use the same there.
                                                                       # Cell 7 may be different because closer to agriculture. 
                                                                       # Ephemeral wetlands is a different type of system so definitely have the potential to be different, treat as uncorrelated.
             
                                                                       if(alternative_details$restore_ephemeralWetlands == "no"){
                                                                         correlation <- parameterByIterTracking$wetland_eggTadSurv_TempCor_noEph[i]
                                                                         sigma <- rbind(c(1,correlation), c(correlation,1)) # create the variance covariance matrix
                                                                         colnames(sigma) <- c("cells3and4", "cell7")
                                                                         rownames(sigma) <- c("cells3and4", "cell7")
                                                                         
                                                                         percentilesEV_survival_eggs_tad <- dapva::selectEVPercentilesNormal(cor_mat = sigma, n_years = parameterByIterTracking$yrs[i]) # note: increase yrs to see that the data is really has roughly the appropriate correlations
                                                                         
                                                                         # Separate out the EVs so that each wetland has its own col that can be called later
                                                                         percentilesEV_survival_eggs_tad$cell3 <- percentilesEV_survival_eggs_tad$cells3and4
                                                                         percentilesEV_survival_eggs_tad$cell4 <- percentilesEV_survival_eggs_tad$cells3and4
                                                                         percentilesEV_survival_eggs_tad <- percentilesEV_survival_eggs_tad[,c("cell3", "cell4", "cell7")]
                                                                       }
                                                                       
                                                                       if(alternative_details$restore_ephemeralWetlands == "yes"){
                                                                         correlation <- parameterByIterTracking$wetland_eggTadSurv_TempCor_noEph[i]
                                                                         sigma <- rbind(c(1,correlation,0), c(correlation,1, 0), c(0,0,1)) # create the variance covariance matrix
                                                                         colnames(sigma) <- c("cells3and4", "cell7", "ephemeral_wetlands")
                                                                         rownames(sigma) <- c("cells3and4", "cell7", "ephemeral_wetlands")
                                                                         
                                                                         percentilesEV_survival_eggs_tad <- dapva::selectEVPercentilesNormal(cor_mat = sigma, n_years = parameterByIterTracking$yrs[i])
                                                                         
                                                                         # Separate out the EVs so that each wetland has its own col that can be called later
                                                                         percentilesEV_survival_eggs_tad$cell3 <- percentilesEV_survival_eggs_tad$cells3and4
                                                                         percentilesEV_survival_eggs_tad$cell4 <- percentilesEV_survival_eggs_tad$cells3and4
                                                                         percentilesEV_survival_eggs_tad <- percentilesEV_survival_eggs_tad[,c("cell3", "cell4", "cell7", "ephemeral_wetlands")]
                                                                       }
                                                                
                                                                       sigma3 <- rbind(c(1,1), c(1,1)) # create the variance covariance matrix; make 2 fully correlated vectors and just use one, when time update code so that it can have just one var
                                                                       percentilesEV_survival_yoy_adult <- dapva::selectEVPercentilesNormal(cor_mat = sigma3, n_years = parameterByIterTracking$yrs[i])
                                                                       colnames(percentilesEV_survival_yoy_adult)[1] <- c("all_wetlands")

                                                                       sigma4 <- rbind(c(1,1), c(1,1)) # create the variance covariance matrix; make 2 fully correlated vectors and just use one, when time update code so that it can have just one var
                                                                       percentilesEV_reproduction <- dapva::selectEVPercentilesNormal(cor_mat = sigma4, n_years = parameterByIterTracking$yrs[i])
                                                                       colnames(percentilesEV_reproduction)[1] <- c("all_wetlands")
                                                                       
                                                                       # Check if there is an existing population in this alternative or not
                                                                       # Used for testing
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

                                                                       # Then check to see what the probability metrics are
                                                                       results_all_so_far <- plyr::rbind.fill(results_annual)
                                                                       
                                                                       check_if_enough_runs <- dapva::makeResultsSummaryOneIteration(results_all_so_far,
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
                                                                       
                                                                       if(flexible_convergence_run_on =="no"){ # Then don't allow it to stop
                                                                         finish <- FALSE
                                                                       }
                                                                  
                                                                        if(finish == TRUE){
                                                                          print(paste('Iteration', i, "stopped at", q, "runs"))
                                                                        break
                                                                        }
                                                                     } # close the run loop
                                                                     
                                                                     results_all_for_this_iteration <- plyr::rbind.fill(results_annual)
                                                                     
                                                                     # Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
                                                                     results_all_for_this_iteration_fall <- results_all_for_this_iteration # initialize
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
                                                                     return(list(results_summary_for_this_iteration_overall, 
                                                                                 results_summary_for_this_iteration_by_pop,
                                                                                 results_all_for_this_iteration))
                                                                     } # end iteration loop

    n_it_w_results <- dim(results_summary_all_iterations)[1] # not the same as batch size as we removed those where there was an error
    if(n_iter == 1){ n_it_w_results <- 1}
    if(n_it_w_results > 1){
      results_summary_all_iterations_overall_int[[batch]] <- do.call("rbind", results_summary_all_iterations[1:n_it_w_results])
      results_summary_all_iterations_by_pop_int[[batch]] <- do.call("rbind", results_summary_all_iterations[(n_it_w_results+1):(n_it_w_results*2)])
      results_all_iterations[[batch]] <- do.call("rbind", results_summary_all_iterations[(n_it_w_results*2+1):(n_it_w_results*3)])
    }
    if(n_it_w_results == 1){ # e.g. when you are running a baseline case and so just have one iteration
      results_summary_all_iterations_overall_int[[batch]] <- results_summary_all_iterations[[1]]
      results_summary_all_iterations_by_pop_int[[batch]] <- results_summary_all_iterations[[2]]
      results_all_iterations[[batch]] <- results_summary_all_iterations[[3]]
    }
    
    if(flexible_convergence_iteration_on == 'yes'){
      # At the end of each batch, check to see if we have converged and can stop the number of iteration
      results_summary_all_iterations_overall <- do.call("rbind", results_summary_all_iterations_overall_int)
      
      int_persis <- makeResultsSummary(results_summary_all_iterations = results_summary_all_iterations_overall,
                                       metric = "probability of persistence", initial_year = 1, credible_interval = 0.95)
      
      # Calculate how many iterations have returned results so far (not the same as i necessarily since the parallel 
      # computing throws out results if it gives an error). Also renumber the iterations outside of the dataframe so can use it below.
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
        
        if(converged_for_persist_iter == "yes" & converged_for_selfsustain_iter == "yes") {
          print("Reached enough iterations, breaking out of iteration batch for loop")
          break # break out of the for loop
        }     
      } # close if(batch >= min_n_batches)
    } # close if(flexible_convergence_iteration_on == 'yes')
  }

  # Note: If have to stop code early, can continue on here to use what we have so far
  
  # Combine the results from all the batches
  results_summary_all_iterations_overall <- do.call("rbind", results_summary_all_iterations_overall_int)
  results_summary_all_iterations_by_pop <- do.call("rbind", results_summary_all_iterations_by_pop_int)
  results_all_iterations <- do.call("rbind", results_all_iterations)

  #---- Do the sensitivity analysis on the overall results. ----
  # Note: Not set up to do it by population/colony
  
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
  
  # Clean up - remove any variables that don't make sense to include in the sensitivity analysis
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
  
  # Make tornado parameter labels
  tornado_parameter_labels <- dapva4nlf::makeTornadoParameterLabels(parameterByIterTracking = parameterByIterTracking_this_alt_clean)
  
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