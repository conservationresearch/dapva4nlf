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
version <- "_v1testing" # insert short description to append to results to help identify

#---- Load libraries, and set the random seed.  -------------
## Import libraries
library(dapva) # this is our own internal library that houses functions used here
library(dapva4nlf) # this is our own internal library that houses functions used here
library(dplyr) # for summarise
library(foreach) # for %do% nd %dopar%; this might come as part of DoParallel which is loaded later, test this later by trying without

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

rows_to_run <- c(3) # gets stuck with 3; note that can't call 1 but just 0s anyways; all the rest seem to run fine
#---- Specify number of iterations and number of runs per iterations.  -------------
n_iter  <- 10
max_n_runs_per_iter <- 10

#---- Start the scenario loop.  -------------
for(m in 1:length(rows_to_run)){ # loop through the different scenarios requested in the scenarios_to_run file
  row_to_run <- rows_to_run[m]
  print(paste0("Running alternative ", alternatives_to_run$alt_name_short[row_to_run]))
  alternative_details <- alternatives_to_run[row_to_run,]
  
#---- Get the inputs.  -------------

  inputs_all <- dapva4nlf::getNLFIdahoFeasinputs()
  inputs <- inputs_all[[1]]
  
#---- Choose the parameters for each iteration (i.e. parametric uncertainty). ----
  print("Selecting the parameters for each iteration in parallel.")
  parameterByIterTracking <- foreach::foreach(m=1:n_iter,  .combine=rbind,
                                              .packages='dapva4nlf'
                                               ) %dopar% {
                                                # ) %do% {

                                                print(paste("Choosing parameters for interation # ", m))
                                                parameterByIterTracking <- dapva4nlf::selectNLFIdahoParameterByIterTracking(inputs)
                                                return(parameterByIterTracking)
                                                
                                              }

#---- Run the PVA. ----
  # Need to have run the foreach loop for parameterByIterTracking first
  print("Running the PVA with each iteration in parallel.")
  
  # set it up to run in batches of 100 so we can monitor progress, frustrated by the lack of ability to have a progress bar
  
  results_summary_all_iterations_overall_int  <- list() # initialize
  results_summary_all_iterations_by_pop_int  <- list() # initialize
  
  batch_size <- n_iter/2
  batches <- split(1:n_iter, ceiling(seq_along(1:n_iter)/batch_size ))
  
  for(batch in 1:length(batches)){
    print(paste("Running batch ", batch, "; i.e. iterations", first(batches[[batch]]), "to ", last(batches[[batch]]), " of ", n_iter))
    
    results_summary_all_iterations <- foreach::foreach(i=batches[[batch]],
                                                       .combine=rbind,
                                                       # .verbose = T, # print out which iterations have been completed
                                                       .errorhandling = c("remove"), # remove/skip if the result has an error
                                                       .packages=c('foreach', 
                                                                   'dapva',# need foreach in here as per https://stackoverflow.com/questions/21128122/function-do-not-found-on-win-platform-only-when-using-plyr-and-doparallel
                                                                   'dapva4nlf')) %do% {  # change 'dopar' to 'do' if don't want to do the parallel computing
           
                                                                     results_annual <- list() # initalize
                                                                     finish <- FALSE # initalize
                                                                     for(q in 1:max_n_runs_per_iter){# not in parallel here; in parallel at the iteration level
                                                                       
                                                                       #Select the EV percentiles for each year in this run.
 
                                                                       # Survival for eggs and tadpoles correlated 100% (i.e. use the same percentiles)
                                                                       # Survival for the other life stages correlated (but not correlated with eggs/tadpoles)  (i.e. use the same percentiles)
                                                                       # No correlation between survival and reproduction
                                                                       # There is partially correlated variation in the vital rates between wetlands as some may be better than others from year to year
                                                                       # To see that the data is in fact correlated, try increasing the n_years to 10000 and run cor(x) or library(lattice) splom(x)
                                                                       # It is not a perfect algoritm but appropriate for the level of randomness we need I think
                                                                       
                                                                       # Note: increased nyears to 1000 and then just select first 50 since the permute algorithm sometimes gets stuck if there are not enough draws
                                                                       percentilesEV_survival_eggs_tad <- dapva::selectEVPercentilesNormal(input_names_w_EV = c("cell3", "cell4", "cell7", "ephemeral_wetlands"),
                                                                                                                                    correlation = parameterByIterTracking$wetland_vitalrate_correlations[i],
                                                                                                                                    n_years = 1000)[1:parameterByIterTracking$yrs[i],]
                                                                       
                                                                       percentilesEV_survival_yoy_adult <- dapva::selectEVPercentilesNormal(input_names_w_EV = c("cell3", "cell4", "cell7", "ephemeral_wetlands"),
                                                                                                                                    correlation = parameterByIterTracking$wetland_vitalrate_correlations[i],
                                                                                                                                    n_years = 1000)[1:parameterByIterTracking$yrs[i],]
                                                                       
                                                                       percentilesEV_reproduction <- dapva::selectEVPercentilesNormal(input_names_w_EV = c("cell3", "cell4", "cell7", "ephemeral_wetlands"),
                                                                                                                              correlation = parameterByIterTracking$wetland_vitalrate_correlations[i],
                                                                                                                              n_years = 1000)[1:parameterByIterTracking$yrs[i],]

                                                                       # Specify a few more inputs for this iteration
                                                                       initial_year <- parameterByIterTracking$initial_year[i]
                                                                       yrs <- parameterByIterTracking$yrs[i]
                                                                       stage_classes <- c("eggs", "tadpoles", "yoy", "juv", "A2", "A3", "A4plus")
                                                                       
                                                                       if(alternative_details$restore_ephemeralWetlands == "yes"){
                                                                         wetlands <- c("cell3", "cell4", "cell7", "ephemeral_wetlands", "outside")
                                                                       }
                                                                       if(alternative_details$restore_ephemeralWetlands == "no"){
                                                                         wetlands <- c("cell3", "cell4", "cell7", "outside")
                                                                       }                                                              

                                                                       # Run the annual loop
                                                                       results_annual[[q]] <- dapva4nlf::runAnnualLoopNLFIdahoPVA(parameterByIterTracking, yrs, i, q,
                                                                                                                       # dispersal_edge_list,dispersal_tracking,
                                                                                                                       initial_year, wetlands,stage_classes,
                                                                                                                       percentilesEV_survival_eggs_tad,
                                                                                                                       percentilesEV_survival_yoy_adult,
                                                                                                                       percentilesEV_reproduction,
                                                                                                                       alternative_details)

                                                                        # if(q == max_n_runs_per_iter*0.1){ # if we have run 10% of the max number of runs per iterations
                                                                        # 
                                                                        #   # Check to see if within 5% of either 0 or 1 since we noticed from covergence
                                                                        #   # plots that if it is at either of these two extremes, more iterations are typically not necessary
                                                                        # 
                                                                        #   results_all_so_far <- plyr::rbind.fill(results_annual)
                                                                        # 
                                                                        #   check_if_enough_runs <- makeResultsSummaryOneIteration(results_all_so_far,
                                                                        #                                                         by_pop = 'no',
                                                                        #                                                          initial_year = parameterByIterTracking$initial_year[i],
                                                                        #                                                          yrs = parameterByIterTracking$yrs[i],
                                                                        #                                                          n_iter,
                                                                        #                                                          n_runs_per_iter = q,
                                                                        #                                                          alternative = paste0(alternative_details$alt_name_full),
                                                                        #                                                          iteration_number = i)
                                                                        # 
                                                                        #   prob_of_persis_so_far <- check_if_enough_runs[which(check_if_enough_runs$metric == "probability of persistence"),
                                                                        #                                                 paste(parameterByIterTracking$initial_year[i] + parameterByIterTracking$yrs[i]-1)]
                                                                        # 
                                                                        #   # if with 5% of 0 or 1, then no need to do more runs
                                                                        #  if(prob_of_persis_so_far <= 0.05){finish <- TRUE}
                                                                        #   if(prob_of_persis_so_far >= 0.95){finish <- TRUE}
                                                                        # }
                                                                        # 
                                                                        # if(finish == TRUE){
                                                                        #   print(paste('Iteration', i, "stopped at", q, "runs"))
                                                                        # break
                                                                        # }
                                                                       
                                                                        # Return the results for this run
                                                                       # return(results_annual)
                                                                     
                                                                     }
                                                                     
                                                                      results_all_for_this_iteration <- plyr::rbind.fill(results_annual)

                                                                     # Overall results
                                                                      results_summary_for_this_iteration_overall <- makeResultsSummaryOneIteration(results_all_for_this_iteration,
                                                                                                                                                   by_pop = 'no',
                                                                                                                                                   initial_year = parameterByIterTracking$initial_year[i],
                                                                                                                                                   yrs = parameterByIterTracking$yrs[i],
                                                                                                                                                   n_iter,
                                                                                                                                                   n_runs_per_iter = q,
                                                                                                                                                   alternative = paste0(alternative_details$alt_name_full),
                                                                                                                                                   iteration_number = i)

                                                                     # Results by colony
                                                                      results_summary_for_this_iteration_by_pop <- makeResultsSummaryOneIteration(results_all_for_this_iteration,
                                                                                                                                                  by_pop = 'yes',
                                                                                                                                                  initial_year = parameterByIterTracking$initial_year[i],
                                                                                                                                                  yrs = parameterByIterTracking$yrs[i],
                                                                                                                                                  n_iter,
                                                                                                                                                  n_runs_per_iter = q,
                                                                                                                                                  alternative = paste0(alternative_details$alt_name_full),
                                                                                                                                                  iteration_number = i)

                                                                      return(list(results_summary_for_this_iteration_overall, results_summary_for_this_iteration_by_pop))
                                                                   
                                                                   # return(results_all_for_this_iteration)   
                                                                      } # end iteration loop
    
    results_summary_all_iterations_overall_int[[batch]] <- do.call("rbind", results_summary_all_iterations[1:batch_size])
    results_summary_all_iterations_by_pop_int[[batch]] <- do.call("rbind", results_summary_all_iterations[(batch_size+1):(batch_size*2)])
    
  }
  
  
  # If have to stop code early, can continue on here to use what we have so far
  
  results_summary_all_iterations_overall <- do.call("rbind", results_summary_all_iterations_overall_int)
  results_summary_all_iterations_by_pop <- do.call("rbind", results_summary_all_iterations_by_pop_int)
  
  
  
  
#---- Do the sensitivity analysis on the overall results. ----

  # Not set up to do it by populations/colony
  print("Running the sensitivity analysis.")
  
  # Specify which variables don't make sense to include in the sensitivity analysis
  drops <- c("yrs",
             "initial_year"
             )
  
  # Do the sensitivity analysis and make a tornado for each alternative
  
  parameterByIterTracking_this_alt <- parameterByIterTracking
  results_all_this_alt  <- results_summary_all_iterations_overall
  
  # Clean up parameterByIterTracking_this_alt and remove any variables that don't
  # make sense to include in the sensitivity analysis
  parameterByIterTracking_this_alt_clean <- parameterByIterTracking_this_alt[ , !(names(parameterByIterTracking_this_alt) %in% drops)]
  
  # Add order to the ones that are factors

  # Do the sensitivity analysis
  paramSens <- makeParameterSens(parameterByIterTracking = parameterByIterTracking_this_alt_clean,
                                 results_all_this_alt = results_all_this_alt,
                                 metric = "probability of persistence",
                                 start_year = parameterByIterTracking$initial_year[1],
                                 nyrs = parameterByIterTracking$yrs[1])
  
  
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
