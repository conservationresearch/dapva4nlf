# This script uses the Rdata files saved as the output from running the NLF Idaho Feasability PVA
# wrapper script to extract and graph the results for the
# 2021 SDM report.

#---- Load libraries. ----
library(dapva)
library(dapva4nlf)
library(gridExtra) # for grid.arrange

#---- Specify the location where you saved the Rdata files. ----
# Add a folder in there  called 'ForReport' to help organize the outputs.

path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results"# on my work PC
#path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/BTPD_PVA/Results/BTPD_baseline_results_march17"# on my mac
setwd(path_to_results_folder) # on my mac

#---- Extract the results from the Rdata files saved for each scenario. ----
# One by one, load in the Rdata files and then export the results to csvs

files <-  list.files(path = ".","*.RData", full.names="TRUE")
for (i in 1:length(files)){
  # Load the Rdata file
  load(files[i])

  # Export the parameters used
  write.csv(parameterByIterTracking, file = paste0("params_", name, version,".csv"),row.names = FALSE)

  # Export the 'by population' results
  write.csv(results_summary_all_iterations_by_pop, file = paste0("results_by_pop_", name, version,".csv"), row.names = FALSE)

  # Summarize the 'by population' results into 'overall' results and export that
  write.csv(results_summary_all_iterations_overall, file = paste0("results_overall_", name, version,".csv"), row.names = FALSE)

  # Sensitivity analysis results
  write.csv(paramSens[1], # write the summary, which is the first output (the second is yrs, used in the tornado)
           file = paste0("paramSens_", name, version,".csv"), row.names = FALSE)

  # Draw the associated tornado
  tornado <- dapva::drawTornado(paramSens = paramSens,
                         metric = "probability of persistence",
                         year = 50,
                         title = paste(name), breaks = 0.1)

  # Export the tornado diagram
  tiff(filename = paste0("tornado_", name,version,".tiff"),
       width=12, height=6, units="in",
       pointsize=8, compression="lzw", bg="white", res=600)
  print(tornado)
  dev.off()

  # Summarize the population specific results for this scenario/alternative.
  results_summary_num_indiv_by_pop <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations = results_summary_all_iterations_by_pop,
                                                             metric = "mean total number of individuals",
                                                             initial_year = parameterByIterTracking$initial_year[1])
  
  results_summary_prob_persist_by_pop <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_all_iterations_by_pop,
                                                                metric = "probability of persistence",
                                                                initial_year = parameterByIterTracking$initial_year[1])
  
  results_summary_prob_selfsustaining_by_pop <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_all_iterations_by_pop,
                                                                              metric = "probability of self-sustaining population",
                                                                              initial_year = parameterByIterTracking$initial_year[1])
  
  # Graph
  abundance_graph <- dapva::graphResultsSummary(results_summary_num_indiv_by_pop)
  abundance_flyingBars <- dapva::graphFlyingBars(results_summary_all_iterations = results_summary_all_iterations_by_pop,
                                          metric = "mean total number of individuals", year = parameterByIterTracking$initial_year[1] + parameterByIterTracking$yrs[1] - 1) # year is initial_year + yrs - 1

  persistence_graph <- dapva::graphResultsSummary(results_summary_prob_persist_by_pop)
  persistence_flyingBars <- dapva::graphFlyingBars(results_summary_all_iterations = results_summary_all_iterations_by_pop,
                                            metric = "probability of persistence", year = parameterByIterTracking$initial_year[1] + parameterByIterTracking$yrs[1] - 1) # year is initial_year + yrs - 1

  selfsustaining_graph <- dapva::graphResultsSummary(results_summary_prob_selfsustaining_by_pop)
  selfsustaining_flyingBars <- dapva::graphFlyingBars(results_summary_all_iterations = results_summary_all_iterations_by_pop,
                                                   metric = "probability of self-sustaining population", year = parameterByIterTracking$initial_year[1] + parameterByIterTracking$yrs[1] - 1) # year is initial_year + yrs - 1
  
  # Export the graphs

  filename <- paste("graph_ab_overTime_by_pop_", name, version, "_iter_", n_iter, ".tiff", sep="")
  tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
  print(abundance_graph)
  dev.off()

  filename <- paste("graph_ab_flyingBars_by_pop_", name, version, "_iter_", n_iter, ".tiff", sep="")
  tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
  print(abundance_flyingBars)
  dev.off()

  filename <- paste("graph_per_overTime_by_pop_", name, version, "_iter_", n_iter, ".tiff", sep="")
  tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
  print(persistence_graph)
  dev.off()

  filename <- paste("graph_per_flyingBars_by_pop_", name, version, "_iter_", n_iter, ".tiff", sep="")
  tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
  print(persistence_flyingBars)
  dev.off()
  
  
  filename <- paste("graph_ss_overTime_by_pop_", name, version, "_iter_", n_iter, ".tiff", sep="")
  tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
  print(selfsustaining_graph)
  dev.off()
  
  filename <- paste("graph_ss_flyingBars_by_pop_", name, version, "_iter_", n_iter, ".tiff", sep="")
  tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
  print(selfsustaining_flyingBars)
  dev.off()

}

#---- Graph the overall results for all the scenarios/alternatives. ----

# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_overall_")
results_all_iter_list <- lapply(temp_iter, read.csv)
results_all_iter <- do.call(rbind, results_all_iter_list)
colnames(results_all_iter)[7:ncol(results_all_iter)] <- 1:50

# Overall results of interest to use going forward
results_summary_num_indiv <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations = results_all_iter,
                                                                  metric = "mean total number of individuals",
                                                                  initial_year = 1, credible_interval = 0.95)

results_summary_prob_persist <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_all_iter,
                                                                     metric = "probability of persistence",
                                                                     initial_year = 1, credible_interval = 0.95)

results_summary_prob_selfsustaining <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_all_iter,
                                                                     metric = "probability of self-sustaining population",
                                                                     initial_year = 1, credible_interval = 0.95)

# Graph the overall results
(abundance_graph <- dapva::graphResultsSummary(results_summary_num_indiv))
(abundance_flyingBars <- dapva::graphFlyingBars(results_summary_all_iterations = results_all_iter,
                                         metric = "mean total number of individuals", year = 50,
                                         credible_interval = 0.95))

(persistence_graph <- dapva::graphResultsSummary(results_summary_prob_persist))
(persistence_flyingBars <- dapva::graphFlyingBars(results_summary_all_iterations = results_all_iter,
                                           metric = "probability of persistence",
                                           year = 50,
                                           credible_interval = 0.95))

(selfsustaining_graph <- dapva::graphResultsSummary(results_summary_prob_selfsustaining))
(selfsustaining_flyingBars <- dapva::graphFlyingBars(results_summary_all_iterations = results_all_iter,
                                                  metric = "probability of self-sustaining population",
                                                  year = 50,
                                                  credible_interval = 0.95))

# Export the graphs

filename <- paste("graph_abundance_overTime", version, "_iter_", n_iter, ".tiff", sep="")
tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
print(abundance_graph)
dev.off()

filename <- paste("graph_abundance_flyingBars", version, "_iter_", n_iter, ".tiff", sep="")
tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
print(abundance_flyingBars)
dev.off()

filename <- paste("graph_persistence_overTime", version, "_iter_", n_iter, ".tiff", sep="")
tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
print(persistence_graph)
dev.off()

filename <- paste("graph_persistence_flyingBars", version, "_iter_", n_iter, ".tiff", sep="")
tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
print(persistence_flyingBars)
dev.off()

filename <- paste("graph_selfsustaining_overTime", version, "_iter_", n_iter, ".tiff", sep="")
tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
print(selfsustaining_graph)
dev.off()

filename <- paste("graph_selfsustaining_flyingBars", version, "_iter_", n_iter, ".tiff", sep="")
tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
print(selfsustaining_flyingBars)
dev.off()


# Make an output table with the format we want for the report
 results_summary_prob_persist_table <- results_summary_prob_persist[which(results_summary_prob_persist$year == yrs),]
# Export this probability of persistence results summary table
filename <- paste("probPersist_summary_table", version, "_iter_", n_iter, ".csv", sep="")
write.csv(results_summary_prob_persist_table, file = filename)

# Make an output table with the format we want for the report
results_summary_prob_selfsustaining_table <- results_summary_prob_selfsustaining[which(results_summary_prob_selfsustaining$year == yrs),]
# Export this probability of persistence results summary table
filename <- paste("probSelfSustain_summary_table", version, "_iter_", n_iter, ".csv", sep="")
write.csv(results_summary_prob_selfsustaining_table, file = filename)


############## Graphs of individual runs and iterations to show the process ######
# Might be useful to make a function where you can plot runs within an iteration by total and pop and stage

# Might also be useful to make a function where you can plot the results of a few iterations

# ALso to do: Make graphs of the survival rates with the different threats to show Lea and Rebecca 




# Load the Rdata file
i = 1  
load(files[i])

# Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
results_all_iterations_fall <- results_all_iterations # initalize

for(j in 1:yrs){
  nrow(results_all_iterations_fall)
  results_all_iterations_fall[which(results_all_iterations_fall$class == "eggs"),paste(j)] <- 0
  results_all_iterations_fall[which(results_all_iterations_fall$class == "tadpoles"),paste(j)] <- 0 
}

# Graph a few individual iterations to show what it looks like

results_all_iterations_fall_iter1_run1 <- results_all_iterations_fall[which(results_all_iterations_fall$iteration == 2 &
                                                                              results_all_iterations_fall$run ==1),]

results_summary_for_iter1_run1 <- dapva::makeResultsSummaryOneIteration(results_all_iterations_fall_iter1_run1,
                                                                        by_pop = 'yes',
                                                                        initial_year = 1,
                                                                        yrs = 50,
                                                                        n_iter = 1,
                                                                        n_runs_per_iter = 1,
                                                                        alternative = paste0(alternative_details$alt_name_full),
                                                                        iteration_number = 1,
                                                                        prob_self_sustain = FALSE, # much faster without this, not needed here
                                                                        lambda_over_x_years = 10)


results_summary_num_indiv <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_for_iter1_run1 ,
                                                                  metric = "mean total number of individuals",
                                                                  initial_year = 1, credible_interval = 0.95)


(abundance_graph <- dapva::graphResultsSummary(results_summary_num_indiv))



############## Convergence testing - number of runs per iteration ######

# Still do to - update code so can be more flexible on the # of runs
# e.g. min always of 100, after that check after x incremets etc. rather than all the way to 1000

# Intalize the a list to store results for a handful of itrations
convergence_test_runs_per_iter <- list()

for(i in 1:4){
  convergence_test_runs_per_iter[[i]] <- dapva::convergenceTestRunsPerIteration(
    results_all_for_this_iteration = results_all_iterations_fall[which(results_all_iterations_fall$iteration == i),],
    test_interval = 10,
    iteration_number = i,
    initial_year = 1,
    final_year = 50,
    num_rand_pulls_per_subset_size = 100)
}

convergence_test_runs_multiple_iter <- do.call("rbind", convergence_test_runs_per_iter)


graphs <- dapva::graphCongvTestRunsPerIter(convergence_test_runs_multiple_iter,
                                           x_location_vertical_line = 1000,
                                           title = " ")
graphs[[1]]
graphs[[2]]

############## Convergence testing - number of iterations ######


# NEed to use a dataset with more iterations than needed so can show convergence
# For example, if using 500 iterations need to use a dataset with e.g. 2000 iterations here

# Resample to visually inspect convergence
convergence_test <- dapva::convergenceTestIterations(results_all_this_alt = results_summary_all_iterations_overall,
                                                     test_interval = 100,
                                                     num_rand_pulls_per_subset_size = 100,
                                                     initial_year = 1,
                                                     final_year = 50)


# Make graphs for visual inspection
graphs <- dapva::graphCongvTestIter(convergence_test, x_location_vertical_line = 500,
                                    title = " ")

(p_prob_persist_conv_iter_mean <- graphs[[2]])
(p_prob_persist_conv_iter_median <- graphs[[4]])

(p_abundance_conv_iter_mean <- graphs[[1]])
(p_abundance_conv_iter_median <- graphs[[3]])




# Export the graphs

# Do later


# Extract the range of prob of persistence from 500 iterations
int <- convergence_test$prob_persist_mean[which(convergence_test$subset_size == 500)]
min(int) 
max(int) 
max(int) - min(int)


############## Explore what the results would have looked like if we combined parametric and process uncertainty to one prob value ######


# Rename the runs so that they have unique IDs with iteration and run since 
# here pretending like they are all independent runs that we want one set of results for
results_all_iterations_fall$run <- paste(results_all_iterations_fall$iteration, results_all_iterations_fall$run)

# Run the prob of persistence function on the whole thing to show what it would look like if process and parametric were together
n_iter_runcombos <- length(unique(paste(results_all_iterations_fall$iteration, results_all_iterations_fall$run)))
results_summary_for_all_iter_and_runs <- dapva::makeResultsSummaryOneIteration(results_all_iterations_fall,
                                                                                    by_pop = 'no',
                                                                                    initial_year = 1,
                                                                                    yrs = 50,
                                                                                    n_iter = 1,
                                                                                    n_runs_per_iter = n_iter_runcombos,
                                                                                    alternative = paste0(alternative_details$alt_name_full),
                                                                                    iteration_number = 1,
                                                                                    prob_self_sustain = FALSE, # much faster without this, not needed here
                                                                                    lambda_over_x_years = 10)


# Graph the results
# Point here is that if we combine parametric and process uncertainty then:
# Results seem more optimistic (in this case); point is that it is different
# Don't get any insights into where we should put more effort to learn more to make a more informed decision



results_summary_prob_persist <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_for_all_iter_and_runs,
                                                                     metric = "probability of persistence",
                                                                     initial_year = 1, credible_interval = 0.95)


(persistence_graph <- dapva::graphResultsSummary(results_summary_prob_persist))

