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
           file = paste0("paramSens_", name,version,".csv"), row.names = FALSE)

  # Draw the associated tornado
  tornado <- dapva::drawTornado(paramSens = paramSens,
                         metric = "probability of persistence",
                         start_year = parameterByIterTracking$initial_year[1],
                         title = "Test", breaks = 0.1)

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




############## Explore what the results would have looked like if we combined parametric and process uncertainty to one prob value ######
# Load the Rdata file
i = 1  
load(files[i])


test <- results_summary_all_iterations
test1 <- test[[49]]
test2 <- do.call("rbind", results_summary_all_iterations[49:72])
