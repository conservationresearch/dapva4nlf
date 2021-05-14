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




############## Load basecase results for one alternative to use in the examples in the following sections ######
# Load the Rdata file

files_basecase <-  list.files(path = ".","*basecase.RData", full.names="TRUE")
i = 1  
load(files_basecase[i])

# Make the classes factors

results_all_iterations$class <- factor(results_all_iterations$class, levels = c("eggs", "tadpoles", "yoy", "juv", "A2", "A3", "A4plus"))
results_basecase <- results_all_iterations # initalize

# Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
results_basecase_fall <- results_all_iterations # initalize

for(j in 1:yrs){
  nrow(results_basecase_fall)
  results_basecase_fall[which(results_basecase_fall$class == "eggs"),paste(j)] <- 0
  results_basecase_fall[which(results_basecase_fall$class == "tadpoles"),paste(j)] <- 0 
}

############## basecase - Graphs of individual runs to show the process ######

# Start by showing for each class and population; include eggs and tadpoles for illustrative purposes rather than the fall numbers used for the ttoals
results_to_use <- results_basecase  # results_basecase_fall if want to get the total #

test2 <- melt(results_to_use, id.vars=c("iteration", "run", "pop", "class", "sex"))
colnames(test2)[which(colnames(test2) == "variable")]<- "year"
colnames(test2)[which(colnames(test2) == "value")]<- "number_of_indiv"
test2$year <- as.numeric(as.character(test2$year))

ggplot2::ggplot(test2[which(test2$run <= 10),], ggplot2::aes(x = year, y = number_of_indiv, group = run, color = run)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() + 
  ggplot2::facet_grid(pop ~ class) + 
  ggplot2::labs(x = "Year") +
  ggplot2::labs(y = "Number of Individuals") +
  ggplot2::ggtitle("Example of 10 runs for basecase parameters. \n Note: when pop totals are calculated, eggs and tadpoles not included because using fall census") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )




# TO show totals at fall census by run

# from code fin dapva::makeResultsSummaryOneIteration
results_total_by_run <- results_basecase_fall %>%
  dplyr::group_by(run) %>%
  dplyr::summarise(dplyr::across(paste(initial_year:(initial_year + yrs - 1)), sum))
results_total_by_run$metric <- "number_of_indiv"


results_summary_for_this_iteration <- results_total_by_run[, c("run",  "metric", paste(initial_year:(initial_year + yrs - 1)))] # Reorganize the results for easier viewing

library(reshape2)
test <- melt(results_summary_for_this_iteration, id.vars=c("run", "metric"))
colnames(test)[which(colnames(test) == "variable")]<- "year"
colnames(test)[which(colnames(test) == "value")]<- "number_of_indiv"
test$year <- as.numeric(as.character(test$year))
# test$run <- as.factor(test$run)

runs <- length(unique(test$run))
test$facet <- rep(1:(runs/10), each = runs/10)

ggplot2::ggplot(test[which(test$run <=10),], ggplot2::aes(x = year, y = number_of_indiv, group = run, color = run)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



############## basecase - Graphs of the one iteration to show the process ######

# Might also be useful to make a function where you can plot the results of a few iterations

# ALso to do: Make graphs of the survival rates with the different threats to show Lea and Rebecca 



test4 <- dapva::makeResultsSummaryOneIteration(results_basecase_fall,
                                               by_pop = 'yes',
                                               initial_year = 1,
                                               yrs = 50,
                                               n_iter = 1,
                                               n_runs_per_iter = length(unique(results_basecase_fall$run)),
                                               alternative = paste0(alternative_details$alt_name_full),
                                               iteration_number = 1,
                                               prob_self_sustain = TRUE,
                                               lambda_over_x_years = 10)
  
  
results_summary_num_indiv <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = test4 ,
                                                                  metric = "mean total number of individuals",
                                                                  initial_year = 1, credible_interval = 0.95)

results_summary_prob_persist <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = test4 ,
                                                                     metric = "probability of persistence",
                                                                     initial_year = 1, credible_interval = 0.95)

results_summary_prob_selfsustaining <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = test4 ,
                                                                            metric = "probability of self-sustaining population",
                                                                            initial_year = 1, credible_interval = 0.95)


(abundance_graph <- dapva::graphResultsSummary(results_summary_num_indiv))

(persistence_graph <- dapva::graphResultsSummary(results_summary_prob_persist))
(persistence_flyingBars <- dapva::graphFlyingBars(results_summary_all_iterations = test4,
                                                  metric = "probability of persistence",
                                                  year = 50,
                                                  credible_interval = 0.95))

(selfsustaining_graph <- dapva::graphResultsSummary(results_summary_prob_selfsustaining))
(selfsustaining_flyingBars <- dapva::graphFlyingBars(results_summary_all_iterations = test4,
                                                     metric = "probability of self-sustaining population",
                                                     year = 50,
                                                     credible_interval = 0.95))

############## Load one alternative to use in the examples for convergence testing ######
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




# Export the graphss
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



############## Plot the survival distributions for multiple threats to illustrate for Lea and Rebecca. #####################

# Visualization code from ?dbeta help file
# dbeta is the density function where the x axis is the survival rate
# pbeta is the cumulative distribution function where x axis is the survival rate and y axis is the prob that survival is less than or qual
# qbeta is the inverse of the cumulative distribution function, where the x axis is the quantile and the y axis is the survival rate

pl.beta <- function(title, a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length.out = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = paste(sprintf("[dpq]beta(x, a=%g, b=%g)", a,b), "\n", title))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}


# BASE CASE
inputs_all <- dapva4nlf::getNLFIdahoFeasinputs()
inputs <- inputs_all[[1]]
parameterByIterTracking_baseCase <- selectNLFIdahoParameterByIterTracking(inputs, base_case = TRUE)
parameterByIterTracking <- parameterByIterTracking_baseCase

# BASE CASE - eggs, no threats
s_eggs_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_eggs_no_threats")])
s_eggs_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_eggs_no_threats")])
s_eggs_no_threats_dist <- dapva::estBetaParams(mean = s_eggs_mean, sd = s_eggs_sd)

pl.beta(s_eggs_no_threats_dist$alpha, 
        s_eggs_no_threats_dist$beta, 
        title = "base case (P50) egg survival, no threats") 

# BASE CASE - eggs with threats
s_pct_reduced_eggs_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_bullfrogs")])
s_pct_reduced_eggs_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_chytrid")])
s_pct_reduced_eggs_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_roads")])

s_eggs_afterThreats_mean <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_roads")])/100)*
  (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_chytrid")])/100)*
  (1-s_pct_reduced_eggs_bullfrogs/100)*
  s_eggs_mean # note quite how it is in the code at the moment since this reduction is applied after EV, but here to illustrate; perhaps should update so do it this way and apply EV after? don't think it will matter

s_eggs_w_threats_dist <- dapva::estBetaParams(mean = s_eggs_afterThreats_mean, sd = s_eggs_sd)

pl.beta(s_eggs_w_threats_dist$alpha, 
        s_eggs_w_threats_dist$beta, 
        title = "base case (P50) egg survival with bullfrogs, chytrid, and roads") 




# BASE CASE - tadpoles, no threats
s_tadpoles_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_tadpoles_no_threats")])
s_tadpoles_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_tadpoles_no_threats")])
s_tadpoles_no_threats_dist <- dapva::estBetaParams(mean = s_tadpoles_mean, sd = s_tadpoles_sd)

pl.beta(s_tadpoles_no_threats_dist$alpha, 
        s_tadpoles_no_threats_dist$beta, 
        title = "base case (P50) tadpole survival, no threats") 

# BASE CASE - tadpoles with threats
s_pct_reduced_tadpoles_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_bullfrogs")])
s_pct_reduced_tadpoles_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_chytrid")])
s_pct_reduced_tadpoles_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_roads")])

s_tadpoles_afterThreats_mean <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_roads")])/100)*
  (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_chytrid")])/100)*
  (1-s_pct_reduced_tadpoles_bullfrogs/100)*
  s_tadpoles_mean # note quite how it is in the code at the moment since this reduction is applied after EV, but here to illustrate; perhaps should update so do it this way and apply EV after? don't think it will matter

s_tadpoles_w_threats_dist <- dapva::estBetaParams(mean = s_tadpoles_afterThreats_mean, sd = s_tadpoles_sd)

pl.beta(s_tadpoles_w_threats_dist$alpha, 
        s_tadpoles_w_threats_dist$beta, 
        title = "base case (P50) tadpole survival with bullfrogs, chytrid, and roads") 



# BASE CASE - yoy, no threats
s_yoy_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_yoy_no_threats")])
s_yoy_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_yoy_no_threats")])
s_yoy_no_threats_dist <- dapva::estBetaParams(mean = s_yoy_mean, sd = s_yoy_sd)

pl.beta(s_yoy_no_threats_dist$alpha, 
        s_yoy_no_threats_dist$beta, 
        title = "base case (P50) yoy survival, no threats") 

# BASE CASE - yoy with threats
s_pct_reduced_yoy_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_bullfrogs")])
s_pct_reduced_yoy_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_chytrid")])
s_pct_reduced_yoy_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_roads")])

s_yoy_afterThreats_mean <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_roads")])/100)*
  (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_chytrid")])/100)*
  (1-s_pct_reduced_yoy_bullfrogs/100)*
  s_yoy_mean # note quite how it is in the code at the moment since this reduction is applied after EV, but here to illustrate; perhaps should update so do it this way and apply EV after? don't think it will matter

s_yoy_w_threats_dist <- dapva::estBetaParams(mean = s_yoy_afterThreats_mean, sd = s_yoy_sd)

pl.beta(s_yoy_w_threats_dist$alpha, 
        s_yoy_w_threats_dist$beta, 
        title = "base case (P50) yoy survival with bullfrogs, chytrid, and roads") 


# BASE CASE - juv, no threats
s_juv_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_juv_no_threats")])
s_juv_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_juv_no_threats")])
s_juv_no_threats_dist <- dapva::estBetaParams(mean = s_juv_mean, sd = s_juv_sd)

pl.beta(s_juv_no_threats_dist$alpha, 
        s_juv_no_threats_dist$beta, 
        title = "base case (P50) juv survival, no threats") 

# BASE CASE - juv with threats
s_pct_reduced_juv_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_bullfrogs")])
s_pct_reduced_juv_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_chytrid")])
s_pct_reduced_juv_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_roads")])

s_juv_afterThreats_mean <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_roads")])/100)*
  (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_chytrid")])/100)*
  (1-s_pct_reduced_juv_bullfrogs/100)*
  s_juv_mean # note quite how it is in the code at the moment since this reduction is applied after EV, but here to illustrate; perhaps should update so do it this way and apply EV after? don't think it will matter

s_juv_w_threats_dist <- dapva::estBetaParams(mean = s_juv_afterThreats_mean, sd = s_juv_sd)

pl.beta(s_juv_w_threats_dist$alpha, 
        s_juv_w_threats_dist$beta, 
        title = "base case (P50) juv survival with bullfrogs, chytrid, and roads") 

# BASE CASE - adult, no threats
s_adult_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_adult_no_threats")])
s_adult_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_adult_no_threats")])
s_adult_no_threats_dist <- dapva::estBetaParams(mean = s_adult_mean, sd = s_adult_sd)

pl.beta(s_adult_no_threats_dist$alpha, 
        s_adult_no_threats_dist$beta, 
        title = "base case (P50) adult survival, no threats") 

# BASE CASE - adult with threats
s_pct_reduced_adult_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_bullfrogs")])
s_pct_reduced_adult_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_chytrid")])
s_pct_reduced_adult_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_roads")])

s_adult_afterThreats_mean <- (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_roads")])/100)*
  (1-as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_chytrid")])/100)*
  (1-s_pct_reduced_adult_bullfrogs/100)*
  s_adult_mean # note quite how it is in the code at the moment since this reduction is applied after EV, but here to illustrate; perhaps should update so do it this way and apply EV after? don't think it will matter

s_adult_w_threats_dist <- dapva::estBetaParams(mean = s_adult_afterThreats_mean, sd = s_adult_sd)

pl.beta(s_adult_w_threats_dist$alpha, 
        s_adult_w_threats_dist$beta, 
        title = "base case (P50) adult survival with bullfrogs, chytrid, and roads") 



# NOTE: funny shape when mean values are too close to 0 or 1, as in tadpole survival
# Explained at https://stats.stackexchange.com/questions/380833/std-dev-should-be-less-than-0-289-help-in-understanding
# One solution might be to put a limit on it so that if the mean survival is a certain level of closesness to 0 or 1 then it just gets assigned the mean
# I think this may be happening in my code anyways but not here where I am plotting it, need to look closer...

