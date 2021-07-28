# This script uses the Rdata files saved as the output from running the NLF Idaho Feasibility PVA
# wrapper script to extract and graph the results for the
# 2021 SDM report.

# Note that my current calgary zoo laptop can not handle the Go Big results file 
# with 10K iterations. Therefore, this script needs to be run on my mac if 
# graphing that set of results.

#---- Clear the environment. ----
rm(list=ls())

#---- Load libraries. ----
library(dapva)
library(dapva4nlf)
library(gridExtra) # for grid.arrange
library(plyr)
library(dplyr)

#---- Specify the location where you saved the Rdata files. ----
# Note: Add a folder in there called 'ForReport' to help organize the outputs.

# Increase computer memory size before loading the RData files
#memory.limit(24000) # for windows computers only
#path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results"# on my work PC
path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/NLF_PVA/Results/"# on my mac
setwd(path_to_results_folder) # on my mac

#---- Specify the number of years you ran the model for/want to see results for. ----
yrs <- 50


#---- Extract the results from the Rdata files saved for each scenario. ----
# One by one, load in the Rdata files and then export the results to csvs

files <-  list.files(path = ".","*.RData", full.names="TRUE")
for (i in 1:length(files)){
  # Load the Rdata file
  load(files[i])
  
  # Export results for this alternative so can investigate one by one if needed 

  # Export the parameters used
  write.csv(parameterByIterTracking, file = paste0("params_", name, version,".csv"),row.names = FALSE)

  # Export the 'by population' results
  write.csv(results_summary_all_iterations_by_pop, file = paste0("results_by_pop_", name, version,".csv"), row.names = FALSE)

  # Summarize the 'by population' results into 'overall' results and export that
  write.csv(results_summary_all_iterations_overall, file = paste0("results_overall_", name, version,".csv"), row.names = FALSE)

  # Sensitivity analysis results
  # REMOVED because not relevant for the ones with fewer iterations, will do the full tornado for Go Big below
  
  # Summarize the population specific results for this scenario/alternative.
  results_summary_prob_persist_by_pop <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_all_iterations_by_pop,
                                                                metric = "probability of persistence",
                                                                initial_year = parameterByIterTracking$initial_year[1])
  
  results_summary_prob_selfsustaining_by_pop <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_all_iterations_by_pop,
                                                                              metric = "probability of self-sustaining population",
                                                                              initial_year = parameterByIterTracking$initial_year[1])
  
  # Graph
  persistence_graph <- dapva::graphResultsSummary(results_summary_prob_persist_by_pop)
  selfsustaining_graph <- dapva::graphResultsSummary(results_summary_prob_selfsustaining_by_pop)

  # Export the graphs
  filename <- paste("graph_per_overTime_by_pop_", name, version, "_iter_", n_iter, ".tiff", sep="")
  tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
  print(persistence_graph)
  dev.off()

  filename <- paste("graph_ss_overTime_by_pop_", name, version, "_iter_", n_iter, ".tiff", sep="")
  tiff(filename, width=12, height=6, units="in",pointsize=8, compression="lzw", bg="white", res=600)
  print(selfsustaining_graph)
  dev.off()
  
  

}

#---- Graph the overall results for all the scenarios/alternatives - explore the results. ----

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
#(abundance_graph <- dapva::graphResultsSummary(results_summary_num_indiv)) # removed because now plots on 0to 1 for persistence, needs to be tweaked for abundance, not important right now for this purpose
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
 results_summary_prob_persist_table <- results_summary_prob_persist[which(results_summary_prob_persist$year == yrs),] # yrs
# Export this probability of persistence results summary table
filename <- paste("ForReport/probPersist_summary_table", version, "_iter_", n_iter, ".csv", sep="")
write.csv(results_summary_prob_persist_table, file = filename)

# Make an output table with the format we want for the report
results_summary_prob_selfsustaining_table <- results_summary_prob_selfsustaining[which(results_summary_prob_selfsustaining$year == yrs),]
# Export this probability of persistence results summary table
filename <- paste("ForReport/probSelfSustain_summary_table", version, "_iter_", n_iter, ".csv", sep="")
write.csv(results_summary_prob_selfsustaining_table, file = filename)


#---- Check on number of runs per iteration and number of iterations. ----

# Of runs per iteration
min(results_all_iter$n_runs_per_iter) #300
max(results_all_iter$n_runs_per_iter) #376

# of iterations
results_all_iter %>%
  group_by(alternative) %>%
  summarize(length(iteration)/3) # divide by 3 because 3 metrics reported for each iteration

# 9999 for Go Big shows that one iteration was skipped during parallel computing
# 400 for the rest means that the full min number of iterations was completed

#---- Check to see which, if any, parameters got replaced because beta shape parameters were not valid. ----

files <-  list.files(path = ".","*.RData", full.names="TRUE")
for (i in 1:length(files)){
  # Load the Rdata file
  load(files[i])
  
  print(paste("Checking for", name))
  
  # Step through manually for each alternative (i) to see clearly
  # If they were replaced, the original paramater draw and the one used in the analysis will differ so the difference !=0
  which((parameterByIterTracking_orig$p_females_lay_eggs_sd_A2 - parameterByIterTracking$p_females_lay_eggs_sd_A2)!= 0)
  which((parameterByIterTracking_orig$p_females_lay_eggs_sd_A3_A4plus - parameterByIterTracking$p_females_lay_eggs_sd_A3_A4plus)!= 0)
  which((parameterByIterTracking_orig$s_sd_eggs_no_threats - parameterByIterTracking$s_sd_eggs_no_threats)!= 0)
  which((parameterByIterTracking_orig$s_sd_tadpoles_no_threats - parameterByIterTracking$s_sd_tadpoles_no_threats)!= 0)
  which((parameterByIterTracking_orig$s_sd_ephWetlands_eggs_no_threats - parameterByIterTracking$s_sd_ephWetlands_eggs_no_threats)!= 0)
  which((parameterByIterTracking_orig$s_sd_ephWetlands_tadpoles_no_threats - parameterByIterTracking$s_sd_ephWetlands_tadpoles_no_threats)!= 0)
  which((parameterByIterTracking_orig$s_sd_yoy_no_threats - parameterByIterTracking$s_sd_yoy_no_threats)!= 0)
  which((parameterByIterTracking_orig$s_sd_juv_no_threats - parameterByIterTracking$s_sd_juv_no_threats)!= 0)
  which((parameterByIterTracking_orig$s_sd_adult_no_threats - parameterByIterTracking$s_sd_adult_no_threats)!= 0)
  
  
}


# go Big with 10K iter - twice for p_females_lay_eggs_sd_A2, not at all for the others
# All other alternatives - did not happen



#---- Clear the environment. ----
rm(list=ls())
yrs <- 50
version <- "_July2021_FINAL"

#---- Make graphs for the report - level of effort, persistence. ----

# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_overall_")
results_all_iter_list <- lapply(temp_iter, read.csv)
results_all_iter <- do.call(rbind, results_all_iter_list)
colnames(results_all_iter)[7:ncol(results_all_iter)] <- 1:50

# Graph over time
# Pull out the results summary
results_summary_prob_persist <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_all_iter,
                                                                     metric = "probability of persistence",
                                                                     initial_year = 1, credible_interval = 0.95)

# Add the Do Nothing scenario, which is 0 since population is currently extirpated with no chance of natural recovery
do_Nothing <- results_summary_prob_persist[1:yrs,] # initalize
do_Nothing$mean <- 0
do_Nothing$median <- 0
do_Nothing$lcl <- 0
do_Nothing$ucl <- 0
do_Nothing$n_iter <- 0
do_Nothing$n_runs_per_iter <- 0
do_Nothing$alternative <- "Do Nothing"
results_summary_prob_persist <- rbind(results_summary_prob_persist, do_Nothing)

# Alternatives of interest
goBig_alt_name  <- "Go Big or Go Home "
mostReal_alt_name  <- "Middle of the Road"
lowEffort_alt_name  <- "Minimum Funding Availabilty / Low Effort"
doNothing_alt_name  <- "Do Nothing"

int <- results_summary_prob_persist[c(which(results_summary_prob_persist$alternative == goBig_alt_name),
                                      which(results_summary_prob_persist$alternative == mostReal_alt_name),
                                      which(results_summary_prob_persist$alternative == lowEffort_alt_name),
                                      which(results_summary_prob_persist$alternative == doNothing_alt_name)),]
int$alternative[which(int$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
int$alternative[which(int$alternative == lowEffort_alt_name)] <- "Minimum Funding / Low Effort" # put it on two lines


int$alternative<- factor(int$alternative, levels=c("Do Nothing", "Minimum Funding / Low Effort",
                                                   "Middle of the Road", "Go Big or Go Home")) # reorder factor levels

(persist_effort_graph1 <- graphResultsSummary(results_summary = int,
                                       overlap = FALSE,
                                       title = 'A)',
                                       x_axis_lab = "Year",
                                       y_axis_lab = "\n Probability of Persistence \n ")) # The extra lines push the title out to the same spot as in panel B)


# Flying Bars

# Add the Do Nothing scenario, which is 0 since population is currently extirpated with no chance of natural recovery
do_Nothing <- results_all_iter[1,] # initalize
do_Nothing$metric <- "probability of persistence"
do_Nothing$alternative <- "Do Nothing"
do_Nothing$n_iter <- 0
do_Nothing$n_runs_per_iter <- 0
do_Nothing[1,7:(7+yrs-1)] <- rep(0, time = yrs)
results_all_iter <- rbind(results_all_iter, do_Nothing, do_Nothing, do_Nothing)


int2 <- results_all_iter[c(which(results_all_iter$alternative == goBig_alt_name),
                           which(results_all_iter$alternative == mostReal_alt_name),
                           which(results_all_iter$alternative == lowEffort_alt_name),
                           which(results_all_iter$alternative == doNothing_alt_name)),]
int2$alternative[which(int2$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
int2$alternative[which(int2$alternative == lowEffort_alt_name)] <- "Minimum Funding /\n Low Effort" # put it on two lines

int2$alternative<- factor(int2$alternative, levels=c("Go Big or Go Home", "Middle of the Road",
                                                    "Minimum Funding /\n Low Effort",
                                                    "Do Nothing" )) # reorder factor levels

(persistence_effort_flyingBars1 <- dapva::graphBoxandViolinPlot(results_summary_all_iterations = int2,
                                            metric = "probability of persistence",
                                            year = yrs,
                                            credible_interval = 0.95,
                                            x_axis_lab = "Probability of Persistence in Year 50",
                                            y_axis_lab = "\n Management Alternative",
                                            # title = 'B)'))
                                            title = 'A)'))


# cumulative distribution function - to look at stochastic dominance
(persistence_effort_CDF1 <- dapva::graphCDF(results_summary_all_iterations = int2,
         metric = "probability of persistence",
         year = yrs,
         x_axis_lab = "Probability of Persistence in Year 50",
         y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
         title = 'C)'))

(persistence_effort_PDF1 <- dapva::graphPDF(results_summary_all_iterations = int2,
                                     metric = "probability of persistence",
                                     year = yrs,
                                     x_axis_lab = "Probability of Persistence in Year 50",
                                     title = 'C)'))

# What if top shows just the box and whisker charts
# Then the next shows the violin plots for just Middle of the Road and GO Big
# Then have the CDFs on the bottom

(persistence_effort_flyingBars1_opt2<- dapva::graphFlyingBars(results_summary_all_iterations = int2,
                                                                metric = "probability of persistence",
                                                                year = yrs,
                                                                credible_interval = 0.95,
                                                                x_axis_lab = "Probability of Persistence in Year 50",
                                                                y_axis_lab = "\n Management Alternative",
                                                                # title = 'B)'))
                                                                title = 'A)'))

# (persistence_effort_violinPlot_opt2 <- dapva::graphViolinPlotSideways(results_summary_all_iterations = int2[which(int2$alternative != "Minimum Funding /\n Low Effort"  &
#                                                                                                                int2$alternative != "Do Nothing"),],
#                                                            metric = "probability of persistence",
#                                                            year = yrs,
#                                                            credible_interval = 0.95,
#                                                            x_axis_lab = "Probability of Persistence in Year 50",
#                                                            y_axis_lab = "\n Management Alternative",
#                                                            title = 'C)'))


# Doing manually so can get the colors to match the ones with all four alternatives
x_axis_lab1 <- ggplot2::labs(y = "Probability of Persistence in Year 50")
y_axis_lab1 <- ggplot2::labs(x = "\n Management Alternative")
title1 <- ggplot2::ggtitle(paste0('C)'))

int2b <- int2[which(int2$alternative != "Minimum Funding /\n Low Effort"  &
                      int2$alternative != "Do Nothing"),]

results_summary_all_iterations_year <- int2b[which(int2b$metric == "probability of persistence"), 
                                                                      c(paste0(50), "alternative", "pop")]
colnames(results_summary_all_iterations_year) <- c("quantity", 
                                                   "alternative", "pop")

persistence_effort_violinPlot_opt2 <- ggplot2::ggplot(results_summary_all_iterations_year, 
                ggplot2::aes(x = alternative, y = quantity, col = alternative)) + 
  ggplot2::geom_violin() + 
  ggplot2::stat_summary(fun = median, colour = "blue", geom = "point", shape = 17, size = 3) + 
  ggplot2::stat_summary(fun = mean, colour = "darkred", geom = "point", shape = 18, size = 4) + 
  ggplot2::scale_y_continuous() + 
  ggplot2::scale_color_hue(direction = 1, h = c(270, 180)) +
  # ggthemes::scale_colour_colorblind() + 
  # ggthemes::scale_fill_colorblind() + 
  x_axis_lab1 + y_axis_lab1 + title1 + 
  ggplot2::coord_flip() + 
  ggplot2::theme_bw() + 
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                                                               panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_blank(), 
                                                               panel.border = ggplot2::element_rect(colour = "black"), 
                                                               text = ggplot2::element_text(size = 12), axis.text.x = ggplot2::element_text(angle = 45, 
                                                                                                                                            hjust = 1), legend.position = "none")



# CDF

int2$alternative <- as.character(int2$alternative)
int2$alternative[which(int2$alternative == "Go Big or Go Home")] <- "Go Big or \n Go Home"  # put it on two lines
int2$alternative[which(int2$alternative == "Middle of the Road")] <- "Middle of \n the Road"  # put it on two lines
int2$alternative <- as.factor(int2$alternative)
int2$alternative<- factor(int2$alternative, levels=c("Go Big or \n Go Home", "Middle of \n the Road",
                                                     "Minimum Funding /\n Low Effort",
                                                     "Do Nothing" )) # reorder factor levels

(persistence_effort_CDF1_opt2 <- dapva::graphCDF(results_summary_all_iterations = int2,
                                     metric = "probability of persistence",
                                     year = yrs,
                                     x_axis_lab = "Probability of Persistence in Year 50",
                                     y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                     title = 'E)'))

#---- Make graphs for the report - level of effort, self-sustaining. ----
# Get a fresh load of the results
# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_overall_")
results_all_iter_list <- lapply(temp_iter, read.csv)
results_all_iter <- do.call(rbind, results_all_iter_list)
colnames(results_all_iter)[7:ncol(results_all_iter)] <- 1:50

# Graph over time
# Pull out the results summary
results_summary_prob_selfsustaining <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_all_iter,
                                                                            metric = "probability of self-sustaining population",
                                                                            initial_year = 1, credible_interval = 0.95)

# Add the Do Nothing scenario, which is 0 since population is currently extirpated with no chance of natural recovery
do_Nothing <- results_summary_prob_selfsustaining[1:yrs,] # initalize
do_Nothing$mean <- 0
do_Nothing$median <- 0
do_Nothing$lcl <- 0
do_Nothing$ucl <- 0
do_Nothing$n_iter <- 0
do_Nothing$n_runs_per_iter <- 0
do_Nothing$alternative <- "Do Nothing"
results_summary_prob_selfsustaining <- rbind(results_summary_prob_selfsustaining, do_Nothing)

# Best Guess vs Status Quo
goBig_alt_name  <- "Go Big or Go Home "
mostReal_alt_name  <- "Middle of the Road"
lowEffort_alt_name  <- "Minimum Funding Availabilty / Low Effort"
doNothing_alt_name  <- "Do Nothing"

int3 <- results_summary_prob_selfsustaining[c(which(results_summary_prob_selfsustaining$alternative == goBig_alt_name),
                                      which(results_summary_prob_selfsustaining$alternative == mostReal_alt_name),
                                      which(results_summary_prob_selfsustaining$alternative == lowEffort_alt_name),
                                      which(results_summary_prob_selfsustaining$alternative == doNothing_alt_name)),]
int3$alternative[which(int3$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
int3$alternative[which(int3$alternative == lowEffort_alt_name)] <- "Minimum Funding / Low Effort" # put it on two lines


int3$alternative <- factor(int3$alternative, levels=c("Do Nothing", "Minimum Funding / Low Effort",
                                                   "Middle of the Road", "Go Big or Go Home")) # reorder factor levels

(selfsustain_effort_graph1 <- graphResultsSummary(results_summary = int3,
                                              overlap = FALSE,
                                              # title = 'A)',
                                              title = 'B)',
                                              x_axis_lab = "Year",
                                              # y_axis_lab = "Probability of a Self-Sustaining Population \n \n ")) # The extra lines push the title out to the same spot as in panel B)
                                              y_axis_lab = "\n Probability of a Self-Sustaining Population \n")) # The extra lines push the title out to the same spot as in panel B)


# Flying Bars

# Add the Do Nothing scenario, which is 0 since population is currently extirpated with no chance of natural recovery
do_Nothing <- results_all_iter[1,] # initalize
do_Nothing$metric <- "probability of self-sustaining population"
do_Nothing$alternative <- "Do Nothing"
do_Nothing$n_iter <- 0
do_Nothing$n_runs_per_iter <- 0
do_Nothing[1,7:(7+yrs-1)] <- rep(0, time = yrs)
results_all_iter <- rbind(results_all_iter, do_Nothing, do_Nothing, do_Nothing)


int4 <- results_all_iter[c(which(results_all_iter$alternative == goBig_alt_name),
                           which(results_all_iter$alternative == mostReal_alt_name),
                           which(results_all_iter$alternative == lowEffort_alt_name),
                           which(results_all_iter$alternative == doNothing_alt_name)),]
int4$alternative[which(int4$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
int4$alternative[which(int4$alternative == lowEffort_alt_name)] <- "Minimum Funding /\n Low Effort" # put it on two lines

int4$alternative <- factor(int4$alternative, levels=c("Go Big or Go Home", "Middle of the Road",
                                                     "Minimum Funding /\n Low Effort",
                                                     "Do Nothing" )) # reorder factor levels

(selfsustain_effort_flyingBars1 <- dapva::graphBoxandViolinPlot(results_summary_all_iterations = int4,
                                                   metric = "probability of self-sustaining population",
                                                   year = yrs,
                                                   credible_interval = 0.95,
                                                   x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                                   y_axis_lab = "\n Management Alternative",
                                                   # title = 'B)'))
                                                   title = 'B)'))

(selfsustain_effort_CDF1 <- graphCDF(results_summary_all_iterations = int4,
                                     metric = "probability of self-sustaining population",
                                     year = yrs,
                                     x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                     y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                     title = 'D)'))

# What if top shows just the box and whisker charts
# Then the next shows the violin plots for just Middle of the Road and GO Big
# Then have the CDFs on the bottom

(selfsustain_effort_flyingBars1_opt2 <- dapva::graphFlyingBars(results_summary_all_iterations = int4,
                                                    metric = "probability of self-sustaining population",
                                                    year = yrs,
                                                    credible_interval = 0.95,
                                                    x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                                    y_axis_lab = "\n Management Alternative",
                                                    # title = 'B)'))
                                                    title = 'B)'))

# (selfsustain_effort_violinPlot_opt2 <- dapva::graphViolinPlotSideways(results_summary_all_iterations = int4[which(int4$alternative != "Minimum Funding /\n Low Effort"  &
#                                                                                                         int4$alternative != "Do Nothing"),],
#                                                           metric = "probability of self-sustaining population",
#                                                           year = yrs,
#                                                           credible_interval = 0.95,
#                                                           x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
#                                                           y_axis_lab = "\n Management Alternative",
#                                                           title = 'D)'))

# Doing manually so can get the colors to match the ones with all four alternatives
x_axis_lab1 <- ggplot2::labs(y = "Probability of a Self-Sustaining Population in Year 50")
y_axis_lab1 <- ggplot2::labs(x = "\n Management Alternative")
title1 <- ggplot2::ggtitle(paste0('D)'))

int4b <- int4[which(int4$alternative != "Minimum Funding /\n Low Effort"  &
                      int4$alternative != "Do Nothing"),]

results_summary_all_iterations_year <- int4b[which(int4b$metric == "probability of self-sustaining population"), 
                                             c(paste0(50), "alternative", "pop")]
colnames(results_summary_all_iterations_year) <- c("quantity", 
                                                   "alternative", "pop")

selfsustain_effort_violinPlot_opt2 <- ggplot2::ggplot(results_summary_all_iterations_year, 
                                                      ggplot2::aes(x = alternative, y = quantity, col = alternative)) + 
  ggplot2::geom_violin() + 
  ggplot2::stat_summary(fun = median, colour = "blue", geom = "point", shape = 17, size = 3) + 
  ggplot2::stat_summary(fun = mean, colour = "darkred", geom = "point", shape = 18, size = 4) + 
  ggplot2::scale_y_continuous() + 
  ggplot2::scale_color_hue(direction = 1, h = c(270, 180)) +
  # ggthemes::scale_colour_colorblind() + 
  # ggthemes::scale_fill_colorblind() + 
  x_axis_lab1 + y_axis_lab1 + title1 + 
  ggplot2::coord_flip() + 
  ggplot2::theme_bw() + 
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_blank(), 
                 panel.border = ggplot2::element_rect(colour = "black"), 
                 text = ggplot2::element_text(size = 12), axis.text.x = ggplot2::element_text(angle = 45, 
                                                                                              hjust = 1), legend.position = "none")



# CDF

int4$alternative <- as.character(int4$alternative)
int4$alternative[which(int4$alternative == "Go Big or Go Home")] <- "Go Big or \n Go Home"  # put it on two lines
int4$alternative[which(int4$alternative == "Middle of the Road")] <- "Middle of \n the Road"  # put it on two lines
int4$alternative <- as.factor(int4$alternative)
int4$alternative<- factor(int4$alternative, levels=c("Go Big or \n Go Home", "Middle of \n the Road",
                                                     "Minimum Funding /\n Low Effort",
                                                     "Do Nothing" )) # reorder factor levels

(selfsustain_effort_CDF1_opt2 <- dapva::graphCDF(results_summary_all_iterations = int4,
                                     metric = "probability of self-sustaining population",
                                     year = yrs,
                                     x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                     y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                     title = 'F)'))

#---- Make graphs for the report - level of effort, graphs for export. ----

filename <- paste("ForReport/graph_effort_time", version,".tiff", sep="")
tiff(filename, width=12, height=6, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
    # restoreConsole=TRUE)
grid.arrange(persist_effort_graph1,  selfsustain_effort_graph1,
             ncol = 2, nrow = 1)
dev.off()

# filename <- paste("ForReport/graph_effort_year50", version,".tiff", sep="")
# tiff(filename, width=12, height=8, units="in",
#      pointsize=8, compression="lzw", bg="white", res=600)
#     # restoreConsole=TRUE)
# grid.arrange(persistence_effort_flyingBars1, selfsustain_effort_flyingBars1,
#              persistence_effort_CDF1, selfsustain_effort_CDF1,
#              ncol = 2, nrow = 2)
# dev.off()

filename <- paste("ForReport/graph_effort_year50_option2", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
grid.arrange(persistence_effort_flyingBars1_opt2, selfsustain_effort_flyingBars1_opt2,
             persistence_effort_violinPlot_opt2, selfsustain_effort_violinPlot_opt2,
             persistence_effort_CDF1_opt2, selfsustain_effort_CDF1_opt2,
             ncol = 2, nrow = 3)
dev.off()


#---- Clear the environment. ----
rm(list=ls())
yrs <- 50
version <- "_July2021_FINAL"

#---- Make graphs for the report - variations on Go Big, persistence. ----

# Get a fresh load of the results
# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_overall_")
results_all_iter_list <- lapply(temp_iter, read.csv)
results_all_iter <- do.call(rbind, results_all_iter_list)
colnames(results_all_iter)[7:ncol(results_all_iter)] <- 1:50

# Graph over time
# Pull out the results summary
results_summary_prob_persist <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_all_iter,
                                                                     metric = "probability of persistence",
                                                                     initial_year = 1, credible_interval = 0.95)


# PUll out the alternatives of interest
goBig_alt_name  <- "Go Big or Go Home "
oneWetland_alt_name  <- "Try Hard at One Wetland"
fewTadpoles_alt_name  <- "Try Hard but Few Tadpoles"
noBFM_alt_name  <- "Try Hard but No Bullfrog Management"
short_alt_name  <- "Try Hard but Short"
noHabRest_alt_name  <- "Try Hard but No Habitat Restoration" # still missing this one

int5 <- results_summary_prob_persist[c(which(results_summary_prob_persist$alternative == goBig_alt_name),
                                      which(results_summary_prob_persist$alternative == oneWetland_alt_name),
                                      which(results_summary_prob_persist$alternative == fewTadpoles_alt_name),
                                      which(results_summary_prob_persist$alternative == noBFM_alt_name),
                                      which(results_summary_prob_persist$alternative == noHabRest_alt_name),
                                      which(results_summary_prob_persist$alternative == short_alt_name)),]
int5$alternative[which(int5$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space


int5$alternative<- factor(int5$alternative, levels=c("Go Big or Go Home", "Try Hard at One Wetland",
                                                     "Try Hard but No Habitat Restoration",
                                                     "Try Hard but Short",
                                                     "Try Hard but Few Tadpoles",
                                                   "Try Hard but No Bullfrog Management")) # reorder factor levels

(persist_goBigVar_graph1 <- graphResultsSummary(results_summary = int5,
                                              overlap = FALSE,
                                              title = 'A)',
                                              x_axis_lab = "Year",
                                              y_axis_lab = "\n Probability of Persistence \n ")) # The extra lines push the title out to the same spot as in panel B)


# Flying Bars

int6 <- results_all_iter[c(which(results_all_iter$alternative == goBig_alt_name),
                           which(results_all_iter$alternative == oneWetland_alt_name),
                           which(results_all_iter$alternative == fewTadpoles_alt_name),
                           which(results_all_iter$alternative == noBFM_alt_name),
                           which(results_all_iter$alternative == noHabRest_alt_name),
                           which(results_all_iter$alternative == short_alt_name)),]
int6$alternative[which(int6$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
int6$alternative[which(int6$alternative == noBFM_alt_name)] <- "Try Hard but No \n Bullfrog Management" # put it on two lines
int6$alternative[which(int6$alternative == noHabRest_alt_name)] <- "Try Hard but No \n Habitat Restoration" # put it on two lines
int6$alternative[which(int6$alternative == fewTadpoles_alt_name)] <- "Try Hard but Few \n Tadpoles" # put it on two lines

int6$alternative<- factor(int6$alternative, levels=c("Try Hard but No \n Bullfrog Management",
                                                     "Try Hard but Few \n Tadpoles", 
                                                     "Try Hard but Short",
                                                     "Try Hard but No \n Habitat Restoration",
                                                     "Try Hard at One Wetland",
                                                     "Go Big or Go Home")) # reorder factor levels


(persistence_goBigVar_flyingBars1 <- dapva::graphBoxandViolinPlot(results_summary_all_iterations = int6,
                                                   metric = "probability of persistence",
                                                   year = yrs,
                                                   credible_interval = 0.95,
                                                   x_axis_lab = "Probability of Persistence in Year 50",
                                                   y_axis_lab = "\n Management Alternative",
                                                   # title = 'B)'))
                                                   title = 'A)'))

# cumulative distribution function - to look at stochastic dominance
(persistence_goBigVar_CDF1 <- graphCDF(results_summary_all_iterations = int6,
                                     metric = "probability of persistence",
                                     year = yrs,
                                     x_axis_lab = "Probability of Persistence in Year 50",
                                     y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                     title = 'C)'))

(persistence_goBigVar_PDF1 <- graphPDF(results_summary_all_iterations = int6,
                                     metric = "probability of persistence",
                                     year = yrs,
                                     x_axis_lab = "Probability of Persistence in Year 50",
                                     title = 'C)'))



# What if top shows just the box and whisker charts
# Then the next shows the violin plots for just two leading ones - Go Big and One Wetland
# Then have the CDFs on the bottom

(persistence_goBigVar_flyingBars1_opt2<- dapva::graphFlyingBars(results_summary_all_iterations = int6,
                                                       metric = "probability of persistence",
                                                       year = yrs,
                                                       credible_interval = 0.95,
                                                       x_axis_lab = "Probability of Persistence in Year 50",
                                                       y_axis_lab = "\n Management Alternative",
                                                       # title = 'B)'))
                                                       title = 'A)'))

# (persistence_goBigVar_violinPlot_opt2 <- dapva::graphViolinPlotSideways(results_summary_all_iterations = int6,
#                                                                metric = "probability of persistence",
#                                                                year = yrs,
#                                                                credible_interval = 0.95,
#                                                                x_axis_lab = "Probability of Persistence in Year 50",
#                                                                y_axis_lab = "\n Management Alternative",
#                                                                title = 'C)'))

# Doing manually so can get the colors to match the ones with all six alternatives
x_axis_lab1 <- ggplot2::labs(y = "Probability of Persistence in Year 50")
y_axis_lab1 <- ggplot2::labs(x = "\n Management Alternative")
title1 <- ggplot2::ggtitle(paste0('C)'))

int6b <- int6[which(int6$alternative == "Go Big or Go Home"  |
                      int6$alternative == "Try Hard at One Wetland"),]

results_summary_all_iterations_year <- int6b[which(int6b$metric == "probability of persistence"), 
                                             c(paste0(50), "alternative", "pop")]
colnames(results_summary_all_iterations_year) <- c("quantity", 
                                                   "alternative", "pop")

persistence_goBigVar_violinPlot_opt2 <- ggplot2::ggplot(results_summary_all_iterations_year, 
                                                      ggplot2::aes(x = alternative, y = quantity, col = alternative)) + 
  ggplot2::geom_violin() + 
  ggplot2::stat_summary(fun = median, colour = "blue", geom = "point", shape = 17, size = 3) + 
  ggplot2::stat_summary(fun = mean, colour = "darkred", geom = "point", shape = 18, size = 4) + 
  ggplot2::scale_y_continuous() + 
  ggplot2::scale_color_hue(direction = 1, h = c(60, 0)) +
  # ggthemes::scale_colour_colorblind() + 
  # ggthemes::scale_fill_colorblind() + 
  x_axis_lab1 + y_axis_lab1 + title1 + 
  ggplot2::coord_flip() + 
  ggplot2::theme_bw() + 
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_blank(), 
                 panel.border = ggplot2::element_rect(colour = "black"), 
                 text = ggplot2::element_text(size = 12), axis.text.x = ggplot2::element_text(angle = 45, 
                                                                                              hjust = 1), legend.position = "none")

persistence_goBigVar_violinPlot_opt2 

# CDFs

int6$alternative <- as.character(int6$alternative)
int6$alternative[which(int6$alternative == "Go Big or Go Home")] <- "Go Big or \n Go Home"  # put on two lines
int6$alternative[which(int6$alternative == "Try Hard at One Wetland")] <- "Try Hard at \n One Wetland"  # put on two lines
int6$alternative[which(int6$alternative == "Try Hard but No \n Bullfrog Management")] <- "Try Hard but No \n Bullfrog Manage."  # abbreviate management
int6$alternative <- as.factor(int6$alternative)
int6$alternative<- factor(int6$alternative, levels=c("Try Hard but No \n Bullfrog Manage.",
                                                     "Try Hard but Few \n Tadpoles", 
                                                     "Try Hard but Short",
                                                     "Try Hard but No \n Habitat Restoration",
                                                     "Try Hard at \n One Wetland",
                                                     "Go Big or \n Go Home")) # reorder factor levels

(persistence_goBigVar_CDF1_opt2 <- dapva::graphCDF(results_summary_all_iterations = int6,
                                          metric = "probability of persistence",
                                          year = yrs,
                                          x_axis_lab = "Probability of Persistence in Year 50",
                                          y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                          title = 'E)'))


#---- Make graphs for the report - variations on Go Big, self-sustaining. ----
# Get a fresh load of the results
# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_overall_")
results_all_iter_list <- lapply(temp_iter, read.csv)
results_all_iter <- do.call(rbind, results_all_iter_list)
colnames(results_all_iter)[7:ncol(results_all_iter)] <- 1:50

# Graph over time
# Pull out the results summary
results_summary_prob_selfsustaining <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_all_iter,
                                                                            metric = "probability of self-sustaining population",
                                                                            initial_year = 1, credible_interval = 0.95)

# PUll out the alternatives of interest
goBig_alt_name  <- "Go Big or Go Home "
oneWetland_alt_name  <- "Try Hard at One Wetland"
fewTadpoles_alt_name  <- "Try Hard but Few Tadpoles"
noBFM_alt_name  <- "Try Hard but No Bullfrog Management"
short_alt_name  <- "Try Hard but Short"
noHabRest_alt_name  <- "Try Hard but No Habitat Restoration" 

int7 <- results_summary_prob_selfsustaining[c(which(results_summary_prob_selfsustaining$alternative == goBig_alt_name),
                                              which(results_summary_prob_selfsustaining$alternative == oneWetland_alt_name),
                                              which(results_summary_prob_selfsustaining$alternative == fewTadpoles_alt_name),
                                              which(results_summary_prob_selfsustaining$alternative == noBFM_alt_name),
                                              which(results_summary_prob_selfsustaining$alternative == noHabRest_alt_name),
                                              which(results_summary_prob_selfsustaining$alternative == short_alt_name)),]

int7$alternative[which(int7$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space


int7$alternative<- factor(int7$alternative, levels=c("Go Big or Go Home", "Try Hard at One Wetland",
                                                     "Try Hard but No Habitat Restoration",
                                                     "Try Hard but Short",
                                                     "Try Hard but Few Tadpoles",
                                                     "Try Hard but No Bullfrog Management")) # reorder factor levels

(selfsustain_goBigVar_graph1 <- graphResultsSummary(results_summary = int7,
                                                  overlap = FALSE,
                                                  title = 'B)',
                                                  x_axis_lab = "Year",
                                                  y_axis_lab = "\n Probability of a Self-Sustaining Population \n")) # The extra lines push the title out to the same spot as in panel B)

# Flying Bars

int8 <- results_all_iter[c(which(results_all_iter$alternative == goBig_alt_name),
                           which(results_all_iter$alternative == oneWetland_alt_name),
                           which(results_all_iter$alternative == fewTadpoles_alt_name),
                           which(results_all_iter$alternative == noBFM_alt_name),
                           which(results_all_iter$alternative == noHabRest_alt_name),
                           which(results_all_iter$alternative == short_alt_name)),]
int8$alternative[which(int8$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
int8$alternative[which(int8$alternative == noBFM_alt_name)] <- "Try Hard but No \n Bullfrog Management" # put it on two lines
int8$alternative[which(int8$alternative == noHabRest_alt_name)] <- "Try Hard but No \n Habitat Restoration" # put it on two lines
int8$alternative[which(int8$alternative == fewTadpoles_alt_name)] <- "Try Hard but Few \n Tadpoles" # put it on two lines

int8$alternative<- factor(int8$alternative, levels=c("Try Hard but No \n Bullfrog Management",
                                                     "Try Hard but Few \n Tadpoles", 
                                                     "Try Hard but Short",
                                                     "Try Hard but No \n Habitat Restoration",
                                                     "Try Hard at One Wetland",
                                                     "Go Big or Go Home")) # reorder factor levels

(selfsustain_goBigVar_flyingBars1 <- dapva::graphBoxandViolinPlot(results_summary_all_iterations = int8,
                                                   metric = "probability of self-sustaining population",
                                                   year = yrs,
                                                   credible_interval = 0.95,
                                                   x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                                   y_axis_lab = "\n Management Alternative",
                                                   # title = 'B)'))
                                                   title = 'B)'))

(selfsustain_goBigVar_CDF1 <- graphCDF(results_summary_all_iterations = int8,
                                     metric = "probability of self-sustaining population",
                                     year = yrs,
                                     x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                     y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                     title = 'D)'))

# What if top shows just the box and whisker charts
# Then the next shows the violin plots for just the two leading alternatives
# Then have the CDFs on the bottom


(selfsustain_goBigVar_flyingBars1_opt2 <- dapva::graphFlyingBars(results_summary_all_iterations = int8,
                                                        metric = "probability of self-sustaining population",
                                                        year = yrs,
                                                        credible_interval = 0.95,
                                                        x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                                        y_axis_lab = "\n Management Alternative",
                                                        # title = 'B)'))
                                                        title = 'B)'))

# (selfsustain_goBigVar_violinPlot_opt2 <- dapva::graphViolinPlotSideways(results_summary_all_iterations = int8,
#                                                                metric = "probability of self-sustaining population",
#                                                                year = yrs,
#                                                                credible_interval = 0.95,
#                                                                x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
#                                                                y_axis_lab = "\n Management Alternative",
#                                                                title = 'D)'))

# Doing manually so can get the colors to match the ones with all six alternatives
x_axis_lab1 <- ggplot2::labs(y = "Probability of a Self-Sustaining Population in Year 50")
y_axis_lab1 <- ggplot2::labs(x = "\n Management Alternative")
title1 <- ggplot2::ggtitle(paste0('C)'))

int8b <- int8[which(int8$alternative == "Go Big or Go Home"  |
                      int8$alternative == "Try Hard at One Wetland"),]

results_summary_all_iterations_year <- int8b[which(int8b$metric == "probability of self-sustaining population"), 
                                             c(paste0(50), "alternative", "pop")]
colnames(results_summary_all_iterations_year) <- c("quantity", 
                                                   "alternative", "pop")

selfsustain_goBigVar_violinPlot_opt2 <- ggplot2::ggplot(results_summary_all_iterations_year, 
                                                        ggplot2::aes(x = alternative, y = quantity, col = alternative)) + 
  ggplot2::geom_violin() + 
  ggplot2::stat_summary(fun = median, colour = "blue", geom = "point", shape = 17, size = 3) + 
  ggplot2::stat_summary(fun = mean, colour = "darkred", geom = "point", shape = 18, size = 4) + 
  ggplot2::scale_y_continuous() + 
  ggplot2::scale_color_hue(direction = 1, h = c(60, 0)) +
  # ggthemes::scale_colour_colorblind() + 
  # ggthemes::scale_fill_colorblind() + 
  x_axis_lab1 + y_axis_lab1 + title1 + 
  ggplot2::coord_flip() + 
  ggplot2::theme_bw() + 
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), strip.background = ggplot2::element_blank(), 
                 panel.border = ggplot2::element_rect(colour = "black"), 
                 text = ggplot2::element_text(size = 12), axis.text.x = ggplot2::element_text(angle = 45, 
                                                                                              hjust = 1), legend.position = "none")


selfsustain_goBigVar_violinPlot_opt2

# CDFs

int8$alternative <- as.character(int8$alternative)
int8$alternative[which(int8$alternative == "Go Big or Go Home")] <- "Go Big or \n Go Home"  # put on two lines
int8$alternative[which(int8$alternative == "Try Hard at One Wetland")] <- "Try Hard at \n One Wetland"  # put on two lines
int8$alternative[which(int8$alternative == "Try Hard but No \n Bullfrog Management")] <- "Try Hard but No \n Bullfrog Manage."  # abbreviate management
int8$alternative <- as.factor(int8$alternative)
int8$alternative<- factor(int8$alternative, levels=c("Try Hard but No \n Bullfrog Manage.",
                                                     "Try Hard but Few \n Tadpoles", 
                                                     "Try Hard but Short",
                                                     "Try Hard but No \n Habitat Restoration",
                                                     "Try Hard at \n One Wetland",
                                                     "Go Big or \n Go Home")) # reorder factor levels

(selfsustain_goBigVar_CDF1_opt2 <- dapva::graphCDF(results_summary_all_iterations = int8,
                                          metric = "probability of self-sustaining population",
                                          year = yrs,
                                          x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                          y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                          title = 'F)'))


#---- Make graphs for the report - variations on Go Big, panel for export. ----

filename <- paste("ForReport/graph_goBigVar_time", version,".tiff", sep="")
tiff(filename, width=12, height=6, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
grid.arrange(persist_goBigVar_graph1,  selfsustain_goBigVar_graph1,
             ncol = 2, nrow = 1)
dev.off()


# filename <- paste("ForReport/graph_goBigVar_year50", version,".tiff", sep="")
# tiff(filename, width=12, height=8, units="in",
#      pointsize=8, compression="lzw", bg="white", res=600)
#     # restoreConsole=TRUE)
# grid.arrange(persistence_goBigVar_flyingBars1, selfsustain_goBigVar_flyingBars1,
#              persistence_goBigVar_CDF1, selfsustain_goBigVar_CDF1,
#              ncol = 2, nrow = 2)
# dev.off()


filename <- paste("ForReport/graph_goBigVar_year50_option2", version,".tiff", sep="")
tiff(filename, width=12, height=10, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     # restoreConsole=TRUE)
grid.arrange(persistence_goBigVar_flyingBars1_opt2, selfsustain_goBigVar_flyingBars1_opt2,
             persistence_goBigVar_violinPlot_opt2, selfsustain_goBigVar_violinPlot_opt2,
             persistence_goBigVar_CDF1_opt2, selfsustain_goBigVar_CDF1_opt2,
             ncol = 2, nrow = 3)
dev.off()



#---- Clear the environment. ----
rm(list=ls())
yrs <- 50
version <- "_July2021_FINAL"

#---- Make graphs for the report - hypothetical scenarios, persistence. ----
# Now move in the hypothetical scenarios results and move out the others
# Get a fresh load of the results
# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_overall_")
results_all_iter_list <- lapply(temp_iter, read.csv)
results_all_iter <- do.call(rbind, results_all_iter_list)
colnames(results_all_iter)[7:ncol(results_all_iter)] <- 1:50

# Rename testing extreme releases B to just Extreme Releases
results_all_iter$alternative[which(results_all_iter$alternative == "Testing Extreme Releases B")] <- "Testing Extreme Releases"

# Graph over time
# Pull out the results summary
results_summary_prob_persist <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_all_iter,
                                                                     metric = "probability of persistence",
                                                                     initial_year = 1, credible_interval = 0.95)


# Pull out the alternatives of interest
existingPop_alt_name  <- "Testing Existing Population"
# extremeRelA_alt_name  <- "Testing Extreme Releases A"
extremeRelB_alt_name  <- "Testing Extreme Releases"

int5b <- results_summary_prob_persist[c(which(results_summary_prob_persist$alternative == existingPop_alt_name),
                                       # which(results_summary_prob_persist$alternative == extremeRelA_alt_name),
                                       which(results_summary_prob_persist$alternative == extremeRelB_alt_name)),]


int5b$alternative <- factor(int5b$alternative, levels=c("Testing Existing Population",
                                                     "Testing Extreme Releases")) # reorder factor levels

(persist_hypotheticals_graph1 <- graphResultsSummary(results_summary = int5b,
                                                overlap = FALSE,
                                                title = 'A)',
                                                x_axis_lab = "Year",
                                                y_axis_lab = "\n Probability of Persistence \n ")) # The extra lines push the title out to the same spot as in panel B)

# Flying Bars

int6b <- results_all_iter[c(which(results_all_iter$alternative == existingPop_alt_name),
                            # which(results_all_iter$alternative == extremeRelA_alt_name), # remove for report, B is enough
                            which(results_all_iter$alternative == extremeRelB_alt_name)),]

int6b$alternative<- factor(int6b$alternative, levels=c("Testing Extreme Releases",
                                                     # "Testing Extreme Releases A", 
                                                     "Testing Existing Population")) # reorder factor levels

(persistence_hypotheticals_flyingBars1 <- dapva::graphBoxandViolinPlot(results_summary_all_iterations = int6b,
                                                     metric = "probability of persistence",
                                                     year = yrs,
                                                     credible_interval = 0.95,
                                                     x_axis_lab = "Probability of Persistence in Year 50",
                                                     y_axis_lab = "\n Management Alternative",
                                                     # title = 'B)'))
                                                     title = 'A)'))

(persistence_hypotheticals_CDF1 <- graphCDF(results_summary_all_iterations = int6b,
                                       metric = "probability of persistence",
                                       year = yrs,
                                       x_axis_lab = "Probability of Persistence in Year 50",
                                       y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                       title = 'C)'))

# What if top shows just the box and whisker charts
# Then the next shows the violin plots for just Middle of the Road and GO Big
# Then have the CDFs on the bottom

(persistence_hypotheticals_flyingBars1_opt2<- dapva::graphFlyingBars(results_summary_all_iterations = int6b,
                                                         metric = "probability of persistence",
                                                         year = yrs,
                                                         credible_interval = 0.95,
                                                         x_axis_lab = "Probability of Persistence in Year 50",
                                                         y_axis_lab = "\n Management Alternative",
                                                         # title = 'B)'))
                                                         title = 'A)'))

(persistence_hypotheticals_violinPlot_opt2 <- dapva::graphViolinPlotSideways(results_summary_all_iterations = int6b,
                                                                 metric = "probability of persistence",
                                                                 year = yrs,
                                                                 credible_interval = 0.95,
                                                                 x_axis_lab = "Probability of Persistence in Year 50",
                                                                 y_axis_lab = "\n Management Alternative",
                                                                 title = 'C)'))

(persistence_hypotheticals_CDF1_opt2 <- dapva::graphCDF(results_summary_all_iterations = int6b,
                                            metric = "probability of persistence",
                                            year = yrs,
                                            x_axis_lab = "Probability of Persistence in Year 50",
                                            y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                            title = 'E)'))


#---- Make graphs for the report - hypothetical scenarios, self-sustaining. ----
# Get a fresh load of the results
# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_overall_")
results_all_iter_list <- lapply(temp_iter, read.csv)
results_all_iter <- do.call(rbind, results_all_iter_list)
colnames(results_all_iter)[7:ncol(results_all_iter)] <- 1:50

# Rename testing extreme releases B to just Extreme Releases
results_all_iter$alternative[which(results_all_iter$alternative == "Testing Extreme Releases B")] <- "Testing Extreme Releases"

# Graph over time
# Pull out the results summary
results_summary_prob_selfsustaining <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_all_iter,
                                                                            metric = "probability of self-sustaining population",
                                                                            initial_year = 1, credible_interval = 0.95)

# PUll out the alternatives of interest
existingPop_alt_name  <- "Testing Existing Population"
# extremeRelA_alt_name  <- "Testing Extreme Releases A"
extremeRelB_alt_name  <- "Testing Extreme Releases"

int7b <- results_summary_prob_selfsustaining[c(which(results_summary_prob_selfsustaining$alternative == existingPop_alt_name),
                                              # which(results_summary_prob_selfsustaining$alternative == extremeRelA_alt_name),
                                              which(results_summary_prob_selfsustaining$alternative == extremeRelB_alt_name)),]

int7b$alternative<- factor(int7b$alternative, levels=c("Testing Existing Population",
                                                      # "Testing Extreme Releases A",
                                                      "Testing Extreme Releases")) # reorder factor levels

(selfsustain_hypotheticals_graph1 <- graphResultsSummary(results_summary = int7b,
                                                    overlap = FALSE,
                                                    # title = 'A)',
                                                    title = 'B)',
                                                    x_axis_lab = "Year",
                                                    y_axis_lab = "\n Probability of a Self-Sustaining Population \n")) # The extra lines push the title out to the same spot as in panel B)

# Flying Bars

int8b <- results_all_iter[c(which(results_all_iter$alternative == existingPop_alt_name),
                           # which(results_all_iter$alternative == extremeRelA_alt_name),
                           which(results_all_iter$alternative == extremeRelB_alt_name)),]

int8b$alternative<- factor(int8b$alternative, levels=c("Testing Extreme Releases",
                                                      # "Testing Extreme Releases A", 
                                                      "Testing Existing Population")) # reorder factor levels

(selfsustain_hypotheticals_flyingBars1 <- dapva::graphBoxandViolinPlot(results_summary_all_iterations = int8b,
                                                     metric = "probability of self-sustaining population",
                                                     year = yrs,
                                                     credible_interval = 0.95,
                                                     x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                                     y_axis_lab = "\n Management Alternative",
                                                     title = 'B)'))

(selfsustain_hypotheticals_CDF1 <- graphCDF(results_summary_all_iterations = int8b,
                                       metric = "probability of self-sustaining population",
                                       year = yrs,
                                       x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                       y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                       title = 'D)'))


# What if top shows just the box and whisker charts
# Then the next shows the violin plots for just Middle of the Road and GO Big
# Then have the CDFs on the bottom


(selfsustain_hypotheticals_flyingBars1_opt2 <- dapva::graphFlyingBars(results_summary_all_iterations = int8b,
                                                          metric = "probability of self-sustaining population",
                                                          year = yrs,
                                                          credible_interval = 0.95,
                                                          x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                                          y_axis_lab = "\n Management Alternative",
                                                          # title = 'B)'))
                                                          title = 'B)'))

(selfsustain_hypotheticals_violinPlot_opt2 <- dapva::graphViolinPlotSideways(results_summary_all_iterations = int8b,
                                                                 metric = "probability of self-sustaining population",
                                                                 year = yrs,
                                                                 credible_interval = 0.95,
                                                                 x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                                                 y_axis_lab = "\n Management Alternative",
                                                                 title = 'D)'))

(selfsustain_hypotheticals_CDF1_opt2 <- dapva::graphCDF(results_summary_all_iterations = int8b,
                                            metric = "probability of self-sustaining population",
                                            year = yrs,
                                            x_axis_lab = "Probability of a Self-Sustaining Population in Year 50",
                                            y_axis_lab = "\n \n \n \n \n Cumulative Probability \n",
                                            title = 'F)'))



#---- Make graphs for the report - hypothetical scenarios, panel for export. ----

filename <- paste("ForReport/graph_hypotheticals_time", version,".tiff", sep="")
tiff(filename, width=12, height=4, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
grid.arrange(persist_hypotheticals_graph1,  selfsustain_hypotheticals_graph1,
             ncol = 2, nrow = 1)
dev.off()


# filename <- paste("ForReport/graph_hypotheticals_year50", version,".tiff", sep="")
# tiff(filename, width=12, height=6, units="in",
#      pointsize=8, compression="lzw", bg="white", res=600)
#      #restoreConsole=TRUE)
# grid.arrange(persistence_hypotheticals_flyingBars1, selfsustain_hypotheticals_flyingBars1,
#              persistence_hypotheticals_CDF1, selfsustain_hypotheticals_CDF1,
#              ncol = 2, nrow = 2)
# dev.off()

filename <- paste("ForReport/graph_hypotheticals_year50_option2", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
grid.arrange(persistence_hypotheticals_flyingBars1_opt2, selfsustain_hypotheticals_flyingBars1_opt2,
             persistence_hypotheticals_violinPlot_opt2, selfsustain_hypotheticals_violinPlot_opt2,
             persistence_hypotheticals_CDF1_opt2, selfsustain_hypotheticals_CDF1_opt2,
             ncol = 2, nrow = 3)
dev.off()




#---- Load Go Big results and use that for the remaining. ----
# clear workspace
rm(list=ls())
yrs <- 50
version <- "_July2021_FINAL"

# Load the final Go Big results
file_goBig <-  list.files(path = ".","*goBig_vFinalJuly2021_10Kiter.RData", full.names="TRUE")
load(file_goBig)

#---- Make graphs for the report -goBig tornados, panel for export. ----

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
# Draw the associated tornado

tornado_persist_top10 <- dapva::drawTornado(paramSens = paramSens_persist,
                                            metric = "Probability of persistence",
                                            year = 50,
                                            title = "A)", breaks = 0.05,
                                            num_bars_to_show = 10)

tornado_selfsustain_top10 <- dapva::drawTornado(paramSens = paramSens_selfsustain,
                                                metric = "Probability of a self-sustaining population",
                                                year = 50,
                                                title = "B)", breaks = 0.05,
                                                num_bars_to_show = 10)

# Export the tornado diagrams - main report
filename <- paste("ForReport/tornados_top10_goBig", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
grid.arrange(tornado_persist_top10,  tornado_selfsustain_top10,
             ncol = 1, nrow = 2)
dev.off()

# Export the sensitivity csvs, can use to describe/interpret the rest of the tornado
write.csv(paramSens_persist[[1]], file = paste0("ForReport/sens_persist_", name, version,".csv"),row.names = FALSE)
write.csv(paramSens_persist[[1]], file = paste0("ForReport/sens_selfsustain_", name, version,".csv"),row.names = FALSE)

# For context/easy reference, also export the full tornados

tornado_persist_full <- dapva::drawTornado(paramSens = paramSens_persist,
                                            metric = "Probability of persistence",
                                            year = 50,
                                            title = "", breaks = 0.05,
                                            num_bars_to_show = "all")

filename <- paste("ForReport/tornado_full_goBig_persist", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
tornado_persist_full
dev.off()

tornado_persist_selfsust <- dapva::drawTornado(paramSens = paramSens_persist,
                                           metric = "Probability of a self-sustaining population",
                                           year = 50,
                                           title = "", breaks = 0.05,
                                           num_bars_to_show = "all")

filename <- paste("ForReport/tornado_full_goBig_selfsust", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
tornado_persist_selfsust
dev.off()

#---- Explore yoy and tadpole survival vs prob of persistence - plot points. ----
# Correct for potential for parallel computing to skip iterations
iteration_numbers <- unique(results_all_this_alt$iteration) # a small number might have been discarded due to an error in the parallal processing

test2 <- cbind(parameterByIterTracking_this_alt_clean[iteration_numbers, c("s_mean_eggs_no_threats",
                                                                           "s_mean_tadpoles_no_threats",
                                                                           "s_mean_yoy_no_threats",
                                                                           "s_mean_juv_no_threats",
                                                                           "bullfrogMgmt_effective")],
              results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])

colnames(test2) <- c("survival_eggs", "survival_tadpoles","survival_yoy", "survival_juv", "bullfrogMgmt_effective", "prob_persist")

best_guess_input_eggs <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_eggs_no_threats")]))
min_eggs <- as.numeric(as.character(min(test2$survival_eggs)))
max_eggs <- as.numeric(as.character(max(test2$survival_eggs)))
P10_input_eggs <- as.numeric(as.character(quantile(test2$survival_eggs, 0.1)))
P90_input_eggs <- as.numeric(as.character(quantile(test2$survival_eggs, 0.9)))

best_guess_input_tad <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_tadpoles_no_threats")]))
min_tad <- as.numeric(as.character(min(test2$survival_tadpoles)))
max_tad <- as.numeric(as.character(max(test2$survival_tadpoles)))
P10_input_tad <- as.numeric(as.character(quantile(test2$survival_tadpoles, 0.1)))
P90_input_tad <- as.numeric(as.character(quantile(test2$survival_tadpoles, 0.9)))

best_guess_input_yoy <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_yoy_no_threats")]))
min_yoy <- as.numeric(as.character(min(test2$survival_yoy)))
max_yoy <- as.numeric(as.character(max(test2$survival_yoy)))
P10_input_yoy <- as.numeric(as.character(quantile(test2$survival_yoy, 0.1)))
P90_input_yoy <- as.numeric(as.character(quantile(test2$survival_yoy, 0.9)))

best_guess_input_juv <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_juv_no_threats")]))
min_juv <- as.numeric(as.character(min(test2$survival_juv)))
max_juv <- as.numeric(as.character(max(test2$survival_juv)))
P10_input_juv <- as.numeric(as.character(quantile(test2$survival_juv, 0.1)))
P90_input_juv <- as.numeric(as.character(quantile(test2$survival_juv, 0.9)))

p_sens_tad_yoy_surv_persist <- ggplot2::ggplot(data = test2, ggplot2::aes(x=survival_tadpoles, y = survival_yoy)) +
  #ggplot2::geom_point(ggplot2::aes(fill = prob_persist, size = prob_persist), shape = 21, alpha = 0.5) +
  ggplot2::geom_point(ggplot2::aes(fill = prob_persist), shape = 21, alpha = 0.5) +
  ggplot2::scale_fill_continuous(type = "viridis") +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 10, barheight = 0.5)) +  #https://ggplot2.tidyverse.org/reference/guide_colourbar.html
  ggplot2::geom_hline(yintercept = best_guess_input_yoy, linetype = "dashed", color = "red") +
  ggplot2::geom_vline(xintercept = best_guess_input_tad, linetype = "dashed", color = "red") +
  ggplot2::xlab("Mean tadpole survival\n (no threats)") +
  ggplot2::ylab( "Mean young of year survival\n (no threats)") + 
  ggplot2::ggtitle( "A)") + 
  ggplot2::labs(size='', fill='Probability of persistence    ')  +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p_sens_tad_yoy_surv_persist

#---- Explore yoy and tadpole survival vs prob of persistence - histogram. ----

prob_persist <- cbind(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])
colnames(prob_persist) <- c("prob_persist")

p_hist_prob_persist <- ggplot2::ggplot(prob_persist, ggplot2::aes(x = prob_persist)) +
  ggplot2::geom_histogram(color="black", fill="grey", binwidth = 0.05) +
  ggplot2::xlab("Probability of persistence") +
  ggplot2::ylab("Number of iterations") + 
  ggplot2::geom_vline(ggplot2::aes(xintercept=mean(prob_persist)),
                      color="red", linetype="dashed", size=1) + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )
p_hist_prob_persist

rows_yoy_tad_surv_above_P50 <- which(test2$survival_tadpoles >= best_guess_input_tad & 
                                       test2$survival_yoy >= best_guess_input_yoy)
test2$group <- "Tadpole or YOY survival below P50"# initalize
test2$group[rows_yoy_tad_surv_above_P50] <- "Tadpole and YOY survival above P50"
test2$group <- as.factor(test2$group)

mu <- plyr::ddply(test2, "group", summarise, grp.mean=mean(prob_persist))

p_hist_prob_persist_groups <- ggplot2::ggplot(test2 , ggplot2::aes(x = prob_persist, fill=group)) +
  ggplot2::geom_histogram(position="dodge", binwidth = 0.05, alpha=0.5) + # http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
  ggplot2::scale_color_brewer(palette="Dark2") + 
  ggplot2::scale_fill_brewer(palette="Dark2") + 
  ggplot2::xlab("Probability of persistence") +
  ggplot2::ylab("Number of iterations") + 
  ggplot2::ggtitle( "B)") + 
  ggplot2::labs(color = "", fill = "") + 
  ggplot2::geom_vline(data=mu, ggplot2::aes(xintercept=grp.mean, color=group),
                      linetype= "dashed") + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )
p_hist_prob_persist_groups

#---- Export pannel graph for report: yoy and tadpole survival vs prob of persistence. ----

filename <- paste("ForReport/graph_compare_yoyTadsurv_persist", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
grid.arrange(p_sens_tad_yoy_surv_persist,  p_hist_prob_persist_groups ,
             ncol = 1, nrow = 2)
dev.off()

#---- Explore relationship between bullfrog management and tadpole survival. ----
test2b <- cbind(parameterByIterTracking_this_alt_clean[iteration_numbers, c("s_mean_eggs_no_threats",
                                                           "s_mean_tadpoles_no_threats",
                                                            "s_mean_yoy_no_threats",
                                                            "bullfrogMgmt_effective")],
               as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[,])
colnames(test2b) <- c("s_mean_eggs_no_threats",
                      "s_mean_tadpoles_no_threats",
                      "s_mean_yoy_no_threats",
                      "bullfrogMgmt_effective", "prob_persist")

# My own exploration, not in report
testglm2b <- glm(prob_persist ~ s_mean_tadpoles_no_threats + s_mean_yoy_no_threats + 
                    s_mean_tadpoles_no_threats*bullfrogMgmt_effective + s_mean_yoy_no_threats*bullfrogMgmt_effective, 
                # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
                family = quasibinomial, data =  test2b)

summary(testglm2b)
library(visreg)
visreg(testglm2b , "s_mean_tadpoles_no_threats", by="bullfrogMgmt_effective")
visreg(testglm2b , "bullfrogMgmt_effective", by="s_mean_tadpoles_no_threats")

visreg(testglm2b , "s_mean_yoy_no_threats", by="bullfrogMgmt_effective")
visreg(testglm2b , "bullfrogMgmt_effective", by="s_mean_yoy_no_threats")
###

# Make a graph
p_sens_eggs_surv_bullfrogMgmt <- ggplot2::ggplot(data = test2, ggplot2::aes(x=survival_eggs, 
                                                                           y = prob_persist, 
                                                                           fill = bullfrogMgmt_effective)) +
  ggplot2::geom_point(ggplot2::aes(fill = bullfrogMgmt_effective), shape = 21, alpha = 0.5) +
  ggplot2::geom_smooth(ggplot2::aes(group=bullfrogMgmt_effective), color = "black", size=0.5) + 
  ggplot2::scale_fill_discrete(type = "viridis") +
  ggplot2::geom_vline(xintercept = best_guess_input_eggs, linetype = "dashed", color = "red") +
  ggplot2::xlab("Mean egg survival\n (no threats)") +
  ggplot2::ylab( "Probability of persistence") + 
  ggplot2::ggtitle( "") + 
  ggplot2::labs(size='', fill='Bullfrog management effective    ')  +
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
p_sens_eggs_surv_bullfrogMgmt 

p_sens_tad_surv_bullfrogMgmt <- ggplot2::ggplot(data = test2, ggplot2::aes(x=survival_tadpoles, 
                                                                          y = prob_persist, 
                                                                          fill = bullfrogMgmt_effective)) +
  ggplot2::geom_point(ggplot2::aes(fill = bullfrogMgmt_effective), shape = 21, alpha = 0.5) +
  ggplot2::geom_smooth(ggplot2::aes(group=bullfrogMgmt_effective), color = "black", size=0.5) + 
  ggplot2::scale_fill_discrete(type = "viridis") +
   ggplot2::geom_vline(xintercept = best_guess_input_tad, linetype = "dashed", color = "red") +
  ggplot2::xlab("Mean tadpole survival\n (no threats)") +
  ggplot2::ylab( "Probability of persistence") + 
  ggplot2::ggtitle( "") + 
  ggplot2::labs(size='', fill='Bullfrog management effective    ')  +
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

p_sens_tad_surv_bullfrogMgmt

p_sens_yoy_surv_bullfrogMgmt <- ggplot2::ggplot(data = test2, ggplot2::aes(x=survival_yoy, 
                                                                           y = prob_persist, 
                                                                           fill = bullfrogMgmt_effective)) +
  ggplot2::geom_point(ggplot2::aes(fill = bullfrogMgmt_effective), shape = 21, alpha = 0.5) +
  ggplot2::geom_smooth(ggplot2::aes(group=bullfrogMgmt_effective), color = "black", size=0.5) + 
  ggplot2::scale_fill_discrete(type = "viridis") +
  ggplot2::geom_vline(xintercept = best_guess_input_yoy, linetype = "dashed", color = "red") +
  ggplot2::xlab("Mean young-of-year survival\n (no threats)") +
  ggplot2::ylab( "Probability of persistence") + 
  ggplot2::ggtitle( "") + 
  ggplot2::labs(size='', fill='Bullfrog management effective    ')  +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p_sens_yoy_surv_bullfrogMgmt

p_sens_juv_surv_bullfrogMgmt <- ggplot2::ggplot(data = test2, ggplot2::aes(x=survival_juv, 
                                                                           y = prob_persist, 
                                                                           fill = bullfrogMgmt_effective)) +
  ggplot2::geom_point(ggplot2::aes(fill = bullfrogMgmt_effective), shape = 21, alpha = 0.5) +
  ggplot2::geom_smooth(ggplot2::aes(group=bullfrogMgmt_effective), color = "black", size=0.5) + 
  ggplot2::scale_fill_discrete(type = "viridis") +
  ggplot2::geom_vline(xintercept = best_guess_input_juv, linetype = "dashed", color = "red") +
  ggplot2::xlab("Mean juvenile survival\n (no threats)") +
  ggplot2::ylab( "Probability of persistence") + 
  ggplot2::ggtitle( "") + 
  ggplot2::labs(size='', fill='Bullfrog management effective    ')  +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p_sens_juv_surv_bullfrogMgmt

#---- Export panel graph for report: relationship between bullfrog management and tadpole survival. ----

# Export panel graph for report
filename <- paste("ForReport/graph_bullfrogMgt_vs_survival", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
grid.arrange(p_sens_eggs_surv_bullfrogMgmt, 
             p_sens_tad_surv_bullfrogMgmt,
             p_sens_yoy_surv_bullfrogMgmt,
             p_sens_juv_surv_bullfrogMgmt,
             ncol = 2, nrow = 2)
dev.off()


#---- Explore relationship between persistence and self-sustaining. ----
# Plot abundance vs persistence

results_all_this_alt_yr50 <- as.data.frame(results_all_this_alt[,c( "iteration", "metric", "50")])
colnames(results_all_this_alt_yr50) <- c( "iteration", "metric", "value")

library(ggplot2)
library(dplyr)
library(tidyr)

results_all_this_alt_yr50_wide1 <- results_all_this_alt_yr50 %>% 
  pivot_wider(names_from = metric, values_from = value)
colnames(results_all_this_alt_yr50_wide1) <- c("iteration", "mean_abundance", "prob_of_persis", "prob_of_selfsustain")
results_all_this_alt_yr50_wide <- cbind(results_all_this_alt_yr50_wide1, parameterByIterTracking$carrying_capacity_BSCWMA[iteration_numbers])
colnames(results_all_this_alt_yr50_wide) <- c("iteration", "mean_abundance", "prob_of_persis", "prob_of_selfsustain", "carrying_capacity_BSCWMA")

results_all_this_alt_yr50_wide$mean_prop_K <- results_all_this_alt_yr50_wide$mean_abundance/results_all_this_alt_yr50_wide$carrying_capacity_BSCWMA

ggplot2::ggplot(results_all_this_alt_yr50_wide, ggplot2::aes(x = mean_prop_K, y = prob_of_persis)) +
  ggplot2::geom_point(ggplot2::aes(fill = prob_of_selfsustain, size = prob_of_selfsustain), shape = 21, alpha = 0.5) +
  ggplot2::scale_fill_continuous(type = "viridis")

ggplot2::ggplot(results_all_this_alt_yr50_wide, ggplot2::aes(x = mean_prop_K, y = prob_of_selfsustain)) +
  ggplot2::geom_point(ggplot2::aes(fill = prob_of_persis, size = prob_of_persis), shape = 21, alpha = 0.5) +
  ggplot2::scale_fill_continuous(type = "viridis")


p_persis_vs_selfsustain <- ggplot2::ggplot(results_all_this_alt_yr50_wide, ggplot2::aes(x = prob_of_persis, 
                                                                                        y = prob_of_selfsustain)) +
  #ggplot2::geom_point(ggplot2::aes(fill = mean_prop_K, size = mean_prop_K), shape = 21, alpha = 0.5) +
  ggplot2::geom_point(ggplot2::aes(fill = mean_prop_K), shape = 21, alpha = 0.5) +
  ggplot2::scale_fill_continuous(type = "viridis") +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 10, barheight = 0.5)) +  #https://ggplot2.tidyverse.org/reference/guide_colourbar.html
  # ggplot2::geom_point(ggplot2::aes(fill = mean_abundance), shape = 21, alpha = 0.5) +
  geom_abline(slope=1, intercept=0, lty= "dashed")  +
  ggplot2::labs(x = "Probability of persistence") +
  ggplot2::labs(y = "Probability of a \n self-sustaining population") +
  ggplot2::labs(fill ='Mean proportion of carrying capacity    ', fill='')  +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p_persis_vs_selfsustain

#---- Export graph for report: relationship between persistence and self-sustaining. ----

filename <- paste("ForReport/graph_persis_vs_selfsustain", version, "_iter_", n_iter, ".tiff", sep="")
tiff(filename, width=12, height=4, units="in",pointsize=8, compression="lzw", bg="white", res=600)
print(p_persis_vs_selfsustain)
dev.off()


#---- Explore tadpole survival mean and vs prob of persistence - includes temporal variation. ----
# Uses the same results RData file that was loaded above for the tornado
# Confirmed can get simular insights to tornado, tornado is easier and clearer in my opinion :)
parameterByIterTracking_this_alt_clean2 <- parameterByIterTracking_this_alt_clean[iteration_numbers,]

# First filter out so only looking at data where yoy mean is high enough to not be the main problem
rows_yoybigenough <- which(parameterByIterTracking_this_alt_clean2$s_mean_yoy_no_threats >= best_guess_input_yoy)
rows_tadsmallenough <- which(parameterByIterTracking_this_alt_clean2$s_mean_tadpoles_no_threats < best_guess_input_tad)
rows <- intersect(rows_yoybigenough, rows_tadsmallenough)

# rows <- intersect(iteration_numbers, rows_yoybigenough)
# rows <- 1:nrow(parameterByIterTracking_this_alt_clean2)

rows <- rows_yoybigenough



test3 <- cbind(parameterByIterTracking_this_alt_clean2[rows, c("s_mean_tadpoles_no_threats", "s_sd_tadpoles_no_threats")],
               as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[rows,])
# colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
colnames(test3) <- c("survival_tadpoles_mean","survival_tadpoles_sd", "prob_persist")
which(is.na(test3$prob_persist) == TRUE)

ggplot2::ggplot(test3, ggplot2::aes(x = survival_tadpoles_sd, y = prob_persist)) +
  ggplot2::geom_point() + ggplot2::geom_smooth()


# change names later if stick, going fast
best_guess_input_mean <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_mean_tadpoles_no_threats")]))
min_mean <- as.numeric(as.character(min(test3$survival_tadpoles_mean)))
max_mean <- as.numeric(as.character(max(test3$survival_tadpoles_mean)))
P10_input_mean <- as.numeric(as.character(quantile(test3$survival_tadpoles_mean, 0.1)))
P90_input_mean <- as.numeric(as.character(quantile(test3$survival_tadpoles_mean, 0.9)))

best_guess_input_sd <- as.numeric(as.character(inputs$best_guess[which(inputs$input == "s_sd_tadpoles_no_threats")]))
min_sd <- as.numeric(as.character(min(test3$survival_tadpoles_sd)))
max_sd <- as.numeric(as.character(max(test3$survival_tadpoles_sd)))
P10_input_sd <- as.numeric(as.character(quantile(test3$survival_tadpoles_sd, 0.1)))
P90_input_sd <- as.numeric(as.character(quantile(test3$survival_tadpoles_sd, 0.9)))

library(ggplot2)
p_sens_tad_surv_mean_sd_persist <- ggplot2::ggplot(data = test3, ggplot2::aes(x=survival_tadpoles_mean, y = survival_tadpoles_sd)) +
  #ggplot2::geom_point(aes(fill = prob_persist, size = prob_persist), shape = 21, alpha = 0.7) +
  ggplot2::geom_point(aes(fill = prob_persist), shape = 21, alpha = 0.7) +
  scale_fill_viridis_c(guide = "legend", name="Probability of persistence") + # https://community.rstudio.com/t/ggplot2-is-it-possible-to-combine-color-fill-and-size-legends/17072/2
  scale_size_continuous(range = c(1, 5), name="Probability of persistence") +
   ggplot2::xlim(min_tad, max_tad) +
  ggplot2::ylim(min_sd, max_sd) +
  ggplot2::geom_hline(yintercept = best_guess_input_sd, linetype = "dashed", color = "red") +
  ggplot2::geom_vline(xintercept = best_guess_input_mean, linetype = "dashed", color = "red") +
  xlab("Mean tadpole survival\n (no threats)") +
  ylab( "SD tadpole survival\n (no threats)") + 
  ggtitle( "A)") + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p_sens_tad_surv_mean_sd_persist


testglm <- glm(prob_persist ~ survival_tadpoles_mean + survival_tadpoles_sd + survival_tadpoles_mean*survival_tadpoles_sd, 
               # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
               family = quasibinomial, data =  test3)

summary(testglm)
library(visreg)

# interaction is only significant when limit rows also to tadpole means below the P50
# but even with all the tadpole mean data the trends look similar when we look at the grah
# for large mean survival, sd doesn't matter
# for small mean survival, larger standard deviation is worse for prob of persistence

visreg(testglm , "survival_tadpoles_sd", by="survival_tadpoles_mean")
#visreg(testglm , "survival_tadpoles_mean", by="survival_tadpoles_sd")


#---- Explore ephemeral wetlands and tadpole survival. ----

rows <- rows_yoybigenough

test3b <- cbind(parameterByIterTracking_this_alt_clean2[rows, c("s_mean_tadpoles_no_threats",
                                                               "s_mean_ephWetlands_tadpoles_no_threats",
                                                               "ephWetRest_effective",
                                                               "ephemeral_freq_dry")],
               as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[rows,])

colnames(test3b) <- c( "survival_tadpoles_main","survival_tadpoles_ephemeral", "ephWetRest_effective","ephemeral_freq_dry", "prob_persist")


p_sens_tad_surv_main_vs_ephemeral_persist <- ggplot2::ggplot(data = test3b, ggplot2::aes(x=survival_tadpoles_main, y = survival_tadpoles_ephemeral)) +
  ggplot2::facet_wrap(~ephWetRest_effective) +
  #ggplot2::geom_point(aes(fill = prob_persist, size = prob_persist), shape = 21, alpha = 0.7) +
  ggplot2::geom_point(aes(fill = prob_persist), shape = 21, alpha = 0.7) +
  scale_fill_viridis_c(guide = "legend", name="Probability of persistence") + # https://community.rstudio.com/t/ggplot2-is-it-possible-to-combine-color-fill-and-size-legends/17072/2
  scale_size_continuous(range = c(1, 5), name="Probability of persistence") +
  ggplot2::xlim(min_tad, max_tad) +
  ggplot2::ylim(min_sd, max_sd) +
  ggplot2::geom_hline(yintercept = best_guess_input_sd, linetype = "dashed", color = "red") +
  ggplot2::geom_vline(xintercept = best_guess_input_mean, linetype = "dashed", color = "red") +
  xlab("Mean tadpole survival (no threats)\n - Main Wetlands") +
  ylab( "Mean tadpole survival (no threats)\n - Epehemeral Wetlands") + 
  ggtitle( "A)") + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black"),
    text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p_sens_tad_surv_main_vs_ephemeral_persist


testglm <- glm(prob_persist ~ survival_tadpoles_ephemeral + survival_tadpoles_main + survival_tadpoles_main*survival_tadpoles_ephemeral
               + ephemeral_freq_dry + survival_tadpoles_ephemeral * ephemeral_freq_dry , 
               # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
               family = quasibinomial, data =  test3b)

summary(testglm)
library(visreg)

# interaction is only significant when limit rows also to tadpole means below the P50
# but even with all the tadpole mean data the trends look similar when we look at the grah
# for large mean survival, sd doesn't matter
# for small mean survival, larger standard deviation is worse for prob of persistence

visreg(testglm , "survival_tadpoles_ephemeral", by="survival_tadpoles_main")
#visreg(testglm , "survival_tadpoles_main", by="survival_tadpoles_ephemeral")

visreg(testglm , "survival_tadpoles_ephemeral", by="ephemeral_freq_dry")
visreg(testglm , "ephemeral_freq_dry", by="survival_tadpoles_ephemeral")




###### IGNORE FROM HERE DOWN ##########
#OLD - not used for final report - was used to determine # of iterations, etc.
# Left here for now in case need the code again inthe future.
#---- Load one alternative to use for convergence testing. ----

# Load the Rdata file
files <-  list.files(path = ".","*.RData", full.names="TRUE")
i <- 2  
load(files[i])
yrs <- 50
# Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
results_all_iterations_fall <- results_all_iterations # initalize

for(j in 1:yrs){
  nrow(results_all_iterations_fall)
  results_all_iterations_fall[which(results_all_iterations_fall$class == "eggs"),paste(j)] <- 0
  results_all_iterations_fall[which(results_all_iterations_fall$class == "tadpoles"),paste(j)] <- 0 
}

#---- Appendix: convergence plots - number of runs per iteration. ----

#memory.limit(24000)# Pick some iterations that will show a range of outcomes
# 1570 has prob of persistence 1
# 1131 has prob of persistence of approx 0.97
# 495 has prob of persistence of approx 0.5
# 2056 has prob of persistence of approx 0.05
# 2323 has prob of persistence of approx 0

# parameterByIterTracking_forRunConvTest <- parameterByIterTracking[c(1570, 1131, 495, 2056, 2323),]
# Saved the workspace now and will use this on my mac to run

# Once run, load it back in and use going forward
# memory.limit() # check current memory limit
#  # increase memory limit if need be to load the file
# load("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/goBig_v1test12_runConvTestResult.RData")
load("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/goBig_vFinalJune2021_10Kiter.RData")

# Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
results_all_iterations_fall <- results_all_iterations # initalize

for(j in 1:yrs){
  nrow(results_all_iterations_fall)
  results_all_iterations_fall[which(results_all_iterations_fall$class == "eggs"),paste(j)] <- 0
  results_all_iterations_fall[which(results_all_iterations_fall$class == "tadpoles"),paste(j)] <- 0 
}


# Intalize the a list to store results for a handful of iterations
convergence_test_runs_per_iter <- list()

# Now run the convergence test code, takes approx 30 min to run on my work laptop
for(i in 1:5){
  convergence_test_runs_per_iter[[i]] <- dapva::convergenceTestRunsPerIteration(
    results_all_for_this_iteration = results_all_iterations_fall[which(results_all_iterations_fall$iteration == i),],
    test_interval = 50,
    iteration_number = i,
    initial_year = 1,
    final_year = 50,
    num_rand_pulls_per_subset_size = 100)
}

convergence_test_runs_multiple_iter <- do.call("rbind", convergence_test_runs_per_iter)

rows <- which(convergence_test_runs_multiple_iter$iteration_num != 1) # take out iteration 1 because it looks similar to 2
convergence_test_runs_multiple_iter <- convergence_test_runs_multiple_iter[rows,]
# Rename iteration numbers so shows 1-4 in the graph
convergence_test_runs_multiple_iter$iteration_num <- convergence_test_runs_multiple_iter$iteration_num  -1

graphs <- dapva::graphCongvTestRunsPerIter(convergence_test_runs_multiple_iter,
                                           x_location_vertical_line = 300,
                                           title = " ")


# graphs[[1]]
graphs[[2]]

# Export graph for appendix
filename <- paste("ForReport/appendix_graph_convergence_runs", version,".tiff", sep="")
tiff(filename, width=12, height= 8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
graphs[[2]]
dev.off()

#---- Appendix: convergence plots - number of iterations. ----

# Need to use a data set with more iterations than needed so can show convergence
# For example, if using 500 iterations need to use a data set with e.g. 2500 iterations here
# This one didn't have the min 300 runs per iteration so will need to be redone once rerun with that update
load("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/goBig_v1test13_2Kit.RData")

# goBig_vFinalJune2021_10Kiter.RData

# Resample to visually inspect convergence
convergence_test <- dapva::convergenceTestIterations(results_all_this_alt = results_summary_all_iterations_overall,
                                                     test_interval = 100,
                                                     num_rand_pulls_per_subset_size = 100,
                                                     initial_year = 1,
                                                     final_year = 50)


# Make graphs for visual inspection
graphs <- dapva::graphCongvTestIter(convergence_test, x_location_vertical_line = 400,
                                    title = " ")

(p_prob_persist_conv_iter_mean <- graphs[[2]])
(p_prob_persist_conv_iter_median <- graphs[[4]])

(p_abundance_conv_iter_mean <- graphs[[1]])
(p_abundance_conv_iter_median <- graphs[[3]])

# Export graph for appendix
filename <- paste("ForReport/appendix_graph_convergence_iterations", version,".tiff", sep="")
tiff(filename, width=12, height= 8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
p_prob_persist_conv_iter_mean 
dev.off()

# Extract the range of prob of persistence from 500 iterations
int <- convergence_test$prob_persist_mean[which(convergence_test$subset_size == 400)]
min(int) 
max(int) 
max(int) - min(int)

#---- Load base case results for one alternative to use in the examples in the appendices----
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

#---- Appendix: mean survival rates after incorporating threats (base case example). ----

# BASE CASE
inputs_all <- dapva4nlf::getNLFIdahoFeasinputs()
inputs <- inputs_all[[1]]
parameterByIterTracking_baseCase <- selectNLFIdahoParameterByIterTracking(inputs, base_case = TRUE)
parameterByIterTracking <- parameterByIterTracking_baseCase

# BASE CASE - eggs, no threats
s_eggs_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_eggs_no_threats")])
s_eggs_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_eggs_no_threats")])

# BASE CASE - eggs with threats
s_pct_reduced_eggs_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_bullfrogs")])
s_pct_reduced_eggs_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_chytrid")])
s_pct_reduced_eggs_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_eggs_roads")])

# BASE CASE - tadpoles, no threats
s_tadpoles_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_tadpoles_no_threats")])
s_tadpoles_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_tadpoles_no_threats")])
s_tadpoles_no_threats_dist <- dapva::estBetaParams(mean = s_tadpoles_mean, sd = s_tadpoles_sd)


# BASE CASE - tadpoles with threats
s_pct_reduced_tadpoles_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_bullfrogs")])
s_pct_reduced_tadpoles_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_chytrid")])
s_pct_reduced_tadpoles_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_tadpoles_roads")])

# BASE CASE - yoy, no threats
s_yoy_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_yoy_no_threats")])
s_yoy_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_yoy_no_threats")])
s_yoy_no_threats_dist <- dapva::estBetaParams(mean = s_yoy_mean, sd = s_yoy_sd)

# BASE CASE - yoy with threats
s_pct_reduced_yoy_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_bullfrogs")])
s_pct_reduced_yoy_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_chytrid")])
s_pct_reduced_yoy_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_yoy_roads")])

# BASE CASE - juv, no threats
s_juv_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_juv_no_threats")])
s_juv_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_juv_no_threats")])

# BASE CASE - juv with threats
s_pct_reduced_juv_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_bullfrogs")])
s_pct_reduced_juv_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_adult_chytrid")])
s_pct_reduced_juv_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_roads")])

# BASE CASE - adult, no threats
s_adult_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_adult_no_threats")])
s_adult_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_adult_no_threats")])
s_adult_no_threats_dist <- dapva::estBetaParams(mean = s_adult_mean, sd = s_adult_sd)

# BASE CASE - adult with threats
s_pct_reduced_adult_bullfrogs <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_bullfrogs")])
s_pct_reduced_adult_chytrid <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_juvenile_adult_chytrid")])
s_pct_reduced_adult_roads <- as.numeric(parameterByIterTracking[i, paste0("s_pct_reduced_adult_roads")])

survival_w_threats_comparison <- as.data.frame(matrix(nrow = 5, ncol = 7))
colnames(survival_w_threats_comparison) <- c("life_stage", "no_threat", "chytrid", "roads", "bullfrogs", "chytrid_and_roads",  "all_three")

survival_w_threats_comparison$life_stage[1] <- "eggs to tadpoles"
survival_w_threats_comparison$no_threat[1] <- s_eggs_mean
survival_w_threats_comparison$chytrid[1] <- (1-s_pct_reduced_eggs_chytrid/100)*s_eggs_mean 
survival_w_threats_comparison$roads[1] <- (1-s_pct_reduced_eggs_roads/100)*s_eggs_mean 
survival_w_threats_comparison$chytrid_and_roads[1] <- (1-s_pct_reduced_eggs_chytrid/100)*
  (1-s_pct_reduced_eggs_roads/100)*s_eggs_mean 
survival_w_threats_comparison$bullfrogs[1] <- (1-s_pct_reduced_eggs_bullfrogs/100)*s_eggs_mean 
survival_w_threats_comparison$all_three[1] <- (1-s_pct_reduced_eggs_chytrid/100)*
  (1-s_pct_reduced_eggs_roads/100)*
  (1-s_pct_reduced_eggs_bullfrogs/100)*s_eggs_mean 

survival_w_threats_comparison$life_stage[2] <- "tadpoles to yoy"
survival_w_threats_comparison$no_threat[2] <- s_tadpoles_mean
survival_w_threats_comparison$chytrid[2] <- (1-s_pct_reduced_tadpoles_chytrid/100)*s_tadpoles_mean 
survival_w_threats_comparison$roads[2] <- (1-s_pct_reduced_tadpoles_roads/100)*s_tadpoles_mean 
survival_w_threats_comparison$chytrid_and_roads[2] <- (1-s_pct_reduced_tadpoles_chytrid/100)*
  (1-s_pct_reduced_tadpoles_roads/100)*s_tadpoles_mean 
survival_w_threats_comparison$bullfrogs[2] <- (1-s_pct_reduced_tadpoles_bullfrogs/100)*s_tadpoles_mean 
survival_w_threats_comparison$all_three[2] <- (1-s_pct_reduced_tadpoles_chytrid/100)*
  (1-s_pct_reduced_tadpoles_roads/100)*
  (1-s_pct_reduced_tadpoles_bullfrogs/100)*s_tadpoles_mean 


survival_w_threats_comparison$life_stage[3] <- "yoy to juv"
survival_w_threats_comparison$no_threat[3] <- s_yoy_mean
survival_w_threats_comparison$chytrid[3] <- (1-s_pct_reduced_yoy_chytrid/100)*s_yoy_mean 
survival_w_threats_comparison$roads[3] <- (1-s_pct_reduced_yoy_roads/100)*s_yoy_mean 
survival_w_threats_comparison$chytrid_and_roads[3] <- (1-s_pct_reduced_yoy_chytrid/100)*
  (1-s_pct_reduced_yoy_roads/100)*s_yoy_mean 
survival_w_threats_comparison$bullfrogs[3] <- (1-s_pct_reduced_yoy_bullfrogs/100)*s_yoy_mean 
survival_w_threats_comparison$all_three[3] <- (1-s_pct_reduced_yoy_chytrid/100)*
  (1-s_pct_reduced_yoy_roads/100)*
  (1-s_pct_reduced_yoy_bullfrogs/100)*s_yoy_mean 


survival_w_threats_comparison$life_stage[4] <- "juv to adult"
survival_w_threats_comparison$no_threat[4] <- s_juv_mean
survival_w_threats_comparison$chytrid[4] <- (1-s_pct_reduced_juv_chytrid/100)*s_juv_mean 
survival_w_threats_comparison$roads[4] <- (1-s_pct_reduced_juv_roads/100)*s_juv_mean 
survival_w_threats_comparison$chytrid_and_roads[4] <- (1-s_pct_reduced_juv_chytrid/100)*
  (1-s_pct_reduced_juv_roads/100)*s_juv_mean 
survival_w_threats_comparison$bullfrogs[4] <- (1-s_pct_reduced_juv_bullfrogs/100)*s_juv_mean 
survival_w_threats_comparison$all_three[4] <- (1-s_pct_reduced_juv_chytrid/100)*
  (1-s_pct_reduced_juv_roads/100)*
  (1-s_pct_reduced_juv_bullfrogs/100)*s_juv_mean 

survival_w_threats_comparison$life_stage[5] <- "adult to adult"
survival_w_threats_comparison$no_threat[5] <- s_adult_mean
survival_w_threats_comparison$chytrid[5] <- (1-s_pct_reduced_adult_chytrid/100)*s_adult_mean 
survival_w_threats_comparison$roads[5] <- (1-s_pct_reduced_adult_roads/100)*s_adult_mean 
survival_w_threats_comparison$chytrid_and_roads[5] <- (1-s_pct_reduced_adult_chytrid/100)*
  (1-s_pct_reduced_adult_roads/100)*s_adult_mean 
survival_w_threats_comparison$bullfrogs[5] <- (1-s_pct_reduced_adult_bullfrogs/100)*s_adult_mean 
survival_w_threats_comparison$all_three[5] <- (1-s_pct_reduced_adult_chytrid/100)*
  (1-s_pct_reduced_adult_roads/100)*
  (1-s_pct_reduced_adult_bullfrogs/100)*s_adult_mean 


survival_w_threats_comparison$life_stage <- factor(survival_w_threats_comparison$life_stage, 
                                                   levels = c("eggs to tadpoles",
                                                              "tadpoles to yoy", 
                                                              "yoy to juv", 
                                                              "juv to adult", "adult to adult"))


survival_w_threats_comparison_long <- reshape2::melt(survival_w_threats_comparison,  id.vars=c("life_stage"))
colnames(survival_w_threats_comparison_long) <- c("life_stage", "threats" , "survival_rate" )


p_basecase_survival_means <- ggplot2::ggplot(survival_w_threats_comparison_long, ggplot2::aes(x = threats, y = survival_rate)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::facet_wrap(~life_stage) +
  ggplot2::labs(x = "Threats") +
  ggplot2::labs(y = "Survival Rate") +
  ggplot2::ggtitle("Base case (P50) mean survival rates with compounding threats") +
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

filename <- paste("ForReport/appendix_graph_survival_means_basecase", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
  p_basecase_survival_means
dev.off()


#---- Appendix: base case for Go Big or Go Home - example: individual runs. ----

# Start by showing for each class and population; include eggs and tadpoles for illustrative purposes rather than the fall numbers used for the totals
results_to_use <- results_basecase  # results_basecase_fall if want to get the total #
# results_to_use <- results_basecase[1:100,]  # results_basecase_fall if want to get the total #

library(reshape2)
test2 <- reshape2::melt(results_to_use, id.vars=c("iteration", "run", "pop", "class", "sex"))
colnames(test2)[which(colnames(test2) == "variable")]<- "year"
colnames(test2)[which(colnames(test2) == "value")]<- "number_of_indiv"
test2$year <- as.numeric(as.character(test2$year))

p_basecase_runs_example <- ggplot2::ggplot(test2[which(test2$run <= 10),], ggplot2::aes(x = year, y = number_of_indiv, group = run, color = run)) +
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

filename <- paste("ForReport/appendix_graph_basecase_runs_example", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
p_basecase_runs_example
dev.off()


# TO show totals at fall census by run

library(dplyr)
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

#---- Appendix: base case for Go Big or Go Home - example: one iteration ----
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



filename <- paste("ForReport/appendix_graph_basecase_iteration_example", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
grid.arrange(persistence_graph ,  selfsustaining_graph,
             ncol = 2, nrow = 1)
dev.off()


#---- Extra: combining vs separating parametric uncertainty - base case. ----

# Let's start with the base case parameter draw
load("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/goBig_v1test12_basecase.RData")
yrs <- 50
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


results_summary_basecase <- dapva::makeResultsSummaryOneIteration(results_basecase_fall,
                                                                  by_pop = 'no',
                                                                  initial_year = 1,
                                                                  yrs = 50,
                                                                  n_iter = 1,
                                                                  n_runs_per_iter = length(unique(results_basecase_fall$run)),
                                                                  alternative = paste0(alternative_details$alt_name_full),
                                                                  iteration_number = 1,
                                                                  prob_self_sustain = TRUE,
                                                                  lambda_over_x_years = 10)
#Write out the results so can load them all in later
write.csv(results_summary_basecase, file = paste0("results_summary_basecase_", name, version,".csv"), row.names = FALSE)

# Graph the results
results_summary_prob_persist_basecase <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_basecase,
                                                                     metric = "probability of persistence",
                                                                     initial_year = 1, credible_interval = 0.95)

(persistence_graph_basecase <- dapva::graphResultsSummary(results_summary_prob_persist_basecase))



#---- Extra: combining vs separating parametric uncertainty - parametric and process combined. ----

# Now pretend parametric and process uncertainty together
# Using the 'Go Big or Go Home' alternative

#path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/BTPD_PVA/Results/BTPD_baseline_results_march17"# on my mac
setwd(path_to_results_folder) # on my mac

# Once run, load it back in and use going forward
# memory.limit() # check current memory limit
#  # increase memory limit if need be to load the file
#load("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/goBig_v1test13_2Kit.RData")

# on mac
load("/Users/laurakeating/Documents/R/R_scripts/NLF_PVA/Results/goBig_v1test13_2Kit.RData")

# goBig_vFinalJune2021_10Kiter.RData

# Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
results_all_iterations_fall <- results_all_iterations # initalize

for(j in 1:yrs){
  nrow(results_all_iterations_fall)
  results_all_iterations_fall[which(results_all_iterations_fall$class == "eggs"),paste(j)] <- 0
  results_all_iterations_fall[which(results_all_iterations_fall$class == "tadpoles"),paste(j)] <- 0 
} # Takes a min or two

# Rename the runs so that they have unique IDs with iteration and run since 
# here pretending like they are all independent runs that we want one set of results for
results_all_iterations_fall$run <- paste(results_all_iterations_fall$iteration, results_all_iterations_fall$run)

# Run the prob of persistence function on the whole thing to show what it would look like if process and parametric were together
n_iter_runcombos <- length(unique(paste(results_all_iterations_fall$iteration, results_all_iterations_fall$run)))
results_summary_for_all_iter_and_runs_unct_comb <- dapva::makeResultsSummaryOneIteration(results_all_iterations_fall,
                                                                               by_pop = 'no',
                                                                               initial_year = 1,
                                                                               yrs = 50,
                                                                               n_iter = 1,
                                                                               n_runs_per_iter = n_iter_runcombos,
                                                                               alternative = paste0(alternative_details$alt_name_full),
                                                                               iteration_number = 1,
                                                                               prob_self_sustain = FALSE, # much faster without this, not needed here
                                                                               lambda_over_x_years = 10)

#Write out the results so can load them all in later
write.csv(results_summary_for_all_iter_and_runs_unct_comb, file = paste0("results_summary_for_all_iter_and_runs_unct_comb", name, version,".csv"), row.names = FALSE)

# Graph the results
# Point here is that if we combine parametric and process uncertainty then:
# Results seem more optimistic (in this case); point is that it is different
# Don't get any insights into where we should put more effort to learn more to make a more informed decision

results_summary_prob_persist_unct_comb <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_for_all_iter_and_runs_unct_comb,
                                                                     metric = "probability of persistence",
                                                                     initial_year = 1, credible_interval = 0.95)


(persistence_graph_unct_comb <- dapva::graphResultsSummary(results_summary_prob_persist_unct_comb))

#---- Extra: combining vs separating parametric uncertainty - parametric and process seperated. ----
# Using the 'Go Big or Go Home' alternative
# memory.limit() # check current memory limit
# memory.limit(24000) # increase memory limit if need be to load the file
#load("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/goBig_v1test13_2Kit.RData")
# on mac
load("/Users/laurakeating/Documents/R/R_scripts/NLF_PVA/Results/goBig_v1test13_2Kit.RData")

# goBig_vFinalJune2021_10Kiter.RData



# Summarize the 'by population' results into 'overall' results and export that
write.csv(results_summary_all_iterations_overall, file = paste0("results_overall_", name, version,".csv"), row.names = FALSE)

#---- Extra: combining vs separating parametric uncertainty - combine the graphs. ----

# Load each of the results csvs in

results_summary_basecase <- read.csv("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/results_summary_basecase_goBig_v1test12_basecase.csv")
results_summary_for_all_iter_and_runs_unct_comb <- read.csv("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/results_summary_for_all_iter_and_runs_unct_combgoBig_v1test13_2Kit.csv")
results_summary_for_all_iter_and_runs_unct_tgth <- read.csv("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/results_overall_goBig_v1test13_2Kit.csv")

colnames(results_summary_basecase)[7:56] <- 1:50
colnames(results_summary_for_all_iter_and_runs_unct_comb)[7:56] <- 1:50
colnames(results_summary_for_all_iter_and_runs_unct_tgth)[7:56] <- 1:50

# Tweak them to make this graph look like how I want for now without having to change the dapva code
results_summary_basecase$pop <- "Base Case"
results_summary_basecase$alternative <- "A)"

results_summary_for_all_iter_and_runs_unct_comb$pop <- "Parametric and Process Uncertainty Combined"
results_summary_for_all_iter_and_runs_unct_comb$alternative <- "B)"

results_summary_for_all_iter_and_runs_unct_tgth$pop <- "Parametric and Process Uncertainty Seperated"
results_summary_for_all_iter_and_runs_unct_tgth$alternative <- "C)"


# Use them to make graphs
# Base case
results_summary_prob_persist_basecase <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_basecase,
                                                                              metric = "probability of persistence",
                                                                              initial_year = 1, credible_interval = 0.95)

(persistence_graph_basecase <- dapva::graphResultsSummary(results_summary_prob_persist_basecase, y_lim = c(0,1)))

# Parametric and process uncertainty combined
results_summary_prob_persist_unct_comb <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_for_all_iter_and_runs_unct_comb,
                                                                               metric = "probability of persistence",
                                                                               initial_year = 1, credible_interval = 0.95)


(persistence_graph_unct_comb <- dapva::graphResultsSummary(results_summary_prob_persist_unct_comb, y_lim = c(0,1)))

# Parametric and process uncertainty seperated
results_summary_prob_persist_unct_tgth <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_for_all_iter_and_runs_unct_tgth,
                                                                               metric = "probability of persistence",
                                                                               initial_year = 1, credible_interval = 0.95)


(persistence_graph_unct_tgth <- dapva::graphResultsSummary(results_summary_prob_persist_unct_tgth, y_lim = c(0,1)))

# Export the graphs using grid.arrange
filename <- paste("C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/ForReport/compare_persist_combineUnct", version,".tiff", sep="")
tiff(filename, width=12, height=4, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
gridExtra:: grid.arrange(persistence_graph_basecase,  
                         persistence_graph_unct_comb,
                         persistence_graph_unct_tgth,
                         ncol = 3, nrow = 1)
dev.off()

#---- OLD -  Show what results would have looked like if combined parametric and process uncertainty ######
path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results"# on my work PC
#path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/BTPD_PVA/Results/BTPD_baseline_results_march17"# on my mac
setwd(path_to_results_folder) # on my mac
files <-  list.files(path = ".","*.RData", full.names="TRUE")

# FOr each alternative. 
# Load the Rdata file
# i = 1  

for(i in c(1, 2, 4, 5, 6, 7)){ # file 3 get "Error: cannot allocate vector of size 18.1 Mb
  
  print(paste('combining process and parametric uncertainty for file #', i))
  
  load(files[i])
  
  # Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
  results_all_iterations_fall <- results_all_iterations # initalize
  
  for(j in 1:yrs){
    nrow(results_all_iterations_fall)
    results_all_iterations_fall[which(results_all_iterations_fall$class == "eggs"),paste(j)] <- 0
    results_all_iterations_fall[which(results_all_iterations_fall$class == "tadpoles"),paste(j)] <- 0 
  } # Takes a min or two
  
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
  
  #Write out the results so can load them all in later
  write.csv(results_summary_for_all_iter_and_runs, file = paste0("results_summary_for_all_iter_and_runs_", name, version,".csv"), row.names = FALSE)
  
} # close for loop through each file


# Graph the results
# Point here is that if we combine parametric and process uncertainty then:
# Results seem more optimistic (in this case); point is that it is different
# Don't get any insights into where we should put more effort to learn more to make a more informed decision

# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_summary_for_all_iter_and_runs_")
results_summary_for_all_iter_and_runs_list <- lapply(temp_iter, read.csv)
results_summary_for_all_iter_and_runs_allAlt <- do.call(rbind, results_summary_for_all_iter_and_runs_list)
colnames(results_summary_for_all_iter_and_runs_allAlt)[7:ncol(results_summary_for_all_iter_and_runs_allAlt)] <- 1:50


results_summary_prob_persist_combineUnct <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_for_all_iter_and_runs_allAlt,
                                                                                 metric = "probability of persistence",
                                                                                 initial_year = 1, credible_interval = 0.95)

# (persistence_graph_combineUnct <- dapva::graphResultsSummary(results_summary_prob_persist_combineUnct))



# Make a summary graph with just the level of effort alternatives to show the difference



# Add the Do Nothing scenario, which is 0 since population is currently extirpated with no chance of natural recovery
do_Nothing <- results_summary_prob_persist_combineUnct[1:yrs,] # initalize
do_Nothing$mean <- 0
do_Nothing$median <- 0
do_Nothing$lcl <- 0
do_Nothing$ucl <- 0
do_Nothing$n_iter <- 0
do_Nothing$n_runs_per_iter <- 0
do_Nothing$alternative <- "Do Nothing"
results_summary_prob_persist_combineUnct <- rbind(results_summary_prob_persist_combineUnct, do_Nothing)

# Alternatives of interest
goBig_alt_name  <- "Go Big or Go Home "
mostReal_alt_name  <- "Middle of the Road"
lowEffort_alt_name  <- "Minimum Funding Availabilty / Low Effort"
doNothing_alt_name  <- "Do Nothing"

int9 <- results_summary_prob_persist_combineUnct[c(which(results_summary_prob_persist_combineUnct$alternative == goBig_alt_name),
                                                   which(results_summary_prob_persist_combineUnct$alternative == mostReal_alt_name),
                                                   which(results_summary_prob_persist_combineUnct$alternative == lowEffort_alt_name),
                                                   which(results_summary_prob_persist_combineUnct$alternative == doNothing_alt_name)),]
int9$alternative[which(int9$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
int9$alternative[which(int9$alternative == lowEffort_alt_name)] <- "Minimum Funding / Low Effort" # put it on two lines


int9$alternative <- factor(int9$alternative, levels=c("Do Nothing", "Minimum Funding / Low Effort",
                                                      "Middle of the Road", "Go Big or Go Home")) # reorder factor levels

(persist_effort_graph_combineUnct <- dapva::graphResultsSummary(results_summary = int9,
                                                                overlap = FALSE,
                                                                title = 'B)',
                                                                x_axis_lab = "Year",
                                                                y_axis_lab = "\n Probability of Persistence \n ")) # The extra lines push the title out to the same spot as in panel B)


filename <- paste("ForReport/compare_persist_combineUnct", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
gridExtra:: grid.arrange(persist_effort_graph1,  
                         persist_effort_graph_combineUnct,
                         ncol = 2, nrow = 1)
dev.off()


#---- OLD - Try the same graph as above but with just base case parameter draw ######
path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/basecase"# on my work PC
#path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/BTPD_PVA/Results/BTPD_baseline_results_march17"# on my mac
setwd(path_to_results_folder) # on my mac
files <-  list.files(path = ".","*.RData", full.names="TRUE")

# NOTE: IF GO THIS ROUTE, MAY WANT TO RUN MORE ITERATIONS THAN NEEDED FOR CONVERGENCE AT YEAR 50 BECAUSE IT IS 0

# Load the Rdata file

files_basecase <-  list.files(path = ".","*basecase.RData", full.names="TRUE")
# i = 1  

for(i in 1:length(files_basecase)){
  
  print(paste('basecase for alternative #', i))
  
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
  
  
  results_summary_basecase <- dapva::makeResultsSummaryOneIteration(results_basecase_fall,
                                                 by_pop = 'no',
                                                 initial_year = 1,
                                                 yrs = 50,
                                                 n_iter = 1,
                                                 n_runs_per_iter = length(unique(results_basecase_fall$run)),
                                                 alternative = paste0(alternative_details$alt_name_full),
                                                 iteration_number = 1,
                                                 prob_self_sustain = TRUE,
                                                 lambda_over_x_years = 10)
  #Write out the results so can load them all in later
  write.csv(results_summary_basecase, file = paste0("results_summary_basecase_", name, version,".csv"), row.names = FALSE)

} # close for loop through each file



# Graph the results
# Point here is that if we combine parametric and process uncertainty then:
# Results seem more optimistic (in this case); point is that it is different
# Don't get any insights into where we should put more effort to learn more to make a more informed decision

# Upload all of the results in the results folder and bind them together
temp_iter <- list.files(pattern="*results_summary_basecase_")
results_summary_basecase_list <- lapply(temp_iter, read.csv)
results_summary_basecase_allAlt <- do.call(rbind, results_summary_basecase_list)
colnames(results_summary_basecase_allAlt)[7:ncol(results_summary_basecase_allAlt)] <- 1:50


results_summary_prob_persist_basecase <- dapva::makeResultsSummaryMultipleAlt(results_summary_all_iterations  = results_summary_basecase_allAlt,
                                                                                 metric = "probability of persistence",
                                                                                 initial_year = 1, credible_interval = 0.95)

# (persistence_graph_combineUnct <- dapva::graphResultsSummary(results_summary_prob_persist_combineUnct))



# Make a summary graph with just the level of effort alternatives to show the difference



# Add the Do Nothing scenario, which is 0 since population is currently extirpated with no chance of natural recovery
do_Nothing <- results_summary_prob_persist_basecase[1:yrs,] # initalize
do_Nothing$mean <- 0
do_Nothing$median <- 0
do_Nothing$lcl <- 0
do_Nothing$ucl <- 0
do_Nothing$n_iter <- 0
do_Nothing$n_runs_per_iter <- 0
do_Nothing$alternative <- "Do Nothing"
results_summary_prob_persist_basecase <- rbind(results_summary_prob_persist_basecase, do_Nothing)

# Alternatives of interest
goBig_alt_name  <- "Go Big or Go Home "
mostReal_alt_name  <- "Middle of the Road"
lowEffort_alt_name  <- "Minimum Funding Availabilty / Low Effort"
doNothing_alt_name  <- "Do Nothing"

int10 <- results_summary_prob_persist_basecase [c(which(results_summary_prob_persist_basecase$alternative == goBig_alt_name),
                                                   which(results_summary_prob_persist_basecase$alternative == mostReal_alt_name),
                                                   which(results_summary_prob_persist_basecase$alternative == lowEffort_alt_name),
                                                   which(results_summary_prob_persist_basecase$alternative == doNothing_alt_name)),]
int10$alternative[which(int10$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
int10$alternative[which(int10$alternative == lowEffort_alt_name)] <- "Minimum Funding / Low Effort" # put it on two lines


int10$alternative <- factor(int10$alternative, levels=c("Do Nothing", "Minimum Funding / Low Effort",
                                                      "Middle of the Road", "Go Big or Go Home")) # reorder factor levels

(persist_effort_graph_basecase <- dapva::graphResultsSummary(results_summary = int10,
                                                                overlap = FALSE,
                                                                title = 'A)',
                                                                x_axis_lab = "Year",
                                                                y_axis_lab = "\n Probability of Persistence \n ")) # The extra lines push the title out to the same spot as in panel B)


path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results"# on my work PC
#path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/BTPD_PVA/Results/BTPD_baseline_results_march17"# on my mac
setwd(path_to_results_folder) # on my mac
files <-  list.files(path = ".","*.RData", full.names="TRUE")


filename <- paste("ForReport/compare_persist_diffapproaches", version,".tiff", sep="")
tiff(filename, width=12, height=8, units="in",
     pointsize=8, compression="lzw", bg="white", res=600)
     #restoreConsole=TRUE)
gridExtra:: grid.arrange(persist_effort_graph_basecase, 
                         persist_effort_graph_combineUnct,
                         persist_effort_graph1,  
                         ncol = 3, nrow = 1)
dev.off()












#---- OLD -  Explore what the results would have looked like if we combined parametric and process uncertainty to one prob value ######


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










#---- OLD - exploring beta distributions  ----
#### Explore the problem with the beta distribution and too big SDs... 

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


# BASE CASE info
inputs_all <- dapva4nlf::getNLFIdahoFeasinputs()
inputs <- inputs_all[[1]]
parameterByIterTracking_baseCase <- selectNLFIdahoParameterByIterTracking(inputs, base_case = TRUE)
parameterByIterTracking <- parameterByIterTracking_baseCase

# Example one - base case parameter draw, no threat survival for eggs

s_eggs_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_eggs_no_threats")])
s_eggs_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_eggs_no_threats")])
s_eggs_no_threats_dist <- dapva::estBetaParams(mean = s_eggs_mean, sd = s_eggs_sd)

pl.beta(s_eggs_no_threats_dist$alpha, 
        s_eggs_no_threats_dist$beta, 
        title = "base case (P50) egg survival, no threats") 

# Example two - base case parameter draw, no threat survival for tadpoles

# BASE CASE - tadpoles, no threats
s_tadpoles_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_tadpoles_no_threats")])
s_tadpoles_sd <- as.numeric(parameterByIterTracking[i, paste0("s_sd_tadpoles_no_threats")])
s_tadpoles_no_threats_dist <- dapva::estBetaParams(mean = s_tadpoles_mean, sd = s_tadpoles_sd)

pl.beta(s_tadpoles_no_threats_dist$alpha, 
        s_tadpoles_no_threats_dist$beta, 
        title = "base case (P50) tadpole survival, no threats") 

# NOTE: funny shape when mean values are too close to 0 or 1, as in tadpole survival
# Explained at https://stats.stackexchange.com/questions/380833/std-dev-should-be-less-than-0-289-help-in-understanding
# One solution might be to put a limit on it so that if the mean survival is a certain level of closeness to 0 or 1 then it just gets assigned the mean
# I think this may be happening in my code anyways but not here where I am plotting it, need to look closer...

# Example to play with - base case parameter draw, no threat survival for tadpoles
s_tadpoles_mean <- as.numeric(parameterByIterTracking[i, paste0("s_mean_tadpoles_no_threats")])
s_tadpoles_sd <- 0.24 #0.01
s_tadpoles_no_threats_dist <- dapva::estBetaParams(mean = s_tadpoles_mean, sd = s_tadpoles_sd)

pl.beta(s_tadpoles_no_threats_dist$alpha, 
        s_tadpoles_no_threats_dist$beta, 
        title = "base case (P50) tadpole survival, no threats") 

# BUT this still works

# However, if the mean is too close to 0 (or 1) then the alpha and beta are negative, which doesn't work


s_tadpoles_mean <- 0.07
s_tadpoles_sd <- 0.2 #0.14 
# Travis and Karen's paper used 10% of the mean; to see what that looks like, use 
# s_tadpoles_sd <- 0.1*s_tadpoles_mean

# dapva::selectPercentileBetaDistribution(
#     mean = s_tadpoles_mean,
#     sd = s_tadpoles_sd,
#     EV_percentile = 0.1
#     )
s_tadpoles_no_threats_dist <- dapva::estBetaParams(mean = s_tadpoles_mean, sd = s_tadpoles_sd)
pl.beta(s_tadpoles_no_threats_dist$alpha, 
        s_tadpoles_no_threats_dist$beta, 
        title = "base case (P50) tadpole survival, no threats") 

int <- dapva::estBetaParams(mean = s_tadpoles_mean, sd = s_tadpoles_sd)
alpha <- int$alpha
beta <- int$beta

EV_percentile <- 0.9
stats::qbeta(EV_percentile, alpha, beta)


# According to the book (https://books.google.ca/books?id=ZRMJ-CebFm4C&pg=PA83&dq=&redir_esc=y#v=onepage&q&f=false),
# technically non-negative alpha and beta are allowed if you are confident it is really close to 0 or 1. BUT
# the qbeta function does not allow it.


# BEta distribution for visualizations - egg survival
mean <- 0.04
sd <- .07 #0.07 #.1  #0.14    # 0.001 is too small, 0.2 is too big
dist <- dapva::estBetaParams(mean = mean, sd = sd)

pl.beta(dist$alpha, 
        dist$beta, 
        title = "beta distribution2") 


stats::qbeta(0.9, dist$alpha, dist$beta)
stats::qbeta(0.1, dist$alpha, dist$beta)

stats::qbeta(0.99, dist$alpha, dist$beta)
stats::qbeta(0.01, dist$alpha, dist$beta)




# BEta distribution for visualizations - tadpole survival
mean <- 0.04
sd <- .08 #use .06 as best guess, .05 is reasonable, 0.07 is good, 0.14 not as good, .01 seems narrow, ok as a low plausible, .1 seems too big - ok to as upper bound
dist <- dapva::estBetaParams(mean = mean, sd = sd)

pl.beta(dist$alpha, 
        dist$beta, 
        title = "beta distribution2") 


stats::qbeta(0.9, dist$alpha, dist$beta)
stats::qbeta(0.1, dist$alpha, dist$beta)

stats::qbeta(0.99, dist$alpha, dist$beta)
stats::qbeta(0.01, dist$alpha, dist$beta)



# BEta distribution for visualizations - tadpole survival
mean <- 0.04
sd <- .08 #use .06 as best guess, .05 is reasonable, 0.07 is good, 0.14 not as good, .01 seems narrow, ok as a low plausible, .1 seems too big - ok to as upper bound
dist <- dapva::estBetaParams(mean = mean, sd = sd)

pl.beta(dist$alpha, 
        dist$beta, 
        title = "beta distribution2") 


stats::qbeta(0.9, dist$alpha, dist$beta)
stats::qbeta(0.1, dist$alpha, dist$beta)

stats::qbeta(0.99, dist$alpha, dist$beta)
stats::qbeta(0.01, dist$alpha, dist$beta)


# BEta distribution for visualizations - yoy survival
mean <- 0.1
sd <- 0.001 # 0.001 is absolute low, 0.04 is a good low guess, 0.07 is a good best guess, 0.1 is a good high, 0.14 seems a little high - ok for the upper bound
dist <- dapva::estBetaParams(mean = mean, sd = sd)

pl.beta(dist$alpha, 
        dist$beta, 
        title = "beta distribution2") 


stats::qbeta(0.9, dist$alpha, dist$beta)
stats::qbeta(0.1, dist$alpha, dist$beta)

stats::qbeta(0.99, dist$alpha, dist$beta)
stats::qbeta(0.01, dist$alpha, dist$beta)


# BEta distribution for visualizations - juv survival
mean <- 0.4
sd <- 0.01 # 0.05  # 0.04 as low, 0.1 as high, 0.07  as best guess
dist <- dapva::estBetaParams(mean = mean, sd = sd)

pl.beta(dist$alpha, 
        dist$beta, 
        title = "beta distribution2") 


stats::qbeta(0.9, dist$alpha, dist$beta)
stats::qbeta(0.1, dist$alpha, dist$beta)

stats::qbeta(0.99, dist$alpha, dist$beta)
stats::qbeta(0.01, dist$alpha, dist$beta)


# BEta distribution for visualizations - prop that lay eggs, A2
mean <- 0.75
sd <- .07 # 0.07 as best guess, 0.05 as low plausible   0.14 is too wide, 0.1 is a reasonable high, 0.12 as absolute max, 0.01 is too narrow - ok for lower bound
dist <- dapva::estBetaParams(mean = mean, sd = sd)

pl.beta(dist$alpha, 
        dist$beta, 
        title = "beta distribution2") 


stats::qbeta(0.9, dist$alpha, dist$beta)
stats::qbeta(0.1, dist$alpha, dist$beta)

stats::qbeta(0.99, dist$alpha, dist$beta)
stats::qbeta(0.01, dist$alpha, dist$beta)

# BEta distribution for visualizations - prop that lay eggs, A3 and A4
mean <- 0.9
sd <- .12 # 0.05 as best guess, 0.09 for plausible high, 0.03 for plausible low, .01 for absolute low, 0.12 reasonable absolute max
dist <- dapva::estBetaParams(mean = mean, sd = sd)

pl.beta(dist$alpha, 
        dist$beta, 
        title = "beta distribution2") 


stats::qbeta(0.9, dist$alpha, dist$beta)
stats::qbeta(0.1, dist$alpha, dist$beta)

stats::qbeta(0.99, dist$alpha, dist$beta)
stats::qbeta(0.01, dist$alpha, dist$beta)


#---- OLD - Investigating why some perist and others don't   ----

# Ran one iteration  - aftert 100 runs, this parameter draw has a prob of persis of 26% or something like that

path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results/Investigating"# on my work PC
setwd(path_to_results_folder) # on my mac


file <-  list.files(path = ".","*investigating_why_some_persist_May292021.RData", full.names="TRUE")
load(file)


# start with results_all_for_this_iteration

# Remove eggs and tadpoles as they are intermediate stages in the year and we just want the pop size at the fall census
results_all_for_this_iteration_fall <- results_all_for_this_iteration # initalize
results_all_for_this_iteration_fall[which(results_all_for_this_iteration_fall$class == "eggs"),paste(1:yrs)] <- 0
results_all_for_this_iteration_fall[which(results_all_for_this_iteration_fall$class == "tadpoles"),paste(1:yrs)] <- 0



# Identify which runs persist and which don;t
  
  # from code fin dapva::makeResultsSummaryOneIteration
  results_total_by_run <- results_all_for_this_iteration_fall %>%
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
  
  runs_that_persist <- test$run[which(test$year == 50 & test$number_of_indiv > 0)]

# flag the runts that persist in the overall tracking
  results_all_for_this_iteration_fall$persist <- "no" # initialize
  results_all_for_this_iteration_fall$persist[is.na(match(results_all_for_this_iteration_fall$run, runs_that_persist)) == FALSE] <- "yes"
  
  
  results_total_by_run <- results_all_for_this_iteration_fall %>%
    dplyr::group_by(run) %>%
    dplyr::summarise(dplyr::across(paste(initial_year:(initial_year + yrs - 1)), sum))
  results_total_by_run$metric <- "number_of_indiv"
  
  test <- melt(results_all_for_this_iteration_fall, id.vars=c("iteration", "run", "pop", "class", "sex", "persist"))
  colnames(test)[which(colnames(test) == "variable")]<- "year"
  colnames(test)[which(colnames(test) == "value")]<- "number_of_indiv"
  test$year <- as.numeric(as.character(test$year))

  ggplot2::ggplot(test[which(test$class == "yoy"),], ggplot2::aes(x = year, y = number_of_indiv, group = run, color = run)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::facet_grid(class~persist) + 
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
  
  
  
  test2 <- results_all_for_this_iteration_fall[which(results_all_for_this_iteration_fall$persist == "yes" &
                                                       results_all_for_this_iteration_fall$class == "yoy" ),]
  
  
  test2[, "max"] <- apply(test2[, 7:55], 1, max)
  test2$max
  
  test3 <- results_all_for_this_iteration_fall[which(results_all_for_this_iteration_fall$persist == "no" &
                                                       results_all_for_this_iteration_fall$class == "yoy" ),]
  
  
  test3[, "max"] <- apply(test3[, 7:55], 1, max)
  test3$max

  
  # The key seems to be having at least one? good year for yoy survival, this sets the age structure up for success
  # Actually, really depends on the overall parameters. I don't think there is one rule of thumb like this necesarily
  # Maybe could look at it as if the parameters for frogs are good overall, what is the age structure?
  
  
  
  
  
###### Exploring why one wetland now comes out better than regular GoBig ####
  
  # No epehemeral wetlands is better than GoBig because overall the ephemeral wetlands aren't helpful
  # Compare with noEphWetlands
  
  # Flying Bars
  
  # goBig_alt_name  <- "Go Big or Go Home "
  oneWetland_alt_name  <- "Try Hard at One Wetland"
  noHabRest_alt_name  <- "Try Hard but No Habitat Restoration" 
  
  int9 <- results_all_iter[c(which(results_all_iter$alternative == oneWetland_alt_name),
                             which(results_all_iter$alternative == noHabRest_alt_name)),]
  # int9$alternative[which(int6$alternative == goBig_alt_name)] <- "Go Big or Go Home" # get rid of the extra space
  int9$alternative[which(int9$alternative == noHabRest_alt_name)] <- "Try Hard but \n No Habitat Restoration" # put it on two lines
  
  
  int9$alternative <- factor(int9$alternative, levels=c("Try Hard but \n No Habitat Restoration",
                                                       "Try Hard at One Wetland")) # reorder factor levels
  
  
  (persistence_oneWetInv_flyingBars1 <- graphFlyingBars(results_summary_all_iterations = int9,
                                                       metric = "probability of persistence",
                                                       year = 50,
                                                       credible_interval = 0.95,
                                                       x_axis_lab = "Probability of Persistence in Year 50",
                                                       y_axis_lab = "\n Management Alternative",
                                                       # title = 'B)'))
                                                       title = ''))
  
###### Trying to figure out One Wetland
  # Look at compared with wetland correlation/similarity
  path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results"# on my work PC
  #path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/BTPD_PVA/Results/BTPD_baseline_results_march17"# on my mac
  setwd(path_to_results_folder) # on my mac
  file_oneWet <-  list.files(path = ".","*goBigOneWetland_vFinalJune2021_2halfK.RData", full.names="TRUE")
  load(file_oneWet)
    file_goBig <-  list.files(path = ".","*goBig_vFinalJune2021_5K.RData", full.names="TRUE")
    load(file_goBig)
  
  n_iter <- nrow(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])
  
  # Look at with just effective epehemeral wetlands
  test9 <- as.data.frame(cbind(parameterByIterTracking_this_alt_clean[1:n_iter, c("wetland_eggTadSurv_TempCor_noEph", "ephWetRest_effective", 
                                                                                  "s_mean_tadpoles_no_threats", "s_mean_yoy_no_threats",
                                                                                  "s_mean_ephWetlands_tadpoles_no_threats")],
                 as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[,]))
  colnames(test9) <- c("wetland_eggTadSurv_TempCor_noEph", "ephWetRest_effective", 
                       "s_mean_tadpoles_no_threats", "s_mean_yoy_no_threats",
                       "s_mean_ephWetlands_tadpoles_no_threats",
                       "prob_persist")
  
  plot(test9$wetland_eggTadSurv_TempCor_noEph, test9$prob_persist)
  
  test9$wetland_eggTadSurv_TempCor_noEph_cat <- 'wetland_eggTadSurv_TempCor_noEph less than P50' # initialize
  test9$wetland_eggTadSurv_TempCor_noEph_cat [which(test9$wetland_eggTadSurv_TempCor_noEph > quantile(test9$wetland_eggTadSurv_TempCor_noEph, 0.5))] <- '> P50'
  test9$wetland_eggTadSurv_TempCor_noEph_cat <- as.factor(test9$wetland_eggTadSurv_TempCor_noEph_cat)
  
  test9$s_mean_tadpoles_no_threats_cat <- 's_mean_tadpoles_no_threats less than P50' # initialize
  test9$s_mean_tadpoles_no_threats_cat [which(test9$s_mean_tadpoles_no_threats > quantile(test9$s_mean_tadpoles_no_threats, 0.5))] <- 'tad surv > P50'
  test9$s_mean_tadpoles_no_threats_cat <- as.factor(test9$s_mean_tadpoles_no_threats_cat)
  
  test9$s_mean_yoy_no_threats_cat <- 's_mean_yoy_no_threats less than P50' # initialize
  test9$s_mean_yoy_no_threats_cat [which(test9$s_mean_yoy_no_threats > quantile(test9$s_mean_yoy_no_threats, 0.5))] <- 'yoy surv > P50'
  test9$s_mean_yoy_no_threats_cat <- as.factor(test9$s_mean_yoy_no_threats_cat)
  
  test9$s_mean_ephWetlands_tadpoles_no_threats_cat <- 's_mean_ephWetlands_tadpoles_no_threats less than P50' # initialize
  test9$s_mean_ephWetlands_tadpoles_no_threats_cat [which(test9$s_mean_ephWetlands_tadpoles_no_threats > quantile(test9$s_mean_ephWetlands_tadpoles_no_threats, 0.5, na.rm = TRUE))] <- 's_mean_ephWetlands_tadpoles_no_threats > P50'
  test9$s_mean_ephWetlands_tadpoles_no_threats_cat <- as.factor(test9$s_mean_ephWetlands_tadpoles_no_threats_cat)
  
  ggplot2::ggplot(test9, ggplot2::aes(x = wetland_eggTadSurv_TempCor_noEph, prob_persist)) +
    ggplot2::geom_point() + 
    ggplot2::geom_smooth(method = "glm")+ 
    # ggplot2::facet_grid(s_mean_tadpoles_no_threats_cat~wetland_eggTadSurv_TempCor_noEph_cat) 
     # ggplot2::facet_wrap(~s_mean_tadpoles_no_threats_cat)
   ggplot2::facet_grid(s_mean_tadpoles_no_threats_cat~s_mean_yoy_no_threats_cat) 
  
  # I don't think the correlation of the weltnads is a big factor here
  # But for One wetland, prob persist when both yoy and tad is < P50 is almost 0, higher with Go Big (Trhree wetlands)
  # For One Wetland, both > P50 is also higher than Go Big
  # Think about that, maybe a clue, think about how to look at this more
  # Maybe plot the two histograms to at least show this
  # Maybe because with three wetlands morelikely one will take, so higher than one wetland if things are not goo
  # But if things are good, then when you put them in one wetland it can really take off...
  
  test10 <- test9 # initalize
  test10$s_mean_tad_yoy_cat <- "s mean tad and yoy - one or both less than P50"
  test10$s_mean_tad_yoy_cat[intersect(which(test10$s_mean_yoy_no_threats > quantile(test10$s_mean_yoy_no_threats, 0.5)),
                                    which(test10$s_mean_tadpoles_no_threats > quantile(test10$s_mean_tadpoles_no_threats, 0.5)))] <- "s mean tad and yoy - both > than P50"
  
  
  ggplot2::ggplot(test10[which(test10$ephWetRest_effective == "yes"),], ggplot2::aes(x = wetland_eggTadSurv_TempCor_noEph, prob_persist)) +
    ggplot2::geom_point() + 
    ggplot2::geom_smooth(method = "glm")+ 
    # ggplot2::facet_grid(s_mean_tadpoles_no_threats_cat~wetland_eggTadSurv_TempCor_noEph_cat) 
    # ggplot2::facet_wrap(~s_mean_tadpoles_no_threats_cat)
    ggplot2::facet_grid(s_mean_ephWetlands_tadpoles_no_threats_cat~s_mean_tad_yoy_cat) 
  
  
  
  rows_sim <- which(test9$wetland_eggTadSurv_TempCor_noEph > quantile(test9$wetland_eggTadSurv_TempCor_noEph, 0.8))
  rows_notSim <-  which(test9$wetland_eggTadSurv_TempCor_noEph < quantile(test9$wetland_eggTadSurv_TempCor_noEph, 0.2))
  
  # Flying Bars
  (persistence_oneWet_flyingBars1 <- graphFlyingBars(results_summary_all_iterations = results_all_this_alt[rows_notSim,],
                                                     metric = "probability of persistence",
                                                     year = 50,
                                                     credible_interval = 0.95,
                                                     x_axis_lab = "Probability of Persistence in Year 50",
                                                     y_axis_lab = "\n Management Alternative",
                                                     # title = 'B)'))
                                                     title = ''))
  
  
  
  
  
  ## OLD
  
  path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results"# on my work PC
  #path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/BTPD_PVA/Results/BTPD_baseline_results_march17"# on my mac
  setwd(path_to_results_folder) # on my mac
  file_oneWet <-  list.files(path = ".","*goBigOneWetland_vFinalJune2021.RData", full.names="TRUE")
  file_oneWet <-  list.files(path = ".","*goBig_vFinalJune2021.RData", full.names="TRUE")
  load(file_oneWet)
  
  n_iter <- nrow(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])

  # Look at with just effective epehemeral wetlands
  test9 <- cbind(parameterByIterTracking_this_alt_clean[1:n_iter, c("ephWetRest_effective", "ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats")],
                 as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[,])
  colnames(test9) <- c("ephWetRest_effective","ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  
  
  rows_eff <- which(test9$ephWetRest_effective == "yes")
  rows_notEff <- which(test9$ephWetRest_effective == "no")
  
  # Flying Bars
  (persistence_oneWet_flyingBars1 <- graphFlyingBars(results_summary_all_iterations = results_all_this_alt[rows_notEff,],
                                                       metric = "probability of persistence",
                                                       year = 50,
                                                       credible_interval = 0.95,
                                                       x_axis_lab = "Probability of Persistence in Year 50",
                                                       y_axis_lab = "\n Management Alternative",
                                                       # title = 'B)'))
                                                       title = ''))
  
  
  
  
############ Other investigation  ################
  
  
  # iteration 75 has the highest temporal variation for tadpole survival
  which(parameterByIterTracking$s_sd_tadpoles_no_threats == max(parameterByIterTracking$s_sd_tadpoles_no_threats))
  
  test_max <- results_all_iterations[which(results_all_iterations$iteration == 75), ]
  test_max2 <- results_summary_all_iterations_overall[which(results_summary_all_iterations_overall$iteration == 75), ]
  test_max3 <- parameterByIterTracking[75,]

  which(parameterByIterTracking$s_sd_tadpoles_no_threats == min(parameterByIterTracking$s_sd_tadpoles_no_threats))
  which(parameterByIterTracking$s_sd_tadpoles_no_threats <= 0.012) # 320
  
  test_min <- results_all_iterations[which(results_all_iterations$iteration == 302), ]
  test_min2 <- results_summary_all_iterations_overall[which(results_summary_all_iterations_overall$iteration == 302), ]
  test_min3 <- parameterByIterTracking[320,]
  

  
  
  # Here was using the RData file goBig_v1test14testephWet
  
  # If use a different one, first subset the data to include only those iterations where the epehemeral wetland restoration was effective (ephWetRest_effective == 'yes)
  
## What is going on with the ephemeral wetlands?
  # Takeaways:
  #1) It better to have good conditions in the ephemeral wetlands than poor conditions, regardless of drying frequency.
  # This is good from a model debugging perspective – consistent with what we expect. 
  #2) If tadpole survival is worse than in the main wetlands (right side of panel below) OR drying frequency is high (top of panel below),
  # the ephemeral wetlands have sink-like qualities (i.e. eggs that are laid there are likely to not survive due to a drying event). Therefore, 
  # in these cases, more drying frequency is better because then yoy don’t disperse there. But if the epehemeral wetlands are better than the main 
  # wetlands for tadpole survival AND the wetlands do not dry up as often as the best guess, then a smaller drying frequency is better.
  
  # Visual for 1:
  test5 <- cbind(parameterByIterTracking_this_alt_clean[, c("ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats")],
                 as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[,])
  colnames(test5) <- c("ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  
  testglm3 <- glm(prob_persist ~ ephemeral_freq_dry + s_mean_ephWetlands_tadpoles_no_threats + ephemeral_freq_dry*s_mean_ephWetlands_tadpoles_no_threats, 
                  # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
                  family = quasibinomial, data =  test5)
  
  summary(testglm3)
  library(visreg)
  visreg(testglm3 , "s_mean_ephWetlands_tadpoles_no_threats", by="ephemeral_freq_dry")
  
  
  # Helpful visual for 2:

  test6 <- cbind(parameterByIterTracking_this_alt_clean[, c("ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats", "s_mean_tadpoles_no_threats")],
                 as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[,])

  colnames(test6) <- c("ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "s_mean_tadpoles_no_threats", "prob_persist")
  test6$ephSurvBetterThanMain <- NA # initialize
  test6$ephSurvBetterThanMain[which(test6$s_mean_ephWetlands_tadpoles_no_threats > test6$s_mean_tadpoles_no_threats)] <- 'eph wetlands better than main for tadpoles'
  test6$ephSurvBetterThanMain[which(test6$s_mean_ephWetlands_tadpoles_no_threats < test6$s_mean_tadpoles_no_threats)] <- 'eph wetlands worse than main for tadpoles'
  test6$ephSurvBetterThanMain <- as.factor(test6$ephSurvBetterThanMain)
  
  test6$main_surv_better_P50 <- 'no' # initialize
  test6$main_surv_better_P50[which(test6$s_mean_tadpoles_no_threats > quantile(test6$s_mean_tadpoles_no_threats, 0.5))] <- 'yes - main better than P50'
  test6$main_surv_better_P50 <- as.factor(test6$main_surv_better_P50)
  
  test6$ephemeral_freq_dry_cat <- 'eph wetland less often than P50' # initialize
  test6$ephemeral_freq_dry_cat [which(test6$ephemeral_freq_dry > quantile(test6$ephemeral_freq_dry, 0.5))] <- 'eph wetland dry more often than P50'
  test6$ephemeral_freq_dry_cat <- as.factor(test6$ephemeral_freq_dry_cat)

  ggplot(test6, aes(x = ephemeral_freq_dry, prob_persist)) +
    geom_point() + 
    geom_smooth(method = "glm") + 
    facet_grid(ephemeral_freq_dry_cat~ephSurvBetterThanMain)
  
  # Of all of the iterations, how many are in the bottom left quadrant (i.e. where the less frequent dry events are better because it is not a sink)
  num_in_bottom_left_quad <- length(intersect(which(test6$ephemeral_freq_dry_cat == 'eph wetland less often than P50'),
            which(test6$ephSurvBetterThanMain == 'eph wetlands better than main for tadpoles')))
  
  num_total <- dim(test6)[1]
  
  (proportion_in_bottom_left_quad <- num_in_bottom_left_quad/num_total)
  
  # # Other old exploratio nof this
  # # For ephemeral wetlands test, why is freq of drying better? But also having 
  # # better survival in the epehemral wetalnds is also better?
  # 
  # test <- results_summary_all_iterations_by_pop
  # 
  # parameterByIterTracking_this_alt_clean
  # 
  # rows <- 1:dim(parameterByIterTracking_this_alt_clean)[1]
  # 
  # test4 <- cbind(parameterByIterTracking_this_alt_clean[rows, c("ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[rows,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test4) <- c("ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm2 <- glm(prob_persist ~ ephemeral_freq_dry + s_mean_ephWetlands_tadpoles_no_threats + ephemeral_freq_dry*s_mean_ephWetlands_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test4)
  # 
  # summary(testglm2)
  # library(visreg)
  # 
  # # interaction is only significant when limit rows also to tadpole means below the P50
  # # but even with all the tadpole mean data the trends look similar when we look at the grah
  # # for large mean survival, sd doesn't matter
  # # for small mean survival, larger standard deviation is worse for prob of persistence
  # 
  # visreg(testglm2 , "ephemeral_freq_dry", by="s_mean_ephWetlands_tadpoles_no_threats")
  # visreg(testglm2 , "s_mean_ephWetlands_tadpoles_no_threats", by="ephemeral_freq_dry")
  # 
  # 
  # 
  # test5 <- cbind(parameterByIterTracking_this_alt_clean[rows, c("s_mean_tadpoles_no_threats", "s_mean_ephWetlands_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[rows,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test5) <- c("s_mean_tadpoles_no_threats","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm3 <- glm(prob_persist ~ s_mean_tadpoles_no_threats + s_mean_ephWetlands_tadpoles_no_threats + s_mean_tadpoles_no_threats*s_mean_ephWetlands_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test5)
  # 
  # summary(testglm3)
  # library(visreg)
  # 
  # # interaction is only significant when limit rows also to tadpole means below the P50
  # # but even with all the tadpole mean data the trends look similar when we look at the grah
  # # for large mean survival, sd doesn't matter
  # # for small mean survival, larger standard deviation is worse for prob of persistence
  # 
  # visreg(testglm3 , "s_mean_tadpoles_no_threats", by="s_mean_ephWetlands_tadpoles_no_threats")
  # visreg(testglm3 , "s_mean_ephWetlands_tadpoles_no_threats", by="s_mean_tadpoles_no_threats")
  # # Interp: when tadpole survival is high in the main wetlands, it helps to also have good tadpole survival 
  # # in the ephemeral wetlands. But if survival is low in the main wetlands then the ephemeral wetlands don't really have an effect on persistence at all.
  # 
  # 
  # 
  # # Try filtering out only situations where they go dry alot
  # rows_ephDryAlot <- which(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry >= quantile(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry, 0.9))
  # rows_ephDryRarely <- which(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry < quantile(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry, 0.1))
  # # rows <- intersect(rows_yoybigenough , rows_tadsmallenough )
  # 
  # rows <- rows_ephDryRarely# rows_ephDryAlot 
  # 
  # test5 <- cbind(parameterByIterTracking_this_alt_clean[rows, c("s_mean_tadpoles_no_threats", "s_mean_ephWetlands_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[rows,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test5) <- c("s_mean_tadpoles_no_threats","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm3 <- glm(prob_persist ~ s_mean_tadpoles_no_threats + s_mean_ephWetlands_tadpoles_no_threats + s_mean_tadpoles_no_threats*s_mean_ephWetlands_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test5)
  # 
  # summary(testglm3)
  # library(visreg)
  # 
  # # interaction is only significant when limit rows also to tadpole means below the P50
  # # but even with all the tadpole mean data the trends look similar when we look at the grah
  # # for large mean survival, sd doesn't matter
  # # for small mean survival, larger standard deviation is worse for prob of persistence
  # 
  # visreg(testglm3 , "s_mean_tadpoles_no_threats", by="s_mean_ephWetlands_tadpoles_no_threats")
  # visreg(testglm3 , "s_mean_ephWetlands_tadpoles_no_threats", by="s_mean_tadpoles_no_threats")
  # 
  # # When the epehemeral wetlands are rarely dry, it is more helpful to have high tadpole survival in the epehemeral wetlands
  # 
  # # Try looking specificually at the situation where the main wetlands have low tadpole survival
  # rows_mainTadSurvLow <- which(parameterByIterTracking_this_alt_clean$s_mean_tadpoles_no_threats < quantile(parameterByIterTracking_this_alt_clean$s_mean_tadpoles_no_threats, 0.5))
  # 
  # rows <- rows_mainTadSurvLow 
  # 
  # test4 <- cbind(parameterByIterTracking_this_alt_clean[rows, c("ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "50"])[rows,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test4) <- c("ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm2 <- glm(prob_persist ~ ephemeral_freq_dry + s_mean_ephWetlands_tadpoles_no_threats + ephemeral_freq_dry*s_mean_ephWetlands_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test4)
  # 
  # summary(testglm2)
  # library(visreg)
  # 
  # # interaction is only significant when limit rows also to tadpole means below the P50
  # # but even with all the tadpole mean data the trends look similar when we look at the grah
  # # for large mean survival, sd doesn't matter
  # # for small mean survival, larger standard deviation is worse for prob of persistence
  # 
  # visreg(testglm2 , "ephemeral_freq_dry", by="s_mean_ephWetlands_tadpoles_no_threats")
  # visreg(testglm2 , "s_mean_ephWetlands_tadpoles_no_threats", by="ephemeral_freq_dry")
  # 
  # 
  # # Interpretation: when survival in the main wetlands is poor, it is helpful to have good survival in
  # # the ephemeral ewtlands when they are going dry a lot but doesn't really matter if their frequency of going dry is low
  # 
  # 
  # intersect(which(parameterByIterTracking$ephemeral_freq_dry > quantile(parameterByIterTracking$ephemeral_freq_dry, 0.9)), 
  #           which(parameterByIterTracking$s_mean_tadpoles_no_threats > quantile(parameterByIterTracking$s_mean_tadpoles_no_threats, 0.9)))
  # 
  # test2 <- results_all_iterations[which(results_all_iterations$iteration == 13), ]
  # # when dry alot and the mainwetlands are doing poor, the epehemeral wetland rarely get any tadpoles since they don't have anyone dispersing there most of the time, only a few if they do, and then high change of dry again if they do have tadpoles
  # # when dry alot and the mainwetlands are doing well, there are no yoy in the eph wetlands that year (from either tadpoles or dispersal) so they loose a cohort of breeders. Here it is a bit of a sink
  # 
  # intersect(which(parameterByIterTracking$ephemeral_freq_dry < quantile(parameterByIterTracking$ephemeral_freq_dry, 0.1)), 
  #           which(parameterByIterTracking$s_mean_tadpoles_no_threats > quantile(parameterByIterTracking$s_mean_tadpoles_no_threats, 0.9)))
  # 
  # test2 <- results_all_iterations[which(results_all_iterations$iteration == 156), ]
  # # When not dry alot but the main wetlands are doing well, they are still a sink when it goes dry but doesn't really matter
  # 
  # intersect(which(parameterByIterTracking$ephemeral_freq_dry < quantile(parameterByIterTracking$ephemeral_freq_dry, 0.1)), 
  #           which(parameterByIterTracking$s_mean_tadpoles_no_threats < quantile(parameterByIterTracking$s_mean_tadpoles_no_threats, 0.1)))
  # 
  # test2 <- results_all_iterations[which(results_all_iterations$iteration == 54), ]
  # # When not dry alot and main wetlands are doing poorly, then they can be helpful in the short term but by 50 years out doesn't matter
  # # Might be different if we tried releases directly into the ephemeral wetlands to get a pop established in this case
  # 
  # 
  # 
  # 
  # ## Now look at it for results in year 20 - if main wetlands are poor, can the eph wetlands help if they don't dry up often?
  # 
  # rows_mainTadSurvLow <- which(parameterByIterTracking_this_alt_clean$s_mean_tadpoles_no_threats < quantile(parameterByIterTracking_this_alt_clean$s_mean_tadpoles_no_threats, 0.5))
  # 
  # rows <- rows_mainTadSurvLow 
  # 
  # test4 <- cbind(parameterByIterTracking_this_alt_clean[rows, c("ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "20"])[rows,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test4) <- c("ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm2 <- glm(prob_persist ~ ephemeral_freq_dry + s_mean_ephWetlands_tadpoles_no_threats + ephemeral_freq_dry*s_mean_ephWetlands_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test4)
  # 
  # summary(testglm2)
  # library(visreg)
  # 
  # # interaction is only significant when limit rows also to tadpole means below the P50
  # # but even with all the tadpole mean data the trends look similar when we look at the grah
  # # for large mean survival, sd doesn't matter
  # # for small mean survival, larger standard deviation is worse for prob of persistence
  # 
  # visreg(testglm2 , "ephemeral_freq_dry", by="s_mean_ephWetlands_tadpoles_no_threats")
  # visreg(testglm2 , "s_mean_ephWetlands_tadpoles_no_threats", by="ephemeral_freq_dry")
  # 
  # 
  # # Even when tadpole survival in the ephemeral wetlands is good, it is better for the ephemeral wetlands 
  # # to be dry more often because then no one disperses there and is less of a sink?
  # 
  # rows_ephTadSurvHigh <- which(parameterByIterTracking_this_alt_clean$s_mean_ephWetlands_tadpoles_no_threats > quantile(parameterByIterTracking_this_alt_clean$s_mean_ephWetlands_tadpoles_no_threats, 0.5))
  # 
  # rows <- rows_ephTadSurvHigh
  # 
  # test4 <- cbind(parameterByIterTracking_this_alt_clean[rows, c("ephemeral_freq_dry","s_mean_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "20"])[rows,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test4) <- c("ephemeral_freq_dry","s_mean_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm2 <- glm(prob_persist ~ ephemeral_freq_dry + s_mean_tadpoles_no_threats + ephemeral_freq_dry*s_mean_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test4)
  # 
  # summary(testglm2)
  # library(visreg)
  # 
  # # interaction is only significant when limit rows also to tadpole means below the P50
  # # but even with all the tadpole mean data the trends look similar when we look at the grah
  # # for large mean survival, sd doesn't matter
  # # for small mean survival, larger standard deviation is worse for prob of persistence
  # 
  # visreg(testglm2 , "ephemeral_freq_dry", by="s_mean_tadpoles_no_threats")
  # visreg(testglm2 , "s_mean_tadpoles_no_threats", by="ephemeral_freq_dry")
  # 
  # 
  # # Even when tadpole survival in the ephemeral wetlands is good, it is better for the ephemeral wetlands 
  # # to be dry more often because then no one disperses there and is less of a sink?
  # 
  # 
  # # Test the two survivals to see if there is an interaction
  # 
  # rows_ephDryAlot <- which(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry >= quantile(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry, 0.9))
  # rows_ephDryRarely <- which(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry < quantile(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry, 0.1))
  # rows_ephDrymiddle <- intersect(which(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry > quantile(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry, 0.4)),
  #                                which(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry < quantile(parameterByIterTracking_this_alt_clean$ephemeral_freq_dry, 0.6)))
  # 
  # rows <- rows_ephDrymiddle 
  # 
  # test4 <- cbind(parameterByIterTracking_this_alt_clean[rows, c("s_mean_ephWetlands_tadpoles_no_threats","s_mean_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "20"])[rows,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test4) <- c("s_mean_ephWetlands_tadpoles_no_threats","s_mean_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm2 <- glm(prob_persist ~ s_mean_ephWetlands_tadpoles_no_threats + s_mean_tadpoles_no_threats + s_mean_ephWetlands_tadpoles_no_threats*s_mean_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test4)
  # 
  # summary(testglm2)
  # library(visreg)
  # 
  # visreg(testglm2 , "s_mean_ephWetlands_tadpoles_no_threats", by="s_mean_tadpoles_no_threats")
  # # visreg(testglm2 , "s_mean_tadpoles_no_threats", by="s_mean_ephWetlands_tadpoles_no_threats")
  # 
  # # When rarely dry, helpful for tadpole survival to be higher in epehemeral wetlands
  # # When dry alot (or middle amount of), only helpful for tadpole survival to be higher in epehemeral wetlands when things are good in the main wetlands too
  # 
  # 
  # # So when things are good in the main wetlands, expect there to be a reslationshp between tadpole survival in the epehemeral wetlands and freq of drying
  # # where as freq of drying increases higher tadpole survival is helpful
  # 
  # rows_mainTadSurvHigh <- which(parameterByIterTracking_this_alt_clean$s_mean_tadpoles_no_threats > quantile(parameterByIterTracking_this_alt_clean$s_mean_tadpoles_no_threats, 0.8))
  # 
  # rows <- rows_mainTadSurvHigh
  # 
  # test4 <- cbind(parameterByIterTracking_this_alt_clean[rows, c("ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "20"])[rows,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test4) <- c("ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm2 <- glm(prob_persist ~ ephemeral_freq_dry + s_mean_ephWetlands_tadpoles_no_threats + ephemeral_freq_dry*s_mean_ephWetlands_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test4)
  # 
  # summary(testglm2)
  # library(visreg)
  # 
  # visreg(testglm2 , "ephemeral_freq_dry", by="s_mean_ephWetlands_tadpoles_no_threats")
  # # visreg(testglm2 , "s_mean_ephWetlands_tadpoles_no_threats", by="ephemeral_freq_dry")
  # 
  # # If things are good in the main wetlands and good in the ephemeral wetlands, then worse if dry more often
  # # If things are good in the main wetlands and not as good in the epehemeral wetlands, doesn't really matter.
  # 
  # # And when things are not good in the main wetlands, same thing - worse if dry more and things are good in that wetland but doesn't matter if things are bad.
  # 
  # rows_mainTadSurvLow<- which(parameterByIterTracking_this_alt_clean$s_mean_tadpoles_no_threats <= quantile(parameterByIterTracking_this_alt_clean$s_mean_tadpoles_no_threats, 0.2))
  # 
  # rows2 <- rows_mainTadSurvLow
  # 
  # test4 <- cbind(parameterByIterTracking_this_alt_clean[rows2, c("ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "20"])[rows2,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test4) <- c("ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm2 <- glm(prob_persist ~ ephemeral_freq_dry + s_mean_ephWetlands_tadpoles_no_threats + ephemeral_freq_dry*s_mean_ephWetlands_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test4)
  # 
  # summary(testglm2)
  # library(visreg)
  # 
  # visreg(testglm2 , "ephemeral_freq_dry", by="s_mean_ephWetlands_tadpoles_no_threats")
  # # visreg(testglm2 , "s_mean_ephWetlands_tadpoles_no_threats", by="ephemeral_freq_dry")
  # 
  # # Now for all rows
  # 
  # test4 <- cbind(parameterByIterTracking_this_alt_clean[, c("ephemeral_freq_dry", "s_mean_ephWetlands_tadpoles_no_threats")],
  #                as.data.frame(results_all_this_alt[which(results_all_this_alt$metric == "probability of persistence"), "20"])[,])
  # # colnames(test3) <- c("s_tadpoles_mean", "prob_persist")
  # colnames(test4) <- c("ephemeral_freq_dry","s_mean_ephWetlands_tadpoles_no_threats", "prob_persist")
  # 
  # 
  # 
  # testglm2 <- glm(prob_persist ~ ephemeral_freq_dry + s_mean_ephWetlands_tadpoles_no_threats + ephemeral_freq_dry*s_mean_ephWetlands_tadpoles_no_threats, 
  #                 # family = binomial, data =  test3) # warning is ok, try quasibinomial instead to be sure
  #                 family = quasibinomial, data =  test4)
  # 
  # summary(testglm2)
  # library(visreg)
  # 
  # visreg(testglm2 , "ephemeral_freq_dry", by="s_mean_ephWetlands_tadpoles_no_threats")
  # # visreg(testglm2 , "s_mean_ephWetlands_tadpoles_no_threats", by="ephemeral_freq_dry")
