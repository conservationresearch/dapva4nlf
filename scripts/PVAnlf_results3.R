# This script builds of the PVAnlf_results2 script, which summarized the results
# for the 2021 SDM report, to create figures for the corresponding manuscript.

# Recall that the PVAnlf_results2 script uses the Rdata files saved as the 
# output from running the NLF Idaho Feasibility PVA
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
library(cowplot) # for plot_grid


#---- Specify the location where you saved the Rdata files. ----
# Note: Add a folder in there called 'ForReport' to help organize the outputs.

# Increase computer memory size before loading the RData files
#memory.limit(24000) # for windows computers only
#path_to_results_folder <- "C:/Users/LauraK/The Calgary Zoological Society/Conservation Research - NLF feas. ID/SDM 2021/model_results"# on my work PC
path_to_results_folder <- "/Users/laurakeating/Documents/R/R_scripts/NLF_PVA/Results/"# on my mac
setwd(path_to_results_folder) # on my mac

#---- Specify the number of years you ran the model for/want to see results for. ----
yrs <- 50
version <- "_March2022_manuscript"

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
                                       title = '',
                                       x_axis_lab = "Year",
                                       y_axis_lab = "Probability of Persistence "))


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
int2$alternative[which(int2$alternative == goBig_alt_name)] <- "Go Big or\n Go Home" # get rid of the extra space
int2$alternative[which(int2$alternative == lowEffort_alt_name)] <- "Minimum \n Funding /\n Low Effort" # put it on two lines
int2$alternative[which(int2$alternative == mostReal_alt_name)] <- "Middle of\n the Road" # put it on two lines

int2$alternative <- factor(int2$alternative, levels=c("Go Big or\n Go Home", "Middle of\n the Road",
                                                    "Minimum \n Funding /\n Low Effort",
                                                    "Do Nothing" )) # reorder factor levels

(persistence_effort_flyingBars1 <- dapva::graphBoxandViolinPlot(results_summary_all_iterations = int2,
                                            metric = "probability of persistence",
                                            year = yrs,
                                            credible_interval = 0.95,
                                            x_axis_lab = "Probability of Persistence\nin Year 50",
                                            y_axis_lab = "Management Alternative",
                                            # title = 'B)'))
                                            title = ''))


# cumulative distribution function - to look at stochastic dominance
(persistence_effort_CDF1 <- dapva::graphCDF(results_summary_all_iterations = int2,
         metric = "probability of persistence",
         year = yrs,
         x_axis_lab = "Probability of Persistence\nin Year 50",
         y_axis_lab = "Cumulative Probability",
         title = ''))

(persistence_effort_PDF1 <- dapva::graphPDF(results_summary_all_iterations = int2,
                                     metric = "probability of persistence",
                                     year = yrs,
                                     x_axis_lab = "Probability of Persistence\nin Year 50",
                                     title = ''))

# What if top shows just the box and whisker charts
# Then the next shows the violin plots for just Middle of the Road and GO Big
# Then have the CDFs on the bottom

(persistence_effort_flyingBars1_opt2<- dapva::graphFlyingBars(results_summary_all_iterations = int2,
                                                                metric = "probability of persistence",
                                                                year = yrs,
                                                                credible_interval = 0.95,
                                                                x_axis_lab = "Probability of Persistence\nin Year 50",
                                                                y_axis_lab = "Management Alternative",
                                                                # title = 'B)'))
                                                                title = ''))

# (persistence_effort_violinPlot_opt2 <- dapva::graphViolinPlotSideways(results_summary_all_iterations = int2[which(int2$alternative != "Minimum Funding /\n Low Effort"  &
#                                                                                                                int2$alternative != "Do Nothing"),],
#                                                            metric = "probability of persistence",
#                                                            year = yrs,
#                                                            credible_interval = 0.95,
#                                                            x_axis_lab = "Probability of Persistence in Year 50",
#                                                            y_axis_lab = "\n Management Alternative",
#                                                            title = 'C)'))


# Doing manually so can get the colors to match the ones with all four alternatives
x_axis_lab1 <- ggplot2::labs(y = "Probability of Persistence\nin Year 50")
y_axis_lab1 <- ggplot2::labs(x = "Management Alternative")
title1 <- ggplot2::ggtitle(paste0(''))

# int2b <- int2[which(int2$alternative != "Minimum Funding /\n Low Effort"  &
#                       int2$alternative != "Do Nothing"),]

#int2b <- int2[which( int2$alternative != "Do Nothing"),]
int2b <- int2

results_summary_all_iterations_year <- int2b[which(int2b$metric == "probability of persistence"), 
                                                                      c(paste0(50), "alternative", "pop")]
colnames(results_summary_all_iterations_year) <- c("quantity", 
                                                   "alternative", "pop")

# Add Do Nothing back in



(persistence_effort_violinPlot_opt2 <- ggplot2::ggplot(results_summary_all_iterations_year, 
                ggplot2::aes(x = alternative, y = quantity, col = alternative)) + 
  ggplot2::geom_violin() + 
  ggplot2::stat_summary(fun = median, colour = "blue", geom = "point", shape = 17, size = 3) + 
  ggplot2::stat_summary(fun = mean, colour = "darkred", geom = "point", shape = 18, size = 4) + 
  ggplot2::scale_y_continuous() + 
  ggplot2::scale_color_hue(direction = -1) +
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
)


# CDF

# Adjust the spacing in the alternative names
# int3 <- int2
# int3$alternative <- as.character(int3$alternative)
# #int3$alternative[which(int3$alternative == "Do Nothing")] <- "Do \n Nothing"
# int3$alternative[which(int3$alternative == "Go Big or\n Go Home")] <- "Go Big or Go Home"
# int3$alternative[which(int3$alternative == "Middle of\n the Road")] <- "Middle of the Road"
# int3$alternative[which(int3$alternative == "Minimum \n Funding /\n Low Effort")] <- "Min. Funding / Low Effort"
# int3$alternative <- as.factor(int3$alternative)


# (persistence_effort_CDF1_opt2 <- dapva::graphCDF_labelled(results_summary_all_iterations = int3,
#                                      metric = "probability of persistence",
#                                      year = yrs,
#                                      x_axis_lab = "Probability of Persistence\nin Year 50",
#                                      y_axis_lab = "Cumulative Probability",
#                                      title = '',
#                                      label_y_location = c(0.8, 0.6, 0.4, 0.2),
#                                      label_x_nudge = 0.3,
#                                      label_y_nudge = -0.6)
# )

(persistence_effort_CDF1_opt2 <- dapva::graphCDF(results_summary_all_iterations = int2,
                                                          metric = "probability of persistence",
                                                          year = yrs,
                                                          x_axis_lab = "Probability of Persistence\nin Year 50",
                                                          y_axis_lab = "Cumulative Probability",
                                                          title = '',
                                                 legend_position = "none")
)


#---- Make graphs for the report - level of effort, graphs for export. ----

# filename <- paste("ForReport/graph_effort_time", version,".tiff", sep="")
# tiff(filename, width=12, height=6, units="in",
#      pointsize=8, compression="lzw", bg="white", res=600)
#     # restoreConsole=TRUE)
# grid.arrange(persist_effort_graph1,
#              ncol = 1, nrow = 1)
# dev.off()

# filename <- paste("ForManuscript/graph_effort_year50_option1", version,".pdf", sep="")
# pdf(filename, width=6.5, height=6) # assume 8.5 by 11 page, 1 inch margin on all sides, want fill width and a third of the height
# #filename <- paste("ForManuscript/graph_effort_year50_option2", version,".eps", sep="")
# #postscript(file=filename,horiz=FALSE,onefile=FALSE,width=6.5,height=3)
# cowplot::plot_grid(persist_effort_graph1,
#              # persistence_effort_flyingBars1_opt2, 
#              persistence_effort_violinPlot_opt2, 
#              persistence_effort_CDF1_opt2,
#              ncol = 2, nrow = 2,
#              labels = c("A", "B", "C"))
# dev.off()


# filename <- paste("ForManuscript/graph_effort_year50_option1", version,".eps", sep="")
# #filename <- paste("ForManuscript/graph_effort_year50_option2", version,".eps", sep="")
# #postscript(file=filename,horiz=FALSE,onefile=FALSE,width=6.5,height=3)
# postscript(filename, width = 6.5, height = 6.0,
#            horizontal = FALSE, onefile = FALSE)
# 
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 2, nrow = 2,
#                    labels = c("A", "B", "C"))
# 
# dev.off()

# filename <- paste("ForManuscript/graph_effort_year50_option1", version,".eps", sep="")
# #filename <- paste("ForManuscript/graph_effort_year50_option2", version,".eps", sep="")
# #postscript(file=filename,horiz=FALSE,onefile=FALSE,width=6.5,height=3)
# cairo_ps(filename = filename,
#          width = 6.5, height = 6, pointsize = 12,
#          fallback_resolution = 300)
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 2, nrow = 2,
#                    labels = c("A", "B", "C"))
# 
# dev.off()
# 
# filename <- paste("ForManuscript/graph_effort_year50_option1", version,".eps", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 2, nrow = 2,
#                    labels = c("A", "B", "C"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 6)

# filename <- paste("ForManuscript/graph_effort_year50_option1", version,".png", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 2, nrow = 2,
#                    labels = c("A", "B", "C"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 6)

# filename <- paste("ForManuscript/graph_effort_year50_option1", version,".pdf", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 2, nrow = 2,
#                    labels = c("A", "B", "C"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 6)


# filename <- paste("ForManuscript/graph_effort_year50_option1", version,".pdf", sep="")
# top <- plot_grid(persist_effort_graph1, persistence_effort_violinPlot_opt2, ncol = 2)
# bottom <- plot_grid(persistence_effort_CDF1_opt2, ncol = 1)
# cowplot::plot_grid(plot_grid(top, bottom,
#           ncol=1, rel_heights=c(1,1),
#           labels = c("A", "B", "C")))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 6)



# filename <- paste("ForManuscript/graph_effort_year50_option2", version,".eps", sep="")
# postscript(filename, width = 6.5, height = 3.0,
#            horizontal = FALSE, onefile = FALSE) # assume 8.5 by 11 page, 1 inch margin on all sides, want fill width and a third of the height
# #filename <- paste("ForManuscript/graph_effort_year50_option2", version,".eps", sep="")
# #postscript(file=filename,horiz=FALSE,onefile=FALSE,width=6.5,height=3)
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 3, nrow = 1,
#                    labels = c("A", "B", "C"))
# dev.off()

# filename <- paste("ForManuscript/graph_effort_year50_option2", version,".eps", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 3, nrow = 1,
#                    labels = c("A", "B", "C"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 3.0)
# 
# 
# filename <- paste("ForManuscript/graph_effort_year50_option2", version,".png", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 3, nrow = 1,
#                    labels = c("A", "B", "C"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 3.0)


# filename <- paste("ForManuscript/graph_effort_year50_option3", version,".eps", sep="")
# postscript(filename, width = 3.25, height = 9.0,
#            horizontal = FALSE, onefile = FALSE) # assume 8.5 by 11 page, 1 inch margin on all sides, want fill width and a third of the height
# #filename <- paste("ForManuscript/graph_effort_year50_option2", version,".eps", sep="")
# #postscript(file=filename,horiz=FALSE,onefile=FALSE,width=6.5,height=3)
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 1, nrow = 3,
#                    labels = c("A", "B", "C"))
# dev.off()

# filename <- paste("ForManuscript/graph_effort_year50_option3", version,".pdf", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 1, nrow = 3,
#                    labels = c("A", "B", "C"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 9)
# 
# filename <- paste("ForManuscript/graph_effort_year50_option3", version,".png", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 1, nrow = 3,
#                    labels = c("A", "B", "C"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 9)

# filename <- paste("ForManuscript/graph_effort_year50_option3", version,".png", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    # persistence_effort_flyingBars1_opt2, 
#                    persistence_effort_violinPlot_opt2, 
#                    persistence_effort_CDF1_opt2,
#                    ncol = 1, nrow = 3,
#                    labels = c("A", "B", "C"))
# ggplot2::ggsave(filename = filename, width = 3.25, height = 9)
filename <- paste("ForManuscript/graph_effort_year50_option3", version,".pdf", sep="")
cowplot::plot_grid(persist_effort_graph1,
                   persistence_effort_violinPlot_opt2,
                   persistence_effort_CDF1_opt2,
                   ncol = 1, nrow = 3,
                   labels = c("A", "B", "C"))
ggplot2::ggsave(filename = filename, width = 3.25, height = 9)

# 
# filename <- paste("ForManuscript/graph_effort_year50_option3a", version,".pdf", sep="")
# cowplot::plot_grid(persist_effort_graph1,
#                    ncol = 1, nrow = 1,
#                    labels = c("A"))
# ggplot2::ggsave(filename = filename, width = 3.25, height = 3)
# 
# filename <- paste("ForManuscript/graph_effort_year50_option3b", version,".pdf", sep="")
# cowplot::plot_grid(
#                    persistence_effort_violinPlot_opt2, 
#                    ncol = 1, nrow = 1,
#                    labels = c("B"))
# ggplot2::ggsave(filename = filename, width = 3.25, height = 3)
# 
# filename <- paste("ForManuscript/graph_effort_year50_option3c", version,".pdf", sep="")
# cowplot::plot_grid(
#   persistence_effort_CDF1_opt2,
#   ncol = 1, nrow = 1,
#   labels = c("C"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 3)

#---- Load Go Big results and use that for the remaining. ----
# clear workspace
rm(list=ls())
yrs <- 50
version <- "_March2022_manuscript"

# Load the final Go Big results
file_goBig <-  list.files(path = ".","*goBig_vFinalJuly2021_10Kiter.RData", full.names="TRUE")
load(file_goBig)

#---- Make graphs for the report -goBig tornados, panel for export. ----

# Put some of the labels on two rows
tornado_parameter_labels$label[which(tornado_parameter_labels$label == "tadpole survival in existing wetlands - mean")] <-  "tadpole survival in\nexisting wetlands - mean"
tornado_parameter_labels$label[which(tornado_parameter_labels$label == "bullfrog mgmt. effective (yes = high, no = low)")] <-  "bullfrog mgmt. effective\n(yes = high, no = low)"
tornado_parameter_labels$label[which(tornado_parameter_labels$label == "egg survival in existing wetlands - mean")] <-  "egg survival in existing\nwetlands - mean"
tornado_parameter_labels$label[which(tornado_parameter_labels$label == "egg survival reduction (pct) if bullfrogMgt not effective")] <-  "egg survival reduction if\nbullfrog mgmt. not effective (pct)"
tornado_parameter_labels$label[which(tornado_parameter_labels$label == "adult survival reduction (pct) from roads")] <-  "adult survival reduction\nfrom roads (pct) "
tornado_parameter_labels$label[which(tornado_parameter_labels$label == "proportion of A3 and A4plus lay eggs - mean")] <-  "proportion of A3 and A4plus\nwho lay eggs - mean"
tornado_parameter_labels$label[which(tornado_parameter_labels$label == "yoy survival reduction (pct) from roads")] <-  "yoy survival reduction\nfrom roads (pct)"

# Do the sensitivity analysis
paramSens_persist <- dapva::makeParameterSens(parameterByIterTracking = parameterByIterTracking_this_alt_clean,
                                              results_all_this_alt = results_all_this_alt,
                                              metric = "probability of persistence",
                                              start_year = 1,
                                              nyrs = 50,
                                              parameter_labels = tornado_parameter_labels)

# Draw the associated tornado

tornado_persist_top10 <- dapva::drawTornado(paramSens = paramSens_persist,
                                            metric = "Probability of persistence",
                                            year = 50,
                                            title = "", breaks = 0.2,
                                            num_bars_to_show = 10)
tornado_persist_top10
# Export the tornado diagrams - main report
# filename <- paste("ForManuscript/tornado_top10_goBig", version,".pdf", sep="")
# pdf(filename, width=6.5, height=6)
#      #restoreConsole=TRUE)
# tornado_persist_top10
# dev.off()


# filename <- paste("ForManuscript/tornado_top10_goBig", version,".eps", sep="")
# tornado_persist_top10
# ggplot2::ggsave(filename = filename, width = 6.5, height = 6)

filename <- paste("ForManuscript/tornado_top10_goBig", version,".pdf", sep="")
tornado_persist_top10
ggplot2::ggsave(filename = filename, width = 6.5, height = 4)

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
  ggplot2::xlab("Mean tadpole survival (no threats)") +
  ggplot2::ylab( "Mean young of year \n survival (no threats)") + 
  ggplot2::ggtitle( "") + 
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

mu <- plyr::ddply(test2, "group", plyr::summarise, grp.mean=mean(prob_persist))

p_hist_prob_persist_groups <- ggplot2::ggplot(test2 , ggplot2::aes(x = prob_persist, fill=group)) +
  ggplot2::geom_histogram(position="dodge", binwidth = 0.05, alpha=0.5) + # http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
  ggplot2::scale_color_brewer(palette="Dark2") + 
  ggplot2::scale_fill_brewer(palette="Dark2") + 
  ggplot2::xlab("Probability of persistence") +
  ggplot2::ylab("Number of iterations") + 
  ggplot2::ggtitle( "") + 
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

# filename <- paste("ForManuscript/graph_compare_yoyTadsurv_persist", version,".pdf", sep="")
# pdf(filename, width=6.5, height=6)
# cowplot::plot_grid(p_sens_tad_yoy_surv_persist,  p_hist_prob_persist_groups ,
#                    ncol = 1, nrow = 2,
#                    labels = c("A", "B"))
# dev.off()

# filename <- paste("ForManuscript/graph_compare_yoyTadsurv_persist", version,".eps", sep="")
# cowplot::plot_grid(p_sens_tad_yoy_surv_persist,  p_hist_prob_persist_groups ,
#                    ncol = 1, nrow = 2,
#                    labels = c("A", "B"))
# ggplot2::ggsave(filename = filename, width = 6.5, height = 6)

filename <- paste("ForManuscript/graph_compare_yoyTadsurv_persist", version,".png", sep="")
cowplot::plot_grid(p_sens_tad_yoy_surv_persist,  p_hist_prob_persist_groups ,
                   ncol = 1, nrow = 2,
                   labels = c("A", "B"))
ggplot2::ggsave(filename = filename, width = 6.5, height = 6)

# pdf(filename, width=3.25, height=9) # assume 8.5 by 11 page, 1 inch margin on all sides, want fill width and a third of the height
#filename <- paste("ForManuscript/graph_effort_year50_option2", version,".eps", sep="")
#postscript(file=filename,horiz=FALSE,onefile=FALSE,width=6.5,height=3)




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
 # ggplot2::labs(size='', fill='Bullfrog management effective    ')  +
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
  # ggplot2::labs(size='', fill='Bullfrog management effective    ')  +
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

p_sens_juv_surv_bullfrogMgmt

#---- Export panel graph for report: relationship between bullfrog management and tadpole survival. ----

# Export panel graph for report
# filename <- paste("ForReport/graph_bullfrogMgt_vs_survival", version,".tiff", sep="")
# tiff(filename, width=12, height=8, units="in",
#      pointsize=8, compression="lzw", bg="white", res=600)
#      #restoreConsole=TRUE)
# grid.arrange(p_sens_eggs_surv_bullfrogMgmt, 
#              p_sens_tad_surv_bullfrogMgmt,
#              p_sens_yoy_surv_bullfrogMgmt,
#              p_sens_juv_surv_bullfrogMgmt,
#              ncol = 2, nrow = 2)
# dev.off()


filename <- paste("ForManuscript/graph_bullfrogMgt_vs_survival", version,".png", sep="")
cowplot::plot_grid(p_sens_eggs_surv_bullfrogMgmt, 
                   p_sens_tad_surv_bullfrogMgmt,
                   p_sens_yoy_surv_bullfrogMgmt,
                   p_sens_juv_surv_bullfrogMgmt,
                   ncol = 2, nrow = 2)
ggplot2::ggsave(filename = filename, width = 6.5, height = 6)






#### BELOW HERE CURRENTY ISN"T IN THE MANUSCRIPT, DELETE ONCE CONFIRM


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
  ggtitle( "") + 
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
