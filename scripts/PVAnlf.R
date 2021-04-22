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
version <- "_1" # insert short description to append to results to help identify

#---- Load libraries, and set the random seed.  -------------
## Import libraries
library(dapva) # this is our own internal library that houses functions used here
library(dapva4nlf) # this is our own internal library that houses functions used here
library(dplyr) # for summarise
library(foreach) # for %do% nd %dopar%; this might come as part of DoParallel which is loaded later, test this later by trying without

# Set up for parallel computing - e.g. https://privefl.github.io/blog/a-guide-to-parallelism-in-r/
library(doParallel)  # see https://www.r-bloggers.com/2016/07/lets-be-faster-and-more-parallel-in-r-with-doparallel-package/
no_cores <- detectCores() - 1  # one less than max to be safe and prevent crashing
# cl <- parallel::makeCluster(no_cores, type="FORK") # if on windows, try this instead: cl <- parallel::makeCluster(no_cores)
cl <- parallel::makeCluster(no_cores)
doParallel::registerDoParallel(cl)

# Set the seed in a way that works for parallel computing (each core needs a separate random seed) (e.g. https://www.r-bloggers.com/2020/09/future-1-19-1-making-sure-proper-random-numbers-are-produced-in-parallel-processing/)
clusterSetRNGStream(cl, iseed = 29) # without parallel computing can just do set.seed(1234)

#---- Specify the scenarios to run.  -------------
rows_to_run <- 1
#---- Specify number of iterations and number of runs per iterations.  -------------
n_iter  <- 10
max_n_runs_per_iter <- 10

#---- Start the scenario loop.  -------------
for(m in 1:length(rows_to_run)){ # loop through the different scenarios requested in the scenarios_to_run file


#---- Get the inputs.  -------------

  inputs_all <- getBTPDinputs_statusQuo(scenario_climate = scenarios_to_run$climate[row_to_run],
                                        scenario_plague = scenarios_to_run$plague[row_to_run])
  
  
#---- Choose the parameters for each iteration (i.e. parametric uncertainty). ----


#---- Run the PVA. ----

#---- Do the sensitivity analysis on the overall results. ----

#---- Store the name of the scenario. ----

#---- Save the results for this scenario. ----

#---- Close the scenarios loop. ----
}
#---- Stop the parallel computing and stop the timing. ----
parallel::stopCluster(cl)


}) # turn off sys.time


# Next step: Move the resulting Rdata files to the results folder on your
# local computer.If computing on a virtual machine, will need to export them.

# Then use the results wrapper script to extract the results information and graphs.
