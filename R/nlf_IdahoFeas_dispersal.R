# Making functions for my DAPVA (Decision Analysis Population Viability
# Analysis) package. Following Chapter 6 in
# https://r-pkgs.org/package-within.html

# This file contains functions related to NLF dispersal.

# Laura Keating
# May 2021

##### dispersalTracking  #####

#' Applies the dispersal models and returns a matrix showing how many yoy dispersed 
#' from one wetland cell to another.
#' 
#' The rows are the 'from' wetland and the columns are the 'to' wetland/location.

#' @param resultsTracking_popSize_females Formatted as per the function dapva::makeResultsTracking()
#' @param yoy_rows which(resultsTracking_popSize_females$class == "yoy")
#' @param i Iteration #
#' @param j Year
#' @param wetlands A vector of the wetland names that are available in the WMA.
#' @param wetlands_not_dry A vector of the wetland names that are available as dipersal destinations this year becasue they are not dry.
#' @param wetland_distances_km A matrix of distance between the wetlands.
#' @param parameterByIterTracking A dataframe of the form parameterByIterTracking()
#'  that contains the following dispersal related inputs: dispersal_CSF_vs_MoreGoShort,
#'  dispersal_CSFmodel_lessEqual1km, etc.
#' @param allow_outside "yes" or "no", allow frogs to disperse outside of the WMA or not.
#' @param demographic_stochasticity TRUE or FALSE, defaults to TRUE
#'
#' @examples
#' resultsTracking_popSize_females <- dapva::makeResultsTracking(i = 1,
#'                                                              yrs = 50, 
#'                                                              initial_year = 1,
#'                                                              pops = c("cell3", "cell4", "cell7"), 
#'                                                              class_names = c("eggs", "tadpoles", 
#'                                                                              "yoy", "juv", "A2", 
#'                                                                              "A3", "A4plus"),
#'                                                              sex = "female")
#'
#'resultsTracking_popSize_females[,paste("1")] <- 100 # put 100 in each wetland and age class
#'
#'yoy_rows <- which(resultsTracking_popSize_females$class == "yoy")
#'
#'inputs_all <- dapva4nlf::getNLFIdahoFeasinputs()
#'inputs <- inputs_all[[1]]
#'wetland_distances_km <- inputs_all[[2]]
#'
#'parameterByIterTracking_baseCase <- dapva4nlf::selectNLFIdahoParameterByIterTracking(inputs, base_case = TRUE)
#'
#'# Run the dispersal function
#'dispersal_results <- dispersalTracking(resultsTracking_popSize_females, 
#'                                       yoy_rows, i = 1, j = 1,
#'                                       wetlands = c("cell3", "cell4", "cell7"),
#'                                       wetlands_not_dry = c("cell3", "cell4", "cell7"),
#'                                       wetland_distances_km,
#'                                       parameterByIterTracking = parameterByIterTracking_baseCase,
#'                                       allow_outside = 'no', 
#'                                       demographic_stochasticity = FALSE)
#' 
#' @export
# unit test in place, needs more testing
dispersalTracking <- function(resultsTracking_popSize_females, yoy_rows, i, j,
                              wetlands,
                              wetlands_not_dry,
                              wetland_distances_km,
                              parameterByIterTracking,
                              allow_outside, 
                              demographic_stochasticity = TRUE){
  
  
  p_yoy_disperse <- as.numeric(parameterByIterTracking[i, paste0("p_yoy_disperse")])
  
  if(demographic_stochasticity == TRUE){
    n_dispersing_per_wetland <- stats::rbinom(size = resultsTracking_popSize_females[yoy_rows, paste(j)], 
                                              n = length(yoy_rows), prob = p_yoy_disperse)
  }
  if(demographic_stochasticity == FALSE){
    n_dispersing_per_wetland <- resultsTracking_popSize_females[yoy_rows, paste(j)] * p_yoy_disperse
  }
  
  
  if(allow_outside == 'no'){
    wetlands <- wetlands[which(wetlands != "outside")]
    wetlands_not_dry <- wetlands_not_dry[which(wetlands_not_dry != "outside")]
  }
  
  wetland_distances_km_relevant <- wetland_distances_km[paste(wetlands), paste(wetlands)]
  
  dispersal_tracking <- wetland_distances_km_relevant # initialize
  dispersal_tracking[,] <- 0 # initialize
  
  # for each wetland, see where they go
  for(w in 1:length(yoy_rows)){
    # Which wetland are we dealing with 
    wetland <- resultsTracking_popSize_females$pop[yoy_rows][w]
    
    # How many yoy are dispersing from this wetland
    n_dispersing <- n_dispersing_per_wetland[w]
    
    if(n_dispersing>0){ # if there are some yoy to disperse, now decide where they go
      # Which dispersal model are we using?
      dispersal_model <- parameterByIterTracking[i, paste0("dispersal_CSF_vs_MoreGoShort")]
      
      # Which distance bucket to each of them decide to go to?
      if(demographic_stochasticity == TRUE){
        if(dispersal_model == "CSF"){
          n_to_each_distance <-  sample(c("lessEqual_1km", "greater1lessEqual2km", "greater2km"), 
                                        n_dispersing, replace = TRUE, 
                                        prob = c(as.numeric(parameterByIterTracking[i, paste0("dispersal_CSFmodel_lessEqual1km")]),
                                                 as.numeric(parameterByIterTracking[i, paste0("dispersal_CSFmodel_greater1kmlessequal2km")]),
                                                 as.numeric(parameterByIterTracking[i, paste0("dispersal_CSFmodel_greater2km")])))
          
          
        }
        
        if(dispersal_model == "MoreGoShort"){
          n_to_each_distance <-sample(c("lessEqual_1km", "greater1lessEqual2km", "greater2km"), 
                                      n_dispersing, replace = TRUE, 
                                      prob = c(as.numeric(parameterByIterTracking[i, paste0("dispersal_MoreGoShortmodel_lessEqual1km")]),
                                               as.numeric(parameterByIterTracking[i, paste0("dispersal_MoreGoShortmodel_greater1kmlessequal2km")]),
                                               as.numeric(parameterByIterTracking[i, paste0("dispersal_MoreGoShortmodel_greater2km")])))
          
        }
      }
      
      if(demographic_stochasticity == FALSE){
        if(dispersal_model == "CSF"){
          n_to_each_distance <- rep(c("lessEqual_1km", "greater1lessEqual2km", "greater2km"), 
                                    times = round(n_dispersing*c(as.numeric(parameterByIterTracking[i, paste0("dispersal_CSFmodel_lessEqual1km")]),
                                                                 as.numeric(parameterByIterTracking[i, paste0("dispersal_CSFmodel_greater1kmlessequal2km")]),
                                                                 as.numeric(parameterByIterTracking[i, paste0("dispersal_CSFmodel_greater2km")]))))
          
          
        }
        
        if(dispersal_model == "MoreGoShort"){
          n_to_each_distance <- rep(c("lessEqual_1km", "greater1lessEqual2km", "greater2km"), 
                                    times = round(n_dispersing*c(as.numeric(parameterByIterTracking[i, paste0("dispersal_MoreGoShortmodel_lessEqual1km")]),
                                                          as.numeric(parameterByIterTracking[i, paste0("dispersal_MoreGoShortmodel_greater1kmlessequal2km")]),
                                                          as.numeric(parameterByIterTracking[i, paste0("dispersal_MoreGoShortmodel_greater2km")]))))
        }
      }

      
      
      n_lessEqual_1km <- length(which(n_to_each_distance == "lessEqual_1km"))
      n_greater1lessEqual2km <- length(which(n_to_each_distance == "greater1lessEqual2km"))
      n_greater2km <- length(which(n_to_each_distance == "greater2km"))
      
      
      # What other wetlands fall into each of our three buckets
      wetland_distances <- wetland_distances_km_relevant[paste(wetland),]
      wetlands_lessEqual_1km <- colnames(wetland_distances_km_relevant)[which(wetland_distances > 0 & wetland_distances<= 1)]
      wetlands_greater1lessEqual2km <- colnames(wetland_distances_km_relevant)[which(wetland_distances > 1 & wetland_distances <= 2)]
      wetlands_greater2km <- colnames(wetland_distances_km_relevant)[which(wetland_distances > 2)]
      
      # Only allow dispersal destinations that are not dry
      
      wetlands_lessEqual_1km <- intersect(wetlands_lessEqual_1km, wetlands_not_dry)
      wetlands_greater1lessEqual2km <- intersect(wetlands_greater1lessEqual2km, wetlands_not_dry)
      wetlands_greater2km <- intersect(wetlands_greater2km, wetlands_not_dry)
      
      # If frogs are allowed outside, since all wetlands are less than 1km from the outside, add the outside bucket as an option to all distance buckets
      if(allow_outside == "yes"){
        wetlands_greater1lessEqual2km <- unique(c(wetlands_greater1lessEqual2km, "outside"))
        wetlands_greater2km <- unique(c(wetlands_greater2km, "outside"))
      }

      # If frogs are not allowed outside, then remove the outside option 
      if(allow_outside == "no"){
        wetlands_lessEqual_1km <- wetlands_lessEqual_1km[which(wetlands_lessEqual_1km != 'outside')]
        wetlands_greater1lessEqual2km <- wetlands_greater1lessEqual2km[which(wetlands_greater1lessEqual2km != 'outside')]
        wetlands_greater2km <- wetlands_greater2km[which(wetlands_greater2km != 'outside')]
      }
      
      # If any of the distance categories are now empty, then have them return to their original wetland
      if(length(wetlands_lessEqual_1km) == 0){wetlands_lessEqual_1km <- paste(wetland)}
      if(length(wetlands_greater1lessEqual2km) == 0){wetlands_greater1lessEqual2km <- paste(wetland)}
      if(length(wetlands_greater2km) == 0){wetlands_greater2km <- paste(wetland)}
      
      # Which wetland do each go to
      destinations <- c(sample(wetlands_lessEqual_1km, n_lessEqual_1km, replace = TRUE),
                        sample(wetlands_greater1lessEqual2km, n_greater1lessEqual2km, replace = TRUE),
                        sample(wetlands_greater2km, n_greater2km, replace = TRUE))
      
      dispersal_tracking[paste(wetland),"cell3"] <- length(which(destinations == "cell3"))
      dispersal_tracking[paste(wetland),"cell4"] <- length(which(destinations == "cell4"))
      dispersal_tracking[paste(wetland),"cell7"] <- length(which(destinations == "cell7"))
      if(length(which(wetlands == "ephemeral_wetlands")) > 0){ # i.e. this alternative has restoration for the ephemeral wetlands
        dispersal_tracking[paste(wetland),"ephemeral_wetlands"] <- length(which(destinations == "ephemeral_wetlands"))
      }
      if(allow_outside == 'yes'){
        dispersal_tracking[paste(wetland),"outside"] <- length(which(destinations == "outside"))
      }
    }
    
    if(n_dispersing == 0){ # then none to disperse
      dispersal_tracking[paste(wetland),] <- 0
    }
  }
  
  return(dispersal_tracking)
}

