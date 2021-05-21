# Making functions for my DAPVA (Decision Analysis Population Viability
# Analysis) package. Following Chapter 6 in
# https://r-pkgs.org/package-within.html

# This file contains functions related to NLF dispersal.

# Laura Keating
# May 2021

# Example still needed

##### dispersalTracking  #####

#' Applies the dispersal models and returns a matrix showing how many yoy dispersed 
#' from one wetland cell to another.
#' 
#' The rows are the 'from' wetland and the columns are the 'to' wetland/location.
#'
#' INSERT
#' 
#' @param resultsTracking_popSize_females
#' @param yoy_rows
#' @param i Iteration #
#' @param j Year
#' @param wetlands A vector of the wetland names that are available in the WMA.
#' @param wetland_distances_km A matrix of distance between the wetlands.
#' @param parameterByIterTracking A dataframe of the form parameterByIterTracking()
#'  that contains the following dispersal related inputs: dispersal_CSF_vs_MoreGoShort,
#'  dispersal_CSFmodel_lessEqual1km, etc.
#' @param allow_outside "yes" or "no", allow frogs to disperse outside of the WMA or not.
#'
#' @examples
#' # Still to do
#' 
#' @export
# unit test STILL TO DO
dispersalTracking <- function(resultsTracking_popSize_females, yoy_rows, i, j,
                               wetlands,
                              wetland_distances_km,
                              parameterByIterTracking,
                              allow_outside){
  
  
  p_yoy_disperse <- as.numeric(parameterByIterTracking[i, paste0("p_yoy_disperse")])
  
  n_dispersing_per_wetland <- stats::rbinom(size = resultsTracking_popSize_females[yoy_rows, paste(j)], 
                                            n = length(yoy_rows), prob = p_yoy_disperse)
  
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
      
      
      n_lessEqual_1km <- length(which(n_to_each_distance == "lessEqual_1km"))
      n_greater1lessEqual2km <- length(which(n_to_each_distance == "greater1lessEqual2km"))
      n_greater2km <- length(which(n_to_each_distance == "greater2km"))
      
      
      # What other wetlands fall into each of our three buckets
      wetland_distances <- wetland_distances_km[paste(wetland),]
      wetlands_lessEqual_1km <- colnames(wetland_distances_km)[which(wetland_distances > 0 & wetland_distances<= 1)]
      wetlands_greater1lessEqual2km <- colnames(wetland_distances_km)[which(wetland_distances > 1 & wetland_distances <= 2)]
      wetlands_greater2km <- colnames(wetland_distances_km)[which(wetland_distances > 2)]
      
      # Since all wetlands are less than 1km from the outside, add the outside bucket as an option to all distance buckets
      wetlands_greater1lessEqual2km <- unique(c(wetlands_greater1lessEqual2km, "outside"))
      wetlands_greater2km <- unique(c(wetlands_greater2km, "outside"))
      
      # If frogs are not allowed outside, then remove the outside option 
      if(allow_outside == "no"){
        wetlands_lessEqual_1km <- wetlands_lessEqual_1km[which(wetlands_lessEqual_1km != 'outside')]
        wetlands_greater1lessEqual2km <- wetlands_greater1lessEqual2km[which(wetlands_greater1lessEqual2km != 'outside')]
        wetlands_greater2km <- wetlands_greater2km[which(wetlands_greater2km != 'outside')]
        
        # If any of the distance categories are now empty, then have them return to their original wetland
        if(length(wetlands_lessEqual_1km) == 0){wetlands_lessEqual_1km <- paste(wetland)}
        if(length(wetlands_greater1lessEqual2km) == 0){wetlands_greater1lessEqual2km <- paste(wetland)}
        if(length(wetlands_greater2km) == 0){wetlands_greater2km <- paste(wetland)}
        
      }
      
      
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
      dispersal_tracking[paste(wetland),"outside"] <- length(which(destinations == "outside"))
      

    }
    
    if(n_dispersing == 0){ # then none to disperse
      dispersal_tracking[paste(wetland),] <- 0
    }
  }
  
  return(dispersal_tracking)
}

