context("Testing for the NLF-specific dispersal functions")
library(dapva4nlf)

test_that("dispersalTracking is working as expected", {
  
  # Gather the inputs
  resultsTracking_popSize_females <- dapva::makeResultsTracking(i = 1,
                                                                yrs = 50, 
                                                                initial_year = 1,
                                                                pops = c("cell3", "cell4", "cell7"), 
                                                                class_names = c("eggs", "tadpoles", 
                                                                                "yoy", "juv", "A2", 
                                                                                "A3", "A4plus"),
                                                                sex = "female")
  
  resultsTracking_popSize_females[,paste("1")] <- 100 # put 100 in each wetland and age class
  
  yoy_rows <- which(resultsTracking_popSize_females$class == "yoy")
  
  inputs_all <- dapva4nlf::getNLFIdahoFeasinputs()
  inputs <- inputs_all[[1]]
  wetland_distances_km <- inputs_all[[2]]
  
  parameterByIterTracking_baseCase <- dapva4nlf::selectNLFIdahoParameterByIterTracking(inputs, base_case = TRUE)
  i = 1
  
  # Run the dispersal function
  dispersal_results <- dispersalTracking(resultsTracking_popSize_females, 
                                                    yoy_rows, i, j = 1,
                                                    wetlands = c("cell3", "cell4", "cell7"),
                                                    wetland_distances_km,
                                                    parameterByIterTracking = parameterByIterTracking_baseCase,
                                                    allow_outside = 'no', 
                                                    demographic_stochasticity = FALSE)
  
  # In the base case, it is the MoreGoShort model
  parameterByIterTracking_baseCase$dispersal_CSF_vs_MoreGoShort
  # 11% of the yoy disperse, so 11 of the 100 yoy in each wetland will move if no demographic stochasticity
  as.numeric(parameterByIterTracking_baseCase[i, paste0("p_yoy_disperse")])
  # In each wetland expect 9 to go lessEqual_1km, 1 to go greater1lessEqual2km, and 1 to go greater2km
  # From cell 7, there are no wetlands in our example within 1km (since frogs are not allowed to go outside the WMA).
  # Therefore expect all 9 to stay in cell 7.
  testthat::expect_equal(dispersal_results[3,3] == 9, TRUE)
  # From cell 7 there is one wetland between 1 and 2 km away - cell #4
  # Therefore expect one frog to go to cell #4
  testthat::expect_equal(dispersal_results[3,2] == 1, TRUE)
  # From cell 7 there is one wetland more than 2 km away - cell #3
  # Therefore expect one frog to go to cell #3
  testthat::expect_equal(dispersal_results[3,1] == 1, TRUE)

})
