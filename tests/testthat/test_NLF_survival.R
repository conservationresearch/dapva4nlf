context("Testing for the NLF-specific survival functions")
library(dapva4nlf)

test_that("survivalEggs is working as expected", {
  
  popSizeVector_test <- dapva::makePopSizeVector(
    pop_class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
    num_indiv_per_pop_class = c(1000, 100, 20), sex = "female"
  )
  
  survivalMatrix_test <- dapva::makeSurvivalMatrix(
    class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
    survival_rates = c(0.5, 0.4, 0.9)
  )
  
  # Test without demographic stochasticity
  egg_survival_test <- dapva4nlf::survivalEggs(popSizeVector_test, 
                                                       survivalMatrix_test,
                                                       demographic_stochasticity = FALSE)
  
  testthat::expect_equal((egg_survival_test[1] == 0), TRUE)
  testthat::expect_equal((egg_survival_test[2] == 1000*0.5), TRUE)
  testthat::expect_equal((egg_survival_test[3] == 0), TRUE)
  
  # Test with demographic stochasticity
  egg_survival_test2 <- dapva4nlf::survivalEggs(popSizeVector_test, 
                                                        survivalMatrix_test,
                                                        demographic_stochasticity = TRUE)
  # Just check this visually, confirmed seems to be working
})

test_that("survivalTadpoles is working as expected", {
  
  popSizeVector_test <- dapva::makePopSizeVector(
    pop_class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
    num_indiv_per_pop_class = c(1000, 100, 20), sex = "female"
  )
  
  survivalMatrix_test <- dapva::makeSurvivalMatrix(
    class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
    survival_rates = c(0.5, 0.4, 0.9)
  )
  
  # Test without demographic stochasticity
  tadpole_survival_test <- dapva4nlf::survivalTadpoles(popSizeVector_test, 
                                                            survivalMatrix_test,
                                                            demographic_stochasticity = FALSE)
  
  testthat::expect_equal((tadpole_survival_test[1] ==0), TRUE)
  testthat::expect_equal((tadpole_survival_test[2] ==0), TRUE)
  testthat::expect_equal((tadpole_survival_test[3] == 100*0.4), TRUE)
  
  # Test with demographic stochasticity
  tadpole_survival_test2 <- dapva4nlf::survivalTadpoles(popSizeVector_test, 
                                                       survivalMatrix_test,
                                                       demographic_stochasticity = TRUE)
  # Just check this visually, confirmed seems to be working
})

test_that("survivalOverwinter is working as expected", {
  
  popSizeVector_test <- dapva::makePopSizeVector(
    pop_class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
    num_indiv_per_pop_class = c(1000, 100, 20), sex = "female"
  )
  
  survivalMatrix_test <- dapva::makeSurvivalMatrix(
    class_names = c("pop1 eggs", "pop1 tadpoles", "pop1 adults"),
    survival_rates = c(0.5, 0.4, 0.9)
  )
  
  # Test without demographic stochasticity
  survived_the_winter_test <- dapva4nlf::survivalOverwinter(popSizeVector_test, 
                                                            survivalMatrix_test,
                                                            demographic_stochasticity = FALSE)
  
  testthat::expect_equal((survived_the_winter_test[1] ==0), TRUE)
  testthat::expect_equal((survived_the_winter_test[2] ==0), TRUE)
  testthat::expect_equal((survived_the_winter_test[3] == 20*0.9), TRUE)
  
  # Test with demographic stochasticity
  survived_the_winter_test2 <- dapva4nlf::survivalOverwinter(popSizeVector_test, 
                                                            survivalMatrix_test,
                                                            demographic_stochasticity = TRUE)
  
 # Just check this visually, confirmed seems to be working
  
  

})
