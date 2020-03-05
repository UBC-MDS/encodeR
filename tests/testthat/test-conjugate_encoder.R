test_that("Function does not throw an error if we input invalid arguments", {

  library(readr)

  test_data <- readr::read_csv("../../data/testing_data.csv")

  expect_error(conjugate_encoder(
    X_train = test_data,
    X_test = test_data,
    y = test_data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior_params = list(beta = -5, alpha = -100),
    objective = "binary"))

  expect_error(conjugate_encoder(
    conjugate_encoder(
      X_train = test_data,
      X_test = test_data,
      y = test_data$target_bin,
      cat_columns = c("feature_cat_chr"),
      prior_params = list(beta = 5, alpha = 100),
      objective = "nice_model!!!")
  ))

  expect_error(conjugate_encoder(
    conjugate_encoder(
      X_train = test_data,
      X_test = test_data,
      y = test_data$target_cont,
      cat_columns = c("cool4cats"),
      prior_params = list(beta = 5, alpha = 100),
      objective = "regression")
  ))

  expect_error(conjugate_encoder(
    conjugate_encoder(
      X_train = test_data,
      X_test = test_data,
      y = test_data$target_cont,
      cat_columns = c("feature_cat_chr"),
      prior_params = list(beta = 5, alpha = 100),
      objective = "binary")
  ))

})

test_that("Function does not return correct output", {

  library(readr)
  library(dplyr)
  library(tidyr)

# TO DO


})
