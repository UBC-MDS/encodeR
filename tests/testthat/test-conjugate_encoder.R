test_that("Function throws an error if we input invalid input", {

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
      y = test_data$target_bin,
      cat_columns = c("feature_cat_chr"),
      prior_params = list(beta = 5, alpha = 100),
      objective = "nice_model!!!")
  ))

})
