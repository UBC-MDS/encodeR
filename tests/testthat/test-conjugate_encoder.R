test_that("Function does not throw an error if we input invalid arguments", {

  test_data <- readr::read_csv("../../data/testing_data.csv")

  # These tests check for errors raised when given invalid/improper inputs.

  # Test for invalid prior parameters.
  expect_error(conjugate_encoder(
    X_train = test_data,
    X_test = test_data,
    y = test_data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior_params = list(beta = -5, alpha = -100),
    objective = "binary"))

  expect_error(conjugate_encoder(
    X_train = test_data,
    X_test = test_data,
    y = test_data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior_params = list(mu = 0, vega = -3, alpha = 1, beta = -50),
    objective = "regression"))

  # Test for the wrong prior parameters in the list.
  expect_error(conjugate_encoder(
      X_train = test_data,
      X_test = test_data,
      y = test_data$target_bin,
      cat_columns = c("feature_cat_chr"),
      prior_params = list(beta = 5, alpha = 100),
      objective = "regression"))

  expect_error(conjugate_encoder(
    X_train = test_data,
    X_test = test_data,
    y = test_data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior_params = list(alpha = 5, beta = 3, gamma = 100),
    objective = "binary"))

  # Test for categorical columns not being in X_train
  expect_error(conjugate_encoder(
      X_train = test_data,
      X_test = test_data,
      y = test_data$target_cont,
      cat_columns = c("cool4cats"),
      prior_params = list(beta = 5, alpha = 100),
      objective = "regression"))

  # Test for wrong objective
  expect_error(conjugate_encoder(
      X_train = test_data,
      X_test = test_data,
      y = test_data$target_cont,
      cat_columns = c("feature_cat_chr"),
      prior_params = list(beta = 5, alpha = 100),
      objective = "wowccool"))

  # Test for binary classification with a vector of more than two unique values
  expect_error(conjugate_encoder(
    X_train = test_data,
    X_test = test_data,
    y = test_data$feature_cont,
    cat_columns = c("feature_cat_chr"),
    prior_params = list(beta = 5, alpha = 100),
    objective = "binary"))

})

test_that("Function does not run if we pass factors or characters as our target variable.", {

  # Test for different y types.

  test_data <- readr::read_csv("../../data/testing_data.csv")

  # IF we pass a factor, does the function still work?
  expect_type(conjugate_encoder(
    X_train = test_data,
    X_test = test_data,
    y = as.factor(test_data$target_bin),
    cat_columns = c("feature_cat_chr"),
    prior_params = list(beta = 5, alpha = 100),
    objective = "binary"), "list")

  char <- ifelse(test_data$target_bin == 1, "A", "B")

  expect_type(conjugate_encoder(
    X_train = test_data,
    X_test = test_data,
    y = char,
    cat_columns = c("feature_cat_chr"),
    prior_params = list(beta = 5, alpha = 100),
    objective = "binary"), "list")


})

test_that("Function does not run if we do not pass a test set.", {

  # Test for the NULL X_test case.

  test_data <- readr::read_csv("../../data/testing_data.csv")

  # IF we pass a factor, does the function still work?
  expect_type(conjugate_encoder(
    X_train = test_data,
    y = test_data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior_params = list(beta = 5, alpha = 100),
    objective = "binary"), "list")

  expect_type(conjugate_encoder(
    X_train = test_data,
    y = test_data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior_params = list(beta = 5, alpha = 100),
    objective = "binary"), "list")

})

test_that("Function for regression does not return correct output", {

  # Test for the correct output.

  test_data <- readr::read_csv("../../data/testing_data.csv")

  train1 <- test_data %>%
    filter(train_test_1 == "train")
  test1 <- test_data %>%
    filter(train_test_1 == "test")

  train2 <- test_data %>%
    filter(train_test_3 == "train")
  test2 <- test_data %>%
    filter(train_test_3 == "test")

  # Known true values for train_test_1. Train_test_1 is a standard dataset.
  train_true_mean_1 <- c(rep(0.773327, 7), rep(0.627191, 6), rep(-0.719982, 7))
  train_true_var_1 <- c(rep(1.46942, 7), rep(0.427613, 6), rep(2.57358, 7))

  test_true_mean_1 <- c(rep(0.77327, 3), rep(0.627191, 4), rep(-0.719982, 3))
  test_true_var_1 <- c(rep(1.46942, 3), rep(0.427613, 4), rep(2.57358, 3))

  # Known true values for train_test_2. This dataset has test categories that do not appear in the training set.
  train_true_mean_2 <- c(rep(0.392902, 10), rep(-0.401635, 10))
  train_true_var_2 <- c(rep(1.71888, 10), rep(2.1842, 10))

  test_true_mean_2 <- rep(1, 10)
  test_true_var_2 <- rep(2, 10)

# Generate the encodings.

  encodings_set_1 <- conjugate_encoder(
    X_train = train1,
    X_test = test1,
    y = train1$target_cont,
    cat_columns = c("feature_cat_chr", "feature_cat_num"),
    prior_params = list(mu = 1, vega = 3,  alpha = 2, beta = 2),
    objective="regression"
  )

  encodings_set_2 <- conjugate_encoder(
    X_train = train2,
    X_test = test2,
    y = train2$target_cont,
    cat_columns = c("feature_cat_chr", "feature_cat_num"),
    prior_params = list(mu = 1, vega = 3,  alpha = 2, beta = 2),
    objective="regression"
  )

  # Test against the known encodings for test set 1.

  expect_equal(encodings_set_1$train$feature_cat_chr_encoded_mean, train_true_mean_1, tolerance = 0.01)
  expect_equal(encodings_set_1$train$feature_cat_chr_encoded_var, train_true_var_1, tolerance = 0.01)
  expect_equal(encodings_set_1$test$feature_cat_chr_encoded_mean, test_true_mean_1, tolerance = 0.01)
  expect_equal(encodings_set_1$test$feature_cat_chr_encoded_var, test_true_var_1, tolerance = 0.01)

  # Test against the known encodings for test set 2.

  expect_equal(encodings_set_2$train$feature_cat_chr_encoded_mean, train_true_mean_2, tolerance = 0.01)
  expect_equal(encodings_set_2$train$feature_cat_chr_encoded_var, train_true_var_2, tolerance = 0.01)
  expect_equal(encodings_set_2$test$feature_cat_chr_encoded_mean, test_true_mean_2, tolerance = 0.01)
  expect_equal(encodings_set_2$test$feature_cat_chr_encoded_var, test_true_var_2, tolerance = 0.01)

})

test_that("Function for binary classification does not return correct output", {

  # Test for the correct output.

  test_data <- readr::read_csv("../../data/testing_data.csv")

  train1 <- test_data %>%
    filter(train_test_1 == "train")
  test1 <- test_data %>%
    filter(train_test_1 == "test")

  train2 <- test_data %>%
    filter(train_test_3 == "train")
  test2 <- test_data %>%
    filter(train_test_3 == "test")

  # Known true values for train_test_1. Train_test_1 is a standard dataset.
  train_true_mean_1 <- rep(0.285714, 20)
  test_true_mean_1 <- rep(0.285714, 10)

  # Known true values for train_test_2. This dataset has test categories that do not appear in the training set.
  train_true_mean_2 <- rep(0.357143, 20)
  test_true_mean_2 <- rep(0.625, 10)

  # Generate the encodings.

  encodings_set_1 <- conjugate_encoder(
    X_train = train1,
    X_test = test1,
    y = train1$target_bin,
    cat_columns = c("feature_cat_chr", "feature_cat_num"),
    prior_params = list(alpha = 5, beta = 3),
    objective="binary"
  )

  encodings_set_2 <- conjugate_encoder(
    X_train = train2,
    X_test = test2,
    y = train2$target_bin,
    cat_columns = c("feature_cat_chr", "feature_cat_num"),
    prior_params = list(alpha = 5, beta = 3),
    objective="binary"
  )

  # Test against the known encodings for test set 1.

  expect_equal(encodings_set_1$train$feature_cat_chr_encoded, train_true_mean_1, tolerance = 0.01)
  expect_equal(encodings_set_1$test$feature_cat_chr_encoded, test_true_mean_1, tolerance = 0.01)

  # Test against the known encodings for test set 2.

  expect_equal(encodings_set_2$train$feature_cat_chr_encoded, train_true_mean_2, tolerance = 0.01)
  expect_equal(encodings_set_2$test$feature_cat_chr_encoded, test_true_mean_2, tolerance = 0.01)

})
