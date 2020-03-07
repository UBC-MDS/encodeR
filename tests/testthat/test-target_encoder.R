test_that("Function does not throw an error if we input invalid arguments", {

  data <- readr::read_csv("../../data/testing_data.csv")
  # check input of objective
  expect_error(target_encoder(
    X_train = data,
    X_test = data,
    y = data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior = 0.5,
    objective = "something"))
  # check type of input of prior
  expect_error(target_encoder(
    X_train = data,
    X_test = data,
    y = data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior = 'a',
    objective = "binary"))
  # check type of input X_train
  expect_error(target_encoder(
    X_train = c(1,2,3),
    X_test = data,
    y = data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior = 0.5,
    objective = "binary"))
  # check if length y equals to length X_train
  expect_error(target_encoder(
    X_train = data,
    X_test = data,
    y =  c(1,2,3),
    cat_columns = c("feature_cat_chr"),
    prior = 0.5,
    objective = "binary"))
  # check if X_train contains cat_columns
  expect_error(target_encoder(
    X_train = data,
    X_test = data,
    y = data$target_bin,
    cat_columns = c("something"),
    prior = 0.5,
    objective = "binary"))
  # check if target variable is numeric for regression objective
  expect_error(target_encoder(
    X_train = data,
    X_test = data,
    y = as.character(data$target_bin),
    cat_columns = c("feature_cat_chr"),
    prior = 0.5,
    objective = "regression"))
  #check if target variable is binary
  target_copy1 <- data$target_bin
  target_copy1[1] <- 20

  expect_error(target_encoder(
    X_train = data,
    X_test = data,
    y = target_copy1,
    cat_columns = c("feature_cat_chr"),
    prior = 0.5,
    objective = "binary"))
  # check type of input X_test
  expect_error(target_encoder(
    X_train = data,
    X_test = c(1,2,3),
    y = data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior = 0.5,
    objective = "binary"))

})

# test_that("Function does not return correct output", {
#
#   library(readr)
#   library(dplyr)
#   library(tidyr)
#
#   data <- readr::read_csv("../../data/testing_data.csv")
#
#   train1 <- data %>% filter(train_test_1 == 'train')
#   test1 <- data %>% filter(train_test_1 == 'test')
#
#   train2 <- data %>% filter(train_test_3 == 'train')
#   test2 <- data %>% filter(train_test_3 == 'test')
#
#   # TODO
#
# })
