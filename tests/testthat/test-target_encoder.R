test_that("Function does not throw an error if we input invalid arguments", {

  data <- readr::read_csv("../testdata/testing_data.csv")
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


test_that("Function does not run if we do not pass a test set.", {

  # Test for the NULL X_test case.

  data <- readr::read_csv("../testdata/testing_data.csv")

  # Check the type of output
  expect_type(target_encoder(
    X_train = data,
    y = data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior = 0.5,
    objective = "binary"), "list")

  # Check the shape of output
  expect_equal(length(target_encoder(
    X_train = data,
    y = data$target_bin,
    cat_columns = c("feature_cat_chr"),
    prior = 0.5,
    objective = "binary")), 1)

})


test_that("Function does not return correct output", {

  # check if function return correct output for X_test not null

  data <- readr::read_csv("../testdata/testing_data.csv")

  train1 <- data %>% dplyr::filter(train_test_1 == 'train')
  test1 <- data %>% dplyr::filter(train_test_1 == 'test')

  train2 <- data %>% dplyr::filter(train_test_3 == 'train')
  test2 <- data %>% dplyr::filter(train_test_3 == 'test')

  #check if shape of output X_train is equal to original shape
  expect_equal(nrow(target_encoder(
    X_train=train2,
    y=train2$target_bin,
    cat_columns=c('feature_cat_chr', 'feature_cat_num'),
    X_test=test2,
    prior=0.5,
    objective='binary')$train), nrow(train2))

  # check if output returns correct shape
  expect_equal(length(target_encoder(
    X_train=train2,
    y=train2$target_bin,
    cat_columns=c('feature_cat_chr', 'feature_cat_num'),
    X_test=test2,
    prior=0.5,
    objective='binary')),2)

  # check the target encoding value for regression is correct
  expect_equal(target_encoder(
    X_train=train1,
    y=train1$target_cont,
    cat_columns=c('feature_cat_chr', 'feature_cat_num'),
    X_test=test1,
    prior=0.5)$train[['feature_cat_chr']][1], 0.6957849,tolerance=1e-5)

  # check the target encoding value is correct
  expect_equal(target_encoder(
    X_train=train1,
    y=train1$target_bin,
    cat_columns=c('feature_cat_chr', 'feature_cat_num'),
    X_test=test1,
    prior=0.5,
    objective='binary')$train[['feature_cat_chr']][1], 0.43)

  # check if the unseen test output is correct
  expect_equal(target_encoder(
    X_train=train2,
    y=train2$target_bin,
    cat_columns=c('feature_cat_chr', 'feature_cat_num'),
    X_test=test2,
    prior=0.5,
    objective='binary')$test[['feature_cat_chr']][1], 0.5)


})
