test_that("frequency_encoder() does not replace categories with no encodings with 0", {

  testing_data <- readr::read_csv("../testdata/testing_data.csv")
  train <- testing_data %>%
    dplyr::filter(train_test_3 == 'train')
  test <- testing_data %>%
    dplyr::filter(train_test_3 == 'test')
  result <- frequency_encoder(train,test,c("feature_cat_chr"))
  test <- result$test
  expect_equal(anyNA(test),FALSE)

})

test_that("frequency_encoder() does not produce the expected encoding", {
  testing_data <- readr::read_csv("../testdata/testing_data.csv")
  train <- testing_data %>%
    dplyr::filter(train_test_1 == 'train')
  test <- testing_data %>%
    dplyr::filter(train_test_1 == 'test')
  result <- frequency_encoder(train,test,c("feature_cat_chr"))
  train <- result$train
  expect_equal(train$feature_cat_chr[1],0.35)

})

test_that("frequency_encoder() does not produce a list with correct dimensions", {
  testing_data <- readr::read_csv("../testdata/testing_data.csv")
  train <- testing_data %>%
    dplyr::filter(train_test_1 == 'train')
  test <- testing_data %>%
    dplyr::filter(train_test_1 == 'test')
  result_test <- frequency_encoder(train, test, cat_columns =c("feature_cat_chr"))
  result <- frequency_encoder(train, cat_columns = c("feature_cat_chr"))
  expect_equal(length(result_test),2)
  expect_equal(length(result),1)
})

test_that("frequency_encoder() does not throw an error if we input invalid arguments", {
  testing_data <- readr::read_csv("../testdata/testing_data.csv")
  # Test for categorical columns not being in X_train
  expect_error(frequency_encoder(
    X_train = testing_data,
    X_test = testing_data,
    cat_columns = c("cool4cats")))
  # check type of input X_train
  expect_error(target_encoder(
    X_train = c(1,2,3),
    X_test = testing_data,
    cat_columns = c("feature_cat_chr")))
})
