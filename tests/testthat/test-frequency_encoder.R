test_that("frequency_encoder() replaces categories with no encodings with 0", {

  testing_data <- readr::read_csv("../../data/testing_data.csv")
  train <- testing_data %>%
    dplyr::filter(train_test_3 == 'train')
  test_missing_groups <- testing_data %>%
    dplyr::filter(train_test_3 == 'test')
  result <- frequency_encoder(train,test_missing_groups,c("feature_cat_chr"))
  test_missing_groups_processed <- result$test
  expect_identical(anyNA(test_missing_groups_processed),FALSE)
})
