library(tidyr)
library(dplyr)
library(purrr)
library(rlang)
library(readr)
#' target_encoder
#'
#' This function encodes categorical variables with average target values for each category.
#'
#' @param X_train A tibble representing the training data set containing some categorical features/columns.
#' @param X_test A tibble representing the test set, containing some set of categorical features/columns.
#'  The default is NULL.
#' @param y A numeric vector or character vector representing the target variable.
#'  If the objective is "binary", then the vector should only contain two unique values.
#' @param cat_columns A character vector containing the names of the categorical columns in the tibble
#'  that should be encoded.
#' @param prior A number in [0, inf] that acts as pseudo counts when calculating the encodings. Useful for
#'  preventing encodings of 0 for when the training set does not have particular categories observed
#'  in the test set. A larger value gives less weight to what is observed in the training set. A value
#'  of 0 incorporates no prior information. The default value is 0.5.
#' @param objective A string, either "regression" or "binary" specifying the problem. Default is regression.
#'
#' @return A list containing with processed training and test sets, in which the named categorical
#' columns are replaced with their encodings.
#' @export
#'
#' @examples target_encoder(
#' my_train,
#' my_test,
#' my_train$y,
#' cat_columns = c("foo"),
#' prior = 0.5,
#' objective = "regression")


data <- read_csv("../data/testing_data.csv")

train1 <- data %>% filter(train_test_1 == 'train')
test1 <- data %>% filter(train_test_1 == 'test')

train2 <- data %>% filter(train_test_3 == 'train')
test2 <- data %>% filter(train_test_3 == 'test')

target_encoder <- function(X_train, X_test = NULL, y, cat_columns, prior = 0.5, objective = "regression") {


  train_processed <- X_train

  global_mean <- mean(y, na.rm = TRUE)

  if (is.null(X_test)){
    for(i in seq_along(cat_columns)){
      column <- cat_columns[i]
      # calculate target counts for each category and save to dictionary
      search_table <- train_processed %>%
        bind_cols(target = y) %>%
        group_by(!!sym(column)) %>%
        summarize(the_sum = sum(target, na.rm = TRUE),
                  the_count = n())

      search_table['encodings'] <- (search_table['the_sum'] + prior * global_mean) / (search_table['the_count'] + prior)
      search_table = search_table %>% dplyr::select( -c(the_sum,the_count))
      # # encode categorical columns for training dataset
      train_processed <- left_join(train_processed, search_table, by = column)
      train_processed[column] <- train_processed['encodings']
      train_processed <- train_processed %>% dplyr::select(-encodings)
      }
    out <- train_processed

    } else{
    test_processed <- X_test

    for(i in seq_along(cat_columns)){
      column <- cat_columns[i]
      # calculate target counts for each category and save to dictionary
      search_table <- train_processed %>%
        bind_cols(target = y) %>%
        group_by(!!sym(column)) %>%
        summarize(the_sum = sum(target, na.rm = TRUE),
                  the_count = n())

      search_table['encodings'] <- (search_table['the_sum'] + prior * global_mean) / (search_table['the_count'] + prior)
      search_table = search_table %>% dplyr::select( -c(the_sum,the_count))
      # encode categorical columns for training dataset
      train_processed <- left_join(train_processed, search_table, by = column)
      train_processed[column] <- train_processed['encodings']
      train_processed <- train_processed %>% dplyr::select(-encodings)
      # encode categorical columns for testing dataset
      test_processed <- left_join(test_processed, search_table, by = column)
      test_processed[column] <- test_processed['encodings']
      test_processed <- test_processed %>% dplyr::select(-encodings)
      test_processed[[column]][is.na(test_processed[[column]])] <- global_mean
      }
  out <- list(train_processed, test_processed)
    }
  out
}
