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

target_encoder <- function(X_train, X_test = NULL, y, cat_columns, prior = 0.5, objective = "regression") {

  # check input of objective
  if (!objective %in% c("regression", "binary")) {
    stop("Objective must be regression or binary.")
  }
  # check type of input of prior
  if (!is.numeric(prior)) {
    stop("The prior input should be numeric.")
  }
  # check type of input X_train
  if (!is.data.frame(X_train)) {
    stop("Type of X_train must be a data frame.")
  }
  # check if length y equals to length X_train
  if (length(y) != nrow(X_train)) {
    stop("Input y must equal to X_train.")
  }

  # check if X_train contains cat_columns
  if (any(cat_columns %in% colnames(X_train)) == FALSE) {
    stop("Column does not exist in the training set.")
  }

  #check if target variable is numeric for regression objective
  if (objective == "regression" & !is_double(y)) {
      stop("Type of target variable must be numeric.")
  }

   y_new <- y

  # binary objective
  if (objective == 'binary') {
    if (y %>% unique() %>% length() != 2) {
      stop("The target variable must be binary.")
    }
    # encode target variable to 1 and 0
    if (!is_double(y)) {
      y_new <- case_when(y == unique(y)[1] ~ 0,
                     y == unique(y)[2] ~ 1)
    }

  }

  train_processed <- X_train

  global_mean <- mean(y_new, na.rm = TRUE)

  if (is.null(X_test)) {
    for (i in seq_along(cat_columns)) {
      column <- cat_columns[i]
      # calculate target counts for each category and save to dictionary
      search_table <- train_processed %>%
        bind_cols(target = y_new) %>%
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

    } else {

      # check type of input X_test
      if (!is.data.frame(X_test)) {
        stop("Type of X_test must be a data frame.")
      }

      # check if X_test contains cat_columns
      if (any(cat_columns %in% colnames(X_test)) == FALSE) {
        stop("Column does not exist in the training set.")
      }

      test_processed <- X_test

      for (i in seq_along(cat_columns)) {
        column <- cat_columns[i]
        # calculate target counts for each category and save to dictionary
        search_table <- train_processed %>%
          bind_cols(target = y_new) %>%
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
