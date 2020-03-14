#' onehot_encoder
#'
#' This function encodes categorical variables using the popular onehot method for each category.
#'
#' @param X_train A tibble representing the training data set containing some categorical features/columns.
#' @param X_test A tibble representing the test set, containing some set of categorical features/columns.
#' @param cat_columns A character vector containing the names of the categorical columns in the tibble or
#' data frame that should be encoded.
#'
#' @return A list with processed training and test sets, in which the named categorical
#' columns are replaced with their encodings.
#' @export
#'
#' @examples onehot_encoder(
#' X_train = mtcars,
#' cat_columns = c("gear", "carb"))
#'
onehot_encoder <- function(X_train, X_test = NULL, cat_columns) {

  # check that cat_columns are present in the columns of X_train
  if (any(cat_columns %in% colnames(X_train)) == FALSE) {
    stop("Column does not exist in the training set.")
  }

  # check type of input X_train
  if (!is.data.frame(X_train)) {
    stop("Type of X_train must be a data frame.")
  }

  # Check if X_test was provided
  X_test_included <- !is.null(X_test)

  if (X_test_included) {

    # encode values of X_train and X_test
    X_train_processed <- fastDummies::dummy_cols(
      X_train,
      select_columns = cat_columns,
      remove_first_dummy = TRUE) %>%
      dplyr::select(-tidyselect::all_of(cat_columns))

    X_test_processed <- fastDummies::dummy_cols(
      X_test,
      select_columns = cat_columns,
      remove_first_dummy = TRUE
    ) %>%
      dplyr::select(-tidyselect::all_of(cat_columns))

    # Find any columns that are misaligned

    missing_cols <-  names(X_train_processed)[which(!names(X_train_processed) %in% names(X_test_processed))]

    if (length(missing_cols) != 0) {

    for (k in 1:length(missing_cols)) {

      missing_col <- missing_cols[k]

      X_test_processed[[missing_col]] <- rep(0, nrow(X_test_processed))

    }
}
    # Reorder the columns to match X_train_processed

    X_test_processed <- X_test_processed[names(X_train_processed)]

    out <- list("train" = X_train_processed, "test" = X_test_processed)

  } else {

    # encode values of X_train
    X_train_processed <- fastDummies::dummy_cols(
      X_train,
      select_columns = cat_columns,
      remove_first_dummy = TRUE) %>%
      dplyr::select(-tidyselect::all_of(cat_columns))

    out <- list("train" = X_train_processed)
  }
}
