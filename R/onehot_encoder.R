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
  X_test_included <- !is.null(X_test)
  if (X_test_included) {

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


    for (k in 1:length(missing_cols)) {

      missing_col <- missing_cols[k]

      X_test_processed <- X_test_processed %>%
        mutate(!!missing_col := rep(0, nrow(.)))

    }

    # Reorder the columns to match X_train_processed

    X_test_processed <- X_test_processed[names(X_train_processed)]

    out <- list("train" = X_train_processed, "test" = X_test_processed)
  } else {
    X_train_processed <- fastDummies::dummy_cols(
      X_train,
      select_columns = cat_columns,
      remove_first_dummy = TRUE) %>%
      dplyr::select(-tidyselect::all_of(cat_columns))

    out <- list("train" = X_train_processed)
  }
}
