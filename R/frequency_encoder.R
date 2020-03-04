
encode_freq <- function(X) {
  X
}

#' frequency_encoder
#'
#' This function encodes categorical variables using the frequencies of each category.
#'
#' @param X_train A `tibble` or `data.frame` representing the training data set containing some categorical features/columns.
#' @param X_test A `tibble` or `data.frame` representing the test set, containing some set of categorical features/columns.
#' @param cat_columns A character vector containing the names of the categorical columns in the tibble
#' that should be encoded.
#'
#' @return A `list` with processed training and test sets, in which the named categorical
#' columns are replaced with their encodings.
#' @export
#'
#' @examples frequency_encoder(
#' my_train,
#' my_test,
#' cat_columns = c("foo"))
frequency_encoder <- function(X_train, X_test, cat_columns) {
  train_processed <- dplyr::mutate_if(X_train,~is.element(deparse(substitute(.x)), cat_columns), encode_freq)
  test_processed <- X_test
  out <- list( "train" = train_processed, "test" = test_processed)
}

