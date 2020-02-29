#' frequency_encoder
#'
#' This function encodes categorical variables using the frequencies of each category.
#'
#' @param X_train A tibble representing the training data set containing some categorical features/columns.
#' @param X_test A tibble representing the test set, containing some set of categorical features/columns.
#' @param cat_columns A character vector containing the names of the categorical columns in the tibble
#' that should be encoded.
#'
#' @return A list containing with processed training and test sets, in which the named categorical
#' columns are replaced with their encodings.
#' @export
#'
#' @examples frequency_encoder(
#' my_train,
#' my_test,
#' cat_columns = c("foo"))
frequency_encoder <- function(X_train, X_test, cat_columns) {

  out <- list(train_processed, test_processed)

}
