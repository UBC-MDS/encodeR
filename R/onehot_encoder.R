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
onehot_encoder <- function(X_train, X_test, cat_columns) {

  out <- list(train_processed, test_processed)

}
