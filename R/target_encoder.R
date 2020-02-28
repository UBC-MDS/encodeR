#' target_encoder
#'
#' This function encodes categorical variables with average target values for each category.
#'
#' @param X_train A tibble representing the training data set containing some categorical features/columns.
#' @param X_test A tibble representing the test set, containing some set of categorical features/columns.
#' @param y A numeric vector or character vector representing the target variable. 
#'  If the objective is "binary", then the vector should only contain two unique values.
#' @param cat_columns A character vector containing the names of the categorical columns in the tibble
#'  that should be encoding.
#' @param prior A number in [0, inf] that acts as pseudo counts when calculating the encodings. Useful for
#'  preventing encodings of 0 for when the training set does not have particular categories observed
#'  in the test set. A larger value gives less weight to what is observed in the training set. A value
#'  of 0 incorporates no prior information. The default value is 0.5.
#' @param min_samples: The minimum samples to calculate mean of targets. 
#'  If number of the target smaller than min_samples, the value will be encoded as prior probability. 
#'  The default value is 1.
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
#' min_samples = 1)
target_encoder <- function(X_train, X_test, y, cat_columns, prior = 0.5, min_samples = 1) {
  
  out <- list(train_processed, test_processed)
  
}