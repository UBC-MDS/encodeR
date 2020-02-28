#' cat_boost_encoder
#'
#' This function encodes categorical variables using conditional averages of the target variable per
#' category. This differs from regular target encoding, however, in that the encodings are calculated in a
#' sequential fashion per each row, and then averaged over many permutations.
#'
#' @param X_train A tibble representing the training data set containing some categorical features/columns.
#' @param X_test A tibble representing the test set, containing some set of categorical features/columns.
#' @param y A numeric vector or character vector representing the target variable. If the objective is "binary", then the vector
#'  should only contain two unique values.
#' @param n_permutations An integer representing the number of permutations to use when calculating
#'  the encodings.
#' @param cat_columns A character vector containing the names of the categorical columns in the tibble
#' that should be encoded.
#' @param prior A number in [0, inf] that acts as pseudo counts when calculating the encodings. Useful for
#'  preventing encodings of 0 for when the training set does not have particular categories observed
#'  in the test set. A larger value gives less weight to what is observed in the training set. A value
#'  of 0 incorporates no prior information.
#' @param objective A string, either "regression" or "binary" specifying the problem. Default is regression.
#'  For regression, only the uniform quantization method is incorporated here for simplicity.
#'
#' @return A list containing with processed training and test sets, in which the named categorical
#' columns are replaced with their encodings.
#' @export
#'
#' @examples catboost_encoder(
#' my_train,
#' my_test,
#' my_train$y,
#' n_permutations = 50,
#' cat_columns = c("foo"),
#' prior = 0.05,
#' objective = "regression")
cat_boost_encoder <- function(X_train, X_test, y, n_permutations, cat_columns, prior, objective = "regression") {

  out <- list(train_processed, test_processed)

}
