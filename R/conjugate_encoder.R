#' conjugate_encoder
#'
#' This function encodes categorical variables by fitting a posterior distribution per each category
#' to the target variable y, using a known conjugate-prior. The resulting mean(s) of each posterior distribution
#' per each category are used as the encodings.
#'
#' @param X_train A tibble representing the training data set containing some categorical features/columns.
#' @param X_test A tibble representing the test set, containing some set of categorical features/columns.
#' @param y A numeric vector or character vector representing the target variable. If the objective is "binary", then the vector
#'  should only contain two unique values.
#' @param cat_columns A character vector containing the names of the categorical columns in the tibble
#' that should be encoded.
#' @param prior_params A list with named parameters that specify the prior assumed. For regression, this requires
#' a dictionary with four keys and four values: mu, vega, alpha, beta. All must be real numbers, and must be greater than 0
#' except for mu, which can be negative. For binary classification, this requires a dictionary with two keys and two values: alpha, beta. All must be real
#' numbers and be greater than 0.
#' @param objective A string, either "regression" or "binary" specifying the problem. Default is regression.
#'  For regression, only the uniform quantization method is incorporated here for simplicity.
#'
#' @return A list containing with processed training and test sets, in which the named categorical
#' columns are replaced with their encodings. For regression, the encoder will add one additional dimension to the original training set since the
#' assumed prior distribution is two dimensional.
#' @export
#'
#' @examples conjugate_encoder(
#' my_train,
#' my_test,
#' my_train$y,
#' cat_columns = c("foo"),
#' prior_params = list(alpha = 3, beta = 3),
#' objective = "regression")
conjugate_encoder <- function(X_train, X_test, y, cat_columns, prior_params, objective = "regression") {

  out <- list(train_processed, test_processed)

}
