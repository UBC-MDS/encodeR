
get_encoding <-  function(data, col){
  # obtain frequencies of each category
  out <- data %>%
    dplyr::group_by(!!rlang::sym(col))%>%
    dplyr::summarise(freq = dplyr::n()/nrow(data))
}

encode <- function(data,encoding_col,col){
  # mutate specific column inplace to replace categorical values with encoded
  # numeric values
  encoded_col <- data %>%
    dplyr::left_join(encoding_col) %>%
    dplyr::mutate(!!rlang::sym(col) := dplyr::if_else(is.na(freq), 0, freq)
                  # if encoding didn't exist encode value as 0
                  ) %>%
    dplyr::select(-freq)
  data[[rlang::sym(col)]] <- encoded_col[[rlang::sym(col)]]
  out <- data
}


#' frequency_encoder
#'
#' This function encodes categorical variables using the frequencies of each
#' category.
#'
#' @param X_train A `tibble` or `data.frame` representing the training data set
#' containing some categorical features/columns.
#' @param X_test A `tibble` or `data.frame` representing the test set,
#' containing some set of categorical features/columns.
#' @param cat_columns A character vector containing the names of the
#' categorical columns in the tibble that should be encoded.
#'
#' @return A `list` with processed training and test sets (if provided), in
#' which the named categorical columns are replaced with their encodings.
#' @export
#'
#' @examples frequency_encoder(
#' X_train = mtcars,
#' cat_columns = c("gear", "carb"))
frequency_encoder <- function(X_train, X_test = NULL, cat_columns) {

  # check that cat_columns are present in the columns of X_train
  if (any(cat_columns %in% colnames(X_train)) == FALSE) {
    stop("Column does not exist in the training set.")
  }

  # check type of input X_train
  if (!is.data.frame(X_train)) {
    stop("Type of X_train must be a data frame.")
  }

  encodings <- list()

  # Check if X_test was provided
  X_test_included <- !is.null(X_test)

  if (X_test_included) {

    for (cat in cat_columns) {

      # create encoding column
      encoding_col <- get_encoding(X_train, cat)

      # encode values of X_train and X_test
      X_train <- encode(X_train,encoding_col, cat)
      X_test <- encode(X_test,encoding_col, cat)
    }

    out <- list( "train" = X_train, "test" = X_test)

  } else {

    for (cat in cat_columns) {

      # create encoding column
      col_df <- get_encoding(X_train, cat)

      # encode values of X_train
      X_train <- encode(X_train,col_df, cat)
    }

    out <- list( "train" = X_train)
  }
}
