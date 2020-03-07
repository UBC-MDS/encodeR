
get_encoding <-  function(data, col){
  out <- data %>%
    dplyr::group_by(!!sym(col))%>%
    dplyr::summarise(freq = n()/nrow(data))
}

encode <- function(data,encoding_col,col){
  encoded_col <- data %>%
    dplyr::left_join(encoding_col) %>%
    dplyr::mutate(!!sym(col) := if_else(is.na(freq), 0, freq) # if encoding didn't exist encode value as 0
                  ) %>% 
    dplyr::select(-freq)
  data[[sym(col)]] <- encoded_col[[sym(col)]]
  out <- data
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
#' @return A `list` with processed training and test sets (if provided), in which the named categorical
#' columns are replaced with their encodings.
#' @export
#'
#' @examples frequency_encoder(
#' my_train,
#' my_test,
#' cat_columns = c("foo"))
frequency_encoder <- function(X_train, X_test = NULL, cat_columns) {
  encodings = list()
  X_test_included <- !is.null(X_test)
  if (X_test_included) {
    for (cat in cat_columns) {
      encoding_col <- get_encoding(X_train, cat)
      X_train <- encode(X_train,encoding_col, cat)
      X_test <- encode(X_test,encoding_col, cat)
    }
    out <- list( "train" = X_train, "test" = X_test)
  } else {
    for (cat in cat_columns) {
      col_df <- get_encoding(X_train, cat)
      X_train <- encode(X_train,col_df, cat)
    }
    out <- list( "train" = X_train)
  }
}

