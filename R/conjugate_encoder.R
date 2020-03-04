library(tidyr)
library(dplyr)
library(purrr)
library(rlang)

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
#' a dictionary with four keys and four values: mu, vega, alpha, beta. All must be real numbers, alpha should be greater than 0, beta and vega should be greater than 0.
#' mu can be negative. For binary classification, this requires a list with two keys and two values: alpha, beta. All must be real
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

    if (!objective %in% c("regression", "binary")) {
      stop("Objective must be regression or binary.")
    }

    if (any(cat_columns %in% colnames(X_train)) == FALSE) {
      stop("Column does not exist in the training set.")
    }


    if (objective == "regression") {

      if (setequal(c("vega", "mu", "alpha", "beta"), names(prior_params)) == FALSE) {
        stop("Missing a required prior parameter.")
      }

      if (prior_params$vega <= 0 | prior_params$alpha <= 0 | prior_params$beta <= 0) {
        stop("Invalid prior specification. Note that vega, alpha, and beta must all be greater than 0.")
      }

      if (!is.numeric(y)) {
        stop("The target variable should be numeric.")
      }

      encodings_list = vector("list", length(cat_columns))
      encodings_list[[1]] <- X_train
      mu <- prior_params$mu
      alpha <- prior_params$alpha
      vega <- prior_params$vega
      beta <- prior_params$beta
      n <- nrow(X_train)

      for (i in seq_along(cat_columns)) {

        column <- cat_columns[i]

        conditionals <- X_train %>%
          bind_cols(target = y) %>%
          group_by(!!sym(column)) %>%
          summarize(conditional_mean = mean(target),
                    conditional_variances = var(target))

        if (any(is.na(conditionals$conditional_variances)) == TRUE) {
          warning("NA's fitted for expected variance. The variance of a single data point does not exist. Make sure columns specified are truly categorical.")
        }

        mu_post <- vega * mu + n * conditionals$conditional_mean / (vega + n)
        alpha_post <- alpha + n / 2
        beta_post <- beta + 0.5 * n * conditionals$conditional_variances + (n * vega) / (vega + n) * ((conditionals$conditional_mean - mu)^2 / 2)

        if (alpha_post <= 1) {
          stop("Invalid posterior prior for alpha. Increase the prior on alpha to avoid this error.")
        }

        posterior_expected_var <- beta_post / (alpha_post - 1)
        all_encodings <- conditionals %>%
          bind_cols(., tibble(encoding_mu = mu_post, encoding_sigma = posterior_expected_var)) %>%
          select(-conditional_mean, -conditional_variances)

        names(all_encodings)[2] <- paste(column, "encoded_mean", sep = "_")
        names(all_encodings)[3] <- paste(column, "encoded_var", sep = "_")

        encodings_list[[i + 1]] <- all_encodings

      }

      train_processed <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(-all_of(cat_columns))

      encodings_list[[1]] <- X_test

      test_encodings_means <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(contains("encoded_mean"))

      test_encodings_means[is.na(test_encodings_means)] <- mu

      test_encodings_var <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(contains("encoded_var"))

      test_encodings_var[is.na(test_encodings_var)] <- beta / (alpha - 1)

      test_processed <- bind_cols(X_test, test_encodings_means) %>%
        bind_cols(., test_encodings_var) %>%
        select(-all_of(cat_columns))


    } else {

      if (length(unique(y)) != 2) {
        stop("Binary classification is supported only.")
      }

      if (setequal(c("alpha", "beta"), names(prior_params)) == FALSE) {
        stop("Missing a required prior parameter.")
      }

      if (prior_params$alpha <= 0 | prior_params$beta <= 0) {
        stop("Invalid prior specification. Note that alpha and beta must all be greater than 0.")
      }

      if (is.factor(y) || is.character(y)) {

        all_unique_values <- unique(y)
        target_new <- case_when(y == all_unique_values[1] ~ 0,
                                y == all_unique_values[2] ~ 1,
                                TRUE ~ y)

      }

      encodings_list <- vector("list", length(cat_columns) + 1)
      encodings_list[[1]] <- X_train
      alpha <- prior_params$alpha
      beta <- prior_params$beta
      n <- nrow(X_train)

      for (i in seq_along(cat_columns)) {

        column <- cat_columns[i]

        conditionals <- X_train %>%
          bind_cols(target = y) %>%
          group_by(!!sym(column)) %>%
          summarize(conditional_success = sum(target))

        alpha_post <- alpha + conditionals$conditional_success
        beta_post <- beta + n - conditionals$conditional_success

        posterior_expected_val <- alpha_post / (alpha_post + beta_post)
        all_encodings <- conditionals %>%
          bind_cols(., tibble(encoded = posterior_expected_val)) %>%
          select(-conditional_success)

        names(all_encodings)[2] <- paste(column, "encoded", sep = "_")

        encodings_list[[i + 1]] <- all_encodings

      }

      train_processed <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(-all_of(cat_columns))

      encodings_list[[1]] <- X_test

      test_encodings <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(contains("encoded"))

      test_encodings[is.na(test_encodings)] <- alpha / (alpha + beta)

      test_processed <- bind_cols(X_test, test_encodings) %>%
        select(-all_of(cat_columns))

    }

  out <- list(train_processed, test_processed)

}

