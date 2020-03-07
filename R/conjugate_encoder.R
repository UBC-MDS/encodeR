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
#' @param X_test An optional tibble representing the test set, containing some set of categorical features/columns. Default = NULL.
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
conjugate_encoder <- function(X_train, X_test = NULL, y, cat_columns, prior_params, objective = "regression") {

  # Check inputs for incorrect values.

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

      # Initialize lists and grab the prior parameters necessary for the encodings.
      encodings_list = vector("list", length(cat_columns))
      encodings_list[[1]] <- X_train
      mu <- prior_params$mu
      alpha <- prior_params$alpha
      vega <- prior_params$vega
      beta <- prior_params$beta
      n <- nrow(X_train)

      for (i in seq_along(cat_columns)) {

        column <- cat_columns[i]

        # Calculate the conditional means and conditional variances for each category.

        conditionals <- X_train %>%
          bind_cols(target = y) %>%
          group_by(!!sym(column)) %>%
          summarize(conditional_mean = mean(target),
                    conditional_variances = var(target))

        if (any(is.na(conditionals$conditional_variances)) == TRUE) {
          warning("NA's fitted for expected variance. The variance of a single data point does not exist. Make sure columns specified are truly categorical.")
        }

        # Calculates the posterior parameters of the distribution, given the observed conditional means and conditional variances.
        # These are just closed form formulas.

        mu_post <- vega * mu + n * conditionals$conditional_mean / (vega + n)
        alpha_post <- alpha + n / 2
        beta_post <- beta + 0.5 * n * conditionals$conditional_variances + (n * vega) / (vega + n) * ((conditionals$conditional_mean - mu)^2 / 2)

        # IF alpha is less than or equal to 1, the expected value of the variance is not defined. So, force the user to input
        # a sensical value to prevent division by 0 or an invalid posterior parameter (this is unlikely for most datasets)

        if (alpha_post <= 1) {
          stop("Invalid posterior prior for alpha. Increase the prior on alpha to avoid this error.")
        }

        # Calculate the encodings which are just the marginal means of each random variable of the posterior.
        # The posterior distribution is two dimensional (X, Y) and so we will get 2 columns of encodings.

        posterior_expected_var <- beta_post / (alpha_post - 1)
        all_encodings <- conditionals %>%
          bind_cols(., tibble(encoding_mu = mu_post, encoding_sigma = posterior_expected_var)) %>%
          select(-conditional_mean, -conditional_variances)

        # Changing the names of the learned encodings here so I can reference them later when joining.

        names(all_encodings)[2] <- paste(column, "encoded_mean", sep = "_")
        names(all_encodings)[3] <- paste(column, "encoded_var", sep = "_")

        # i + 1 here since we set the first item in the list to the original train set. This is so that I can
        # use reduce on the list with left_join to join the encodings for each category back to the training set without looping.

        encodings_list[[i + 1]] <- all_encodings

      }

      # Join all of the learned encodings for each feature back to the original training set. Then, drop the original categorical
      # columns.
      train_processed <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(-all_of(cat_columns))

      # Providing X_test isn't NA, encode the test set.

      if (!is.na(X_test)) {

      # Set the first element of the list containing all of the learned encodings from the training set to the test set now.
      # This is again to recursively call left_join on each item so bind each pair of encodings for each categorical column back
      # to the test set.
      encodings_list[[1]] <- X_test

      # This is the recursive call to left_join. It's the same thing as a loop where the result of each left_join in the "loop"
      # is carried forwards.
      test_encodings_means <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(contains("encoded_mean"))

      # If there are test categories that we didn't see in our training set, our best guess is the prior marginal mean of the 1st random variable.
      test_encodings_means[is.na(test_encodings_means)] <- mu

      # Same thing as above, but for the 2nd random variable of the posterior distribution.
      test_encodings_var <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(contains("encoded_var"))

      # Again, set categories that are in test set but not in training to be the prior marginal mean of the 2nd random variable.
      test_encodings_var[is.na(test_encodings_var)] <- beta / (alpha - 1)

      test_processed <- bind_cols(X_test, test_encodings_means) %>%
        bind_cols(., test_encodings_var) %>%
        select(-all_of(cat_columns))

      }

    } else {

      # More checks for the binary case. If the user supplies a vector that has more than 2 unique values, stop.

      if (length(unique(y)) != 2) {
        stop("Binary classification is supported only.")
      }

      if (setequal(c("alpha", "beta"), names(prior_params)) == FALSE) {
        stop("Missing a required prior parameter.")
      }

      if (prior_params$alpha <= 0 | prior_params$beta <= 0) {
        stop("Invalid prior specification. Note that alpha and beta must all be greater than 0.")
      }

      # Encode any factor variables or character vectors to numeric 0/1's.
      if (is.factor(y) || is.character(y)) {

        all_unique_values <- unique(y)
        target_new <- case_when(y == all_unique_values[1] ~ 0,
                                y == all_unique_values[2] ~ 1,
                                TRUE ~ y)

      }

      # Grab needed parameters, untialize the list that will store the encodings.

      encodings_list <- vector("list", length(cat_columns) + 1)
      encodings_list[[1]] <- X_train
      alpha <- prior_params$alpha
      beta <- prior_params$beta
      n <- nrow(X_train)

      for (i in seq_along(cat_columns)) {

        column <- cat_columns[i]

        # Same thing as in the regression case, but the posterior here is less complicated (it's one dimension here, not two as above).
        # So we just need the total number of "1"'s.
        conditionals <- X_train %>%
          bind_cols(target = y) %>%
          group_by(!!sym(column)) %>%
          summarize(conditional_success = sum(target))

        # Calculate the posterior parameters.
        alpha_post <- alpha + conditionals$conditional_success
        beta_post <- beta + n - conditionals$conditional_success

        # Get the encodings, which is the mean of a beta distribution.
        posterior_expected_val <- alpha_post / (alpha_post + beta_post)
        all_encodings <- conditionals %>%
          bind_cols(., tibble(encoded = posterior_expected_val)) %>%
          select(-conditional_success)

        names(all_encodings)[2] <- paste(column, "encoded", sep = "_")

        encodings_list[[i + 1]] <- all_encodings

      }

      # Recursive joining to the train set.

      train_processed <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(-all_of(cat_columns))

      if (!is.na(X_test)) {

      encodings_list[[1]] <- X_test

      # Recursive joining on the test set, if it exists.
      test_encodings <- reduce(encodings_list, function(x, y) left_join(x, y, by = NULL)) %>%
        select(contains("encoded"))

      # If there exists a category that we didn't see in our training set, use the prior mean.
      test_encodings[is.na(test_encodings)] <- alpha / (alpha + beta)

      test_processed <- bind_cols(X_test, test_encodings) %>%
        select(-all_of(cat_columns))

      }

    }

  # Output.
  if (!is.na(X_test)) {
  out <- list(train_processed, test_processed)
  } else {
    out <- list(train_processed)
  }

  out

}
