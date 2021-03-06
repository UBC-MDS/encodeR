% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/target_encoder.R
\name{target_encoder}
\alias{target_encoder}
\title{target_encoder}
\usage{
target_encoder(
  X_train,
  X_test = NULL,
  y,
  cat_columns,
  prior = 0.5,
  objective = "regression"
)
}
\arguments{
\item{X_train}{A `tibble` or `data.frame` representing the training data set
containing some categorical features/columns.}

\item{X_test}{A `tibble` or `data.frame` representing the test set,
containing some set of categorical features/columns.}

\item{y}{A numeric vector or character vector representing the target
variable. If the objective is "binary", then the vector should only contain
two unique values.}

\item{cat_columns}{A character vector containing the names of the
categorical columns in the tibble that should be encoded.}

\item{prior}{A number in [0, inf] that acts as pseudo counts when
calculating the encodings. Useful for preventing encodings of 0 for when the
training set does not have particular categories observed in the test set.
A larger value gives less weight to what is observed in the training set. A
value of 0 incorporates no prior information. The default value is 0.5.}

\item{objective}{A string, either "regression" or "binary" specifying the
problem. Default is regression.}
}
\value{
A list containing with processed training and test sets, in which
the named categorical columns are replaced with their encodings.
}
\description{
This function encodes categorical variables with average target values for
each category.
}
\examples{
target_encoder(
X_train = mtcars,
y = mtcars$mpg,
cat_columns = c("gear", "carb"),
prior = 0.5,
objective = "regression")
}
