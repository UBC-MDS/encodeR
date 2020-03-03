
<!-- README.md is generated from README.Rmd. Please edit that file -->

# encodeR

<!-- badges: start -->

<!-- badges: end -->

This package seeks to provide a convenient set of functions that allow
for the encoding of categorical features in potentially more informative
ways when compared to other, more standard methods. The user will feed
as input a training and testing dataset with categorical features, and
the resulting data frames returned will be preprocessed with a specific
encoding of the categorical features. At a high level, this package
automates the preprocessing of categorical features in ways that exploit
particular correlations between the different categories and the data
**without** increasing the dimension of the dataset, like in one hot
encoding. Thus, through the more deliberate handling of these
categorical features, higher model performance can possibly be achieved.

## Features

This package contains four functions, each that accept two tibbles
representing the train and test sets. Depending on the method, the
functions will also require additional arguments depending on how the
encodings are calculated for each category. For now, the package aims to
support binary classification and regression problems.

1.  One-hot encoder: the standard one-hot encoding of categorical
    features, which will create K-1 columns of 0/1 indicator variables.
2.  Frequency encoder: calculates encodings based off the observed
    frequency of each category in the training set.
3.  Target/Label encoder: calculates encodings by computing the average
    observed response per each category.
4.  Conjugate encoder: calculates encodings based off Bayes rule using
    conjugate priors and the mean of the posterior distribution. The
    original paper for this method can be found
    [here.](https://arxiv.org/pdf/1904.13001.pdf)

## Where encodeR Fits in The R Ecosystem

There are some packages in R that include different, more sophisticated
kinds of encoding methods. The well known framework
[H20](http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-munging/target-encoding.html)
has a function for target encoding, and the
[recipes](https://cran.r-project.org/web/packages/recipes/recipes.pdf)
package has the ability to one hot encode. The package
[cattonum](https://cran.r-project.org/web/packages/cattonum/cattonum.pdf)
also contains many kinds of encoding schemes such as frequency encoding,
target encoding, and one hot encoding. Based on research, there does not
exist a package in R that implements conjugate encoding.

The problem with the R ecosystem for categorical encoding is that there
is not one package that contains all of the most popular encoders. This
results in users having to import many different packages to experiment
with different encoders, each with their own syntax and interface. This
package hopes to give users one, coherent framework for encoding
categorical features in R. Furthermore, methods that have not been
packaged in R like conjugate encoding will directly add something new to
the R ecosystem.

## Installation

You can install the released version of encodeR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("encodeR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/encodeR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(encodeR)
## basic example code
```

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->

<!-- ```{r cars} -->

<!-- summary(cars) -->

<!-- ``` -->

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. -->

<!-- You can also embed plots, for example: -->

<!-- ```{r pressure, echo = FALSE} -->

<!-- plot(pressure) -->

<!-- ``` -->

<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub! -->
