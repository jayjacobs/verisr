\name{foldmatrix}
\alias{foldmatrix}
\title{compact rows for analysis}
\usage{
  foldmatrix(pca, feature, min = 1, clean = FALSE)
}
\arguments{
  \item{pca}{a pca matrix object}

  \item{feature}{a vector of features to fold on}

  \item{min}{if min is > 1, this will check for a minimum
  amount of records for each feature, if it doesn't have
  the min, it will be set to zeros and cleaned if clean is
  set to TRUE}

  \item{clean}{if TRUE, this will look for anything that
  will mess up a PCA or MDS function and remove it.}
}
\description{
  given a verisr object and a list of features, this will
  create a matrix of the data suitable for further PCA,
  hclust or other dist() analysis.
}

