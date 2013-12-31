\name{veris2matrix}
\alias{veris2matrix}
\title{convert veris object to a matrix}
\usage{
veris2matrix(veris, industry = 2, unknown = F)
}
\arguments{
  \item{veris}{a verisr object}

  \item{industry}{either 2 or 3 digits for industry}

  \item{unknown}{logical, whether to include Unknown/Other
  fields}
}
\description{
given a verisr object, this will create a matrix of the
data suitable for further PCA analysis
}

