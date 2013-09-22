\name{getenum}
\alias{getenum}
\title{Get a vector of values from an enumeration}
\usage{
  getenum(veris, enum, primary = NULL, secondary = NULL,
    filter = NULL, add.n = F, add.freq = F, fillzero = T)
}
\arguments{
  \item{veris}{a verisr object}

  \item{enum}{the field to count}

  \item{filter}{limit what records are searched (optional)}

  \item{add.n}{include a total count of variables found
  (denominator)}

  \item{add.freq}{include a percentage (x/n)}
}
\description{
  This will collect the values for an enumation
}
\examples{
\dontrun{
hacking <- getenum(veris, "action.hacking.variety")
external <- getenum(veris, "actor.external.motive")
}
}

