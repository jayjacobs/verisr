\name{getenumby}
\alias{getenumby}
\title{Get a count of enumerations values by some other enumeration}
\usage{
getenumby(veris, enum, primary, secondary = NULL, filter = NULL,
  add.n = F, add.freq = F, fillzero = T)
}
\arguments{
  \item{veris}{a verisr object}

  \item{enum}{the main enumeration field}

  \item{primary}{the primary enumeration to filter on}

  \item{secondary}{the (optional) secondary enumeration to
  filter on}

  \item{filter}{limit what records are searched (optional)}

  \item{add.n}{include a total count of variables found
  (denominator)}

  \item{add.freq}{include a percentage (x/n)}

  \item{fillzero}{fill in missing matches with zeros}
}
\description{
This will collect an enumation and count it.
}
\examples{
\dontrun{
hacking <- getenum(veris, "action.hacking.variety")
external <- getenum(veris, "actor.external.motive")
}
}

