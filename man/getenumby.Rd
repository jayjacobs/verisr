\name{getenumby}
\alias{getenumby}
\title{Get the sorted levels or return NULL if none exist for enum
Get a count of enumerations values by some other enumeration}
\usage{
getenumby(veris, enum, primary = NULL, secondary = NULL, filter = NULL,
  add.n = T, add.freq = T, fillzero = T)
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
\details{
Change: the "add.n" and "add.freq" options are now TRUE by
default.
}
\examples{
\dontrun{
hacking <- getenumby(veris, "action", "asset.variety", fillzero=T)
}
}

