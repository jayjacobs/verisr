\name{getfilter}
\alias{getfilter}
\title{This will return a verisr filter object.}
\usage{
getfilter(veris, and = NULL, or = NULL, or.not = NULL, and.not = NULL)
}
\arguments{
  \item{veris}{a verisr object}

  \item{or}{list of criteria matching "or"}

  \item{and}{list of criteria matching "and"}

  \item{or.not}{list of criteria matching "or not"}

  \item{and.not}{list of criteria matching "and not"}
}
\description{
Get a filter given a list of "and" combinations and/or a
list of "or" combinations. Note: industry can be industryN
values can be "$exists" for that field being present
}
\examples{
\dontrun{
#tbd
hacking <- getenum(veris, "action.hacking.variety")
external <- getenum(veris, "actor.external.motive")
}
}

