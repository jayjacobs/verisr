\name{getintenum}
\alias{getintenum}
\title{Internal Function.
This will iterate through the veris object and return
a list of matches.  This is intented to maintain the orginal
indexes of the veris object so further manipulation can be done.}
\usage{
  getintenum(veris, enum)
}
\arguments{
  \item{veris}{a verisr object}

  \item{enum}{the field to count}
}
\description{
  Note: Can do a special "industryN" request and it will
  chop off the industry at the N value or return same
  length of zeros if it isn't long enough.
}

