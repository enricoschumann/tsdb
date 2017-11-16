\name{tsdb-package}
\alias{tsdb-package}
\alias{tsdb}
\alias{ttime}
\title{
  Store and Retrieve Time-Series
}
\description{

  Several simple utilities that allow to handle
  time-series data as CSV files.

}
\usage{
ttime(x, from = "datetime", to = "numeric", tz = "",
      strip.attr = TRUE, format = "\%Y-\%m-\%d")
}
\arguments{
  \item{x}{
    numeric
}
  \item{from}{
    character
}
  \item{to}{
    character
}
  \item{tz}{
    character
}
  \item{strip.attr}{
    logical: strip attributes; in particular, timezone information
}
  \item{format}{
    character
}
}
\details{

  \code{ttime} (`translate time') converts timestamps
  between formats.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{read_ts_tables}}
}
\examples{
## see the particular functions
}
