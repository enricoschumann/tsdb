\name{ttime}
\alias{ttime}
\title{
  Translate Timestamps
}
\description{

  Translate a vector of timestamps.
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
    character: \code{datetime}, \code{numeric} or \code{character}
}
  \item{to}{
    character: \code{numeric}, \code{Date} or \code{POSIXct}
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

  \code{ttime} (\sQuote{translate time}) converts timestamps
  between formats.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{ts_table}}
}
\examples{
ttime(Sys.Date())
ttime(17397, from = "numeric", to = "Date")
}
