\name{ts_table}
\alias{ts_table}
\title{
  Create ts_table
}
\description{
  Create a \code{ts_table}.
}
\usage{
ts_table(data, timestamp, columns)
}
\arguments{
  \item{data}{
    numeric
}
  \item{timestamp}{
    \code{\link{Date}} or \code{\link{POSIXct}}
}
  \item{columns}{
    column names
}
}
\details{

  Create a time-series table (\code{ts_table}). A
  \code{ts_table} is a numeric matrix, so there is
  always a \code{dim} attribute. For a \code{ts_table}
  \code{x}, you get the number of observations with
  \code{dim(x)[1L]}.

  Attached to this matrix are several attributes:

  \describe{
    \item{timestamp}{a vector: the numeric representation of
      the timestamp}
    \item{t.type}{character: the class of the original
      timestamp, either \code{Date} or \code{POSIXct}}
    \item{columns}{a character vector that provides the
      columns names}
  }
  
  There may be other attributes as well, but these three
  are always present.

  Timestamps must be of class \code{\link{Date}} or
  \code{\link{POSIXct}} (\code{\link{POSIXlt}} is
  converted). A \code{tzone} attribute is dropped.

  A \code{ts_table} is not meant as a time-series
  class. For most computations (plotting, calculation
  of statistics, etc.), the \code{ts_table} must first
  be coerced to \code{zoo}, \code{xts}, a
  \code{\link{data.frame}} or a similar data
  structure. Methods that perform such coercions are
  responsible for converting the numeric timestamp
  vector to an actual timestamp. For this, they may use
  the function \code{\link{ttime}} (\sQuote{translate time}).

  
  
}
\value{
  a \code{ts_table}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{as.ts_table}}
}
\examples{
ts_table(1:5, Sys.Date() - 5:1, columns = "value")
}
