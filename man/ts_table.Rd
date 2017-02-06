\name{ts_table}
\alias{ts_table}
\title{
  Create ts_table
}
\description{
  Create \code{ts_table}
}
\usage{
ts_table(data, timestamp, columns)
}
\arguments{
  \item{data}{
    numeric
}
  \item{timestamp}{
    Date or POSIXct
}
  \item{columns}{
    column names
}
}
\details{
  Creates a ts_table.

  Timestamps must be of class Date or POSIXct (POSIXlt
  is converted). A \code{tzone} attribute is dropped.
  
}
\value{
  A \code{ts_table}:
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{as.ts_table}}
}
\examples{
ts_table(1:5, Sys.Date()-5:1, columns = "value")
}
