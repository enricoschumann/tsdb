\name{as.ts_table}
\alias{as.ts_table}
\alias{as.ts_table.zoo}
\title{
  Coerce To ts_table
}
\description{
  Coerce objects to \code{ts_table}
}
\usage{
as.ts_table(x, ...)

\method{as.ts_table}{zoo}(x, columns, \dots)
}
\arguments{
  \item{x}{
    object to be coerced to ts_table
}
  \item{columns}{
    character
}
  \item{\dots}{
    arguments to be passed to other methods
}
}
\details{
  A generic function.
}
\value{
  A ts_table.
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{read_ts_tables}}
}
\examples{
require("zoo")
as.ts_table(zoo(1:5, Sys.Date()-5:1),  ## note that the "columns"
            columns = "value")         ## must be specified


}
