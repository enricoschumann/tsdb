\name{write_ts_table}
\alias{write_ts_table}
\title{
  Write Data to Files
}
\description{
  Write time-series data to files.
}
\usage{
write_ts_table(x, dir, file, add = FALSE, overwrite = FALSE)
}
\arguments{
  \item{x}{
    numeric data
}
  \item{dir}{
    character
}
  \item{file}{
    character
}
  \item{add}{

    logical: if TRUE, add data with timestamps that are
    not in a file.

}
  \item{overwrite}{
    logical: overwrite existing file when data differs 
}
}
\details{
  Write files.
}
\value{
  The number of data rows written to a file.
}
\author{
  Enrico Schumann
}
\seealso{
  write_ts_table
}
\examples{
\dontrun{
write_ts_table(ts_table(1, Sys.Date(), "a_column"),
               dir = "~/tsdb/daily", file = "example")
}
}