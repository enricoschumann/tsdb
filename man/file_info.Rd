\name{file_info}
\alias{file_info}
\title{
  Information about Data File
}
\description{

  Provides information about data stored in file:
  columns, number of observations, range of timestamps.

}
\usage{
file_info(dir, file)
}
\arguments{
  \item{dir}{
    character
}
  \item{file}{
    character
}
}
\details{

  Provide information, such as number of entries, of
  specified files.

  It is recommended that code that uses the returned
  information to alter or write tables, should explicitly
  check whether a table exists (column \code{exists} in
  the returned \code{\link{data.frame}}). For instance,
  a value of \code{\link{NA}} for \code{min.timestamp}
  would occur for a non-existing file, but also if the
  file could not be read for some reason.

}
\value{

  An object of type \code{file_info}, which is a
  \code{data.frame} with information such as whether a
  file exists, minimum and maximum timestamp, and more.

}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{ts_table}}
}
\examples{
ts <- ts_table(1:3, as.Date("2018-12-3") + 1:3, columns = "A")
d <- tempdir()
write_ts_table(ts, file = "temp", dir = d)
file_info(d, "temp")
}
