\name{write_ts_table}
\alias{write_ts_table}
\title{
  Write Data to Files
}
\description{
  Write time-series data to files.
}
\usage{
write_ts_table(ts, dir, file, add = FALSE, overwrite = FALSE, backend = "csv")
}
\arguments{

  \item{ts}{
    a \code{ts_table}
  }
  \item{dir}{
    character
  }
  \item{file}{
    character
  }
  \item{add}{
    
    logical: if \code{TRUE}, add data with timestamps
    that are not in a file.
    
  }
  \item{overwrite}{
    
    logical: overwrite existing file when data
    differs. \code{overwrite} implies \code{add}.
    
  }
  \item{backend}{

    character: currently, \code{csv} and \code{monetdb}
    are supported

  }
  
}
\details{

  The function takes a \code{ts_table} and writes it to
  a file.

  If the file already exist and both \code{add} and
  \code{overwrite} are \code{FALSE} (the default),
  nothing is written.

  When \code{add} is \code{TRUE}, the function checks
  if \code{x} contains timestamps not yet in the file
  and, if there are any, writes only those data.

  When \code{overwrite} is \code{TRUE}, the function
  merges all observations in the file with those in
  \code{x} and writes the result back to the file. If
  \code{x} contains timestamps that were already in the
  file, the data in the file are overwritten. Note that
  no data will be removed from the file: timestamps not
  in \code{x} remain unchanged in the file.

  
}
\value{
  Invisibly, the number of data rows written to a file.
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
               dir = "~/tsdb/daily", file = "example")}
}