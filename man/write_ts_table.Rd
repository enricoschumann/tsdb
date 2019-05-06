\name{write_ts_table}
\alias{write_ts_table}
\title{
  Write Time-Series Data to File
}
\description{
  Write time-series data to files.
}
\usage{
write_ts_table(ts, dir, file, add = FALSE, overwrite = FALSE,
               replace.file = FALSE, backend = "csv")
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
  \item{replace.file}{

    logical: if \code{TRUE}, an existing file is deleted
    and replaced by a new file (i.e. containing \code{ts})

  }
  \item{backend}{

    character: currently, \code{csv} and \code{monetdb}
    are supported

  }

}
\details{

  The function takes a \code{ts_table} and writes it to
  a file.

  If the file already exists and both \code{add} and
  \code{overwrite} are \code{FALSE} (the default),
  nothing is written.

  When \code{add} is \code{TRUE}, the function checks
  if \code{ts} contains timestamps not yet in the file
  and, if there are any, writes only those data.

  When \code{overwrite} is \code{TRUE}, the function
  merges all observations in the file with those in
  \code{ts} and writes the result back to the file. If
  \code{ts} contains timestamps that were already in the
  file, the data in the file are overwritten. Note that
  no data will be removed from the file: timestamps not
  in \code{ts} remain unchanged in the file.

}
\value{
  Invisibly, the number of data rows written to a file.
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{read_ts_tables}}
}
\examples{
t1 <- ts_table(1:3, as.Date("2018-12-3") + 1:3, columns = "A")
t2 <- ts_table(4:5, as.Date("2018-12-3") + 1:2, columns = "A")

d <- tempdir()  ## this is just an example.
                ## Actual (valuable) data should never
                ## be stored in a tempdir!

write_ts_table(t1, dir = d, file = "t1")
write_ts_table(t2, dir = d, file = "t2")

read_ts_tables(c("t1", "t2"),
               dir = d, columns = "A",
               return.class = "zoo",
               column.names = "\%file\%.\%column\%")
}
