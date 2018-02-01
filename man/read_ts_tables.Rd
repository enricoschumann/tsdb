\name{read_ts_tables}
\alias{read_ts_tables}
\title{
  Read Data from Files
}
\description{
  Read time-series data from files.
}
\usage{
read_ts_tables(file, dir, t.type = "guess",
               start, end, columns,
               return.class = NULL,
               drop.weekends = TRUE,
               column.names = "\%dir\%/\%file\%::\%column\%",
               backend = "csv",
               fread = FALSE)
}
\arguments{
  \item{file}{
    character
}
  \item{dir}{
    character
}
  \item{t.type}{
    character: \code{guess}, \code{Date} or \code{POSIXct}
}
  \item{start}{
    character
}
  \item{end}{
    character
}
  \item{columns}{
    character. \bold{Currently only a single column is supported.}
}
  \item{return.class}{
    NULL or character
}
  \item{drop.weekends}{
    logical
}
  \item{column.names}{

    character: a format string for column names; may contain
    \code{\%dir\%}, \code{\%file\%}, and \code{\%column\%}

  }
  \item{backend}{
    character: currently, \code{csv} and \code{monetdb} are supported
  }
  \item{fread}{
    logical: use \code{\link[data.table]{fread}}
    from package \pkg{data.table}?
  }

}
\details{
  Read time-series data from csv files.
}
\value{
  When return.class is \code{NULL}, a list:
 \item{data}{a numeric matrix}
 \item{timestamp}{Date or POSIXct}
 \item{columns}{character}
 \item{file.path}{character}

}
\author{
  Enrico Schumann
}
\seealso{
  write_ts_table
}
\examples{
\dontrun{
read_ts_tables(c("table1", "table2"), dir = "~/tsdb",
               columns = "close",
               return.class = "zoo")}
}
