\name{tsdb-package}
\alias{tsdb-package}
\alias{tsdb}
\title{
  Terribly-Simple Database for Time Series
}
\description{

  A terribly-simple data base for numeric time series,
  written purely in \R, so no external database-software is
  needed. Series are stored in plain-text files (the
  most-portable and enduring file type) in \acronym{CSV}
  format; timestamps are encoded using \R's native numeric
  representation for
  \code{\link{Date}}/\code{\link{POSIXct}}, which makes them
  fast to parse, but keeps them accessible with other
  software. The package provides tools for saving and
  updating series in this standardised format, for
  retrieving and joining data, for summarising files and
  directories, and for coercing series from and to other
  data types (such as 'zoo' series).

}
\details{

  See the functions \code{\link{ts_table}} and
  \code{\link{as.ts_table}} for creating a
  \code{\link{ts_table}}.

  See \code{\link{write_ts_table}} and
  \code{\link{read_ts_tables}} for storing and loading a
  \code{\link{ts_table}} (or several).

  For getting started, see the tutorial at
  \url{https://gitlab.com/enricoschumann/tsdb/blob/master/README.org}
  or
  \url{https://github.com/enricoschumann/tsdb/blob/master/README.org} .

}
\author{
  Enrico Schumann
}
\seealso{

  \code{\link{ts_table}} and \code{\link{as.ts_table}} for
  creating a \code{\link{ts_table}}

  \code{\link{write_ts_table}} and \code{\link{read_ts_tables}} for
  storing and loading a \code{\link{ts_table}}
}
