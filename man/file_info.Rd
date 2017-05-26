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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
    character
}
  \item{file}{
    character
}
}
\details{

  Experimental.
  
}
\value{

  An object of type \code{file_info}, which is a
  \code{data.frame}.
  
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{ts_table}}
}
\examples{
\dontrun{
file_info(dir = "/tsdb",
          c("table1", "table2"))}
}
