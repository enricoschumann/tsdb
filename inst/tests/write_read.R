## write nf time-series to files, then read/merge them

nf <- 1000    ## number of files
nd <- 3700   ## number of days per time-series


library("tsdb")
library("zoo")

d <- tempdir()
x <- 1:nd
z0 <- zoo(x, as.Date("2007-12-31") + x)

trials <- 5
t <- system.time(
    for (i in 1:trials ) {
        for (i in 1:nf) {
            z <- z0 + i/10000
            write_ts_table(as.ts_table(z, columns = "A"),
                           file = i, dir = d, replace.file = TRUE)}
    }
)
t[[3]]/trials


t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   start = "2010-1-1", end = "2015-12-31",
                   return.class = "zoo", column.names = "%file%")
    }
)
t[[3]]/trials

t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   start = "2010-1-1", end = "2015-12-31",
                   return.class = "zoo", column.names = "%file%",
                   fread = TRUE)
    }
)
t[[3]]/trials
