## write nf time-series to files, then read/merge them

nf <- 100    ## number of files
nd <- 250*25  ## number of days per time-series (~25 years)



## ----------------------------------------------
library("tsdb")
library("zoo")

d <- tempdir()
x <- 1:nd
z0 <- zoo(x, as.Date("2007-12-31") + x)

trials <- 10

f <- function(s)
    format(s, width = 31)

message("\nRun ", trials, " trials:")
message("  ", nf, " files of length ", nd, "\n")


message(f("write files (from zoo)"), appendLF = FALSE)
t <- system.time(
    for (j in 1:trials ) {
        for (i in 1:nf) {
            z <- z0 + i/10000
            write_ts_table(as.ts_table(z, columns = "A"),
                           file = i, dir = d, replace.file = TRUE)}
    }
)
message(round(t[[3]]/trials, 2), " seconds")




message(f("read/merge 10 years"), appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   start = "2007-1-1", end = "2016-12-31",
                   return.class = "zoo", column.names = "%file%")
    }
)
message(round(t[[3]]/trials, 2), " seconds")




message(f("(f)read/merge 10 years"), appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   start = "2007-1-1", end = "2016-12-31",
                   return.class = "zoo", column.names = "%file%",
                   read.fn = "fread")
    }
)
message(round(t[[3]]/trials, 2), " seconds")




message(f("read/merge complete series"), appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   return.class = "zoo", column.names = "%file%")
    }
)
message(round(t[[3]]/trials, 2), " seconds")




message(f("(f)read/merge complete series"), appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   return.class = "zoo", column.names = "%file%",
                   read.fn = "fread")
    }
)
message(round(t[[3]]/trials, 2), " seconds")



x <- 1:1000000
z <- zoo(x, as.POSIXct("2001-1-1") + x)
write_ts_table(as.ts_table(z, columns = "A"),
               file = "single", dir = d, replace.file = TRUE)
message("\nRun ", trials, " trials:")
message("  ", 1, " file of length ", length(x), "\n")

message(f("read a single file"), appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
        ignore <- read_ts_tables("single", dir = d,
                       start = min(index(z)), end = max(index(z)),
                       return.class = "zoo", column.names = "%file%")
    }
)
message(round(t[[3]]/trials, 2), " seconds")

## ---------------------------------------------------

message(f("(f)read a single file"), appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
        ignore <- read_ts_tables("single", dir = d,
                       start = min(index(z)), end = max(index(z)),
                       return.class = "zoo", column.names = "%file%",
                       read.fn = "fread")
    }
)
message(round(t[[3]]/trials, 2), " seconds")

