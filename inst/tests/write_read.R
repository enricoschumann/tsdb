## write nf time-series to files, then read/merge them

nf <- 100    ## number of files
nd <- 6000   ## number of days per time-series


library("tsdb")
library("zoo")

d <- tempdir()
x <- 1:nd
z0 <- zoo(x, as.Date("2007-12-31") + x)

trials <- 10


message("")
message("Run ", trials, " trials:")
message("  ", nf, " files of length ", nd)
message("")

message("Write files", appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
        for (i in 1:nf) {
            z <- z0 + i/10000
            write_ts_table(as.ts_table(z, columns = "A"),
                           file = i, dir = d, replace.file = TRUE)}
    }
)
message(":                   ", round(t[[3]]/trials, 2), " seconds")



message("Read/merge 10 years", appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   start = "2007-1-1", end = "2016-12-31",
                   return.class = "zoo", column.names = "%file%")
    }
)
message(":           ", round(t[[3]]/trials, 2), " seconds")



message("Read (fread)/merge 10 years", appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   start = "2007-1-1", end = "2016-12-31",
                   return.class = "zoo", column.names = "%file%",
                   read.fn = "fread")
    }
)
message(":   ", round(t[[3]]/trials, 2), " seconds")




message("Read/merge", appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   return.class = "zoo", column.names = "%file%")
    }
)
message(":                    ",
        round(t[[3]]/trials, 2), " seconds")



message("Read (fread)/merge", appendLF = FALSE)
t <- system.time(
    for (i in 1:trials ) {
    read_ts_tables(as.character(1:nf), dir = d,
                   return.class = "zoo", column.names = "%file%",
                   read.fn = "fread")
    }
)
message(":            ",
        round(t[[3]]/trials, 2), " seconds")
