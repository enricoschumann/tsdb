## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

test.ts_table <- function() {

    ## library("RUnit")
    ## library("tsdb")
    ## library("zoo", warn.conflicts = FALSE)
    y <- ts_table(11:15, as.Date("2016-1-1")-5:1, "close")

    checkEquals(y,
                structure(11:15,
                          .Dim = c(5L, 1L),
                          timestamp = c(16796, 16797, 16798, 16799, 16800),
                          t.type = "Date",
                          columns = "close",
                          class = "ts_table"))

    tmp <- as.matrix(11:15); colnames(tmp) <- "close"    
    checkEquals(zoo::as.zoo(y),
                zoo::zoo(tmp, as.Date("2016-1-1")-5:1))

    checkEquals(as.ts_table(zoo::as.zoo(y)), y)

    ## check unnaming
    x <- as.matrix(11:15)
    colnames(x) <- "test_name"
    y <- ts_table(x, as.Date("2016-1-1")-5:1, "close")
    checkEquals(y,
                structure(11:15,
                          .Dim = c(5L, 1L),
                          timestamp = c(16796, 16797, 16798, 16799, 16800),
                          t.type = "Date",
                          columns = "close",
                          class = "ts_table"))

    ## intraday
    y <- ts_table(11:15,
                  as.POSIXct("2016-1-1 10:00:00", tz = "UTC") + 0:4,
                  "close")
    checkEquals(y,
                structure(11:15, .Dim = c(5L, 1L),
                          timestamp = c(1451642400, 1451642401,
                                        1451642402, 1451642403,
                                        1451642404),
                          t.type = "POSIXct",
                          columns = "close",
                          class = "ts_table"))

}

test.read_ts_tables <- function() {

    ## library("RUnit")
    ## library("tsdb")
    ## library("zoo", warn.conflicts = FALSE)
    x <- ts_table(data = 11:15,
                  timestamp = as.Date("2016-1-1") + 1:5,
                  columns = "A")
    y <- ts_table(cbind(1:5, 6:10),
                  as.Date("2016-1-1") + 1:5,
                  c("B", "A"))

    dir <- tempdir()
    write_ts_table(x, dir, "A")
    write_ts_table(y, dir, "BA")
    read_ts_tables("A", dir, columns = "A")
    read_ts_tables("BA", dir, columns = "A")

    tmp <- read_ts_tables(c("A", "BA"), dir, columns = c("A"),
                          start = "2016-1-1", drop.weekends = FALSE)

    checkEquals(tmp$data,
                structure(c(11, 12, 13, 14, 15, 6, 7, 8, 9, 10),
                          .Dim = c(5L, 2L)))
    checkEquals(tmp$timestamp,
                structure(c(16802, 16803, 16804,
                            16805, 16806),
                          class = "Date"))

    tmp <- read_ts_tables(c("A", "BA"), dir, columns = c("A"),
                          start = "2016-1-1",
                          drop.weekends = FALSE,
                          return.class = "zoo")
    colnames(tmp) <- c("A1", "A2")

    checkEquals(tmp,
                structure(c(11, 12, 13, 14, 15, 6, 7, 8, 9, 10),
                          .Dim = c(5L, 2L),
                          .Dimnames = list(NULL, c("A1", "A2")),
                          index = structure(
                              c(16802, 16803, 16804, 16805, 16806),
                              class = "Date"),
                          class = "zoo"))

                
    
    ## check POSIXct
    z1 <- ts_table(11:15,
                   as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+0:4,
                   "close")
    write_ts_table(z1, dir, "X1")
    z2 <- ts_table(1:5,
                   as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+1:5,
                   "close")
    write_ts_table(z2, dir, "X2")
    z12 <- read_ts_tables(c("X1", "X2"), dir, columns = "close",
                          start = as.POSIXct("2016-1-1 10:00:00", tz = "UTC"),
                          end = as.POSIXct("2016-1-1 10:00:20", tz = "UTC"))
    
    checkEquals(z12$data,
                structure(c(11, 12, 13, 14, 15, NA,
                            NA, 1, 2, 3, 4, 5),
                          .Dim = c(6L, 2L)))
    
    checkEquals(z12$timestamp,
                as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+0:5)
    
    z12 <- read_ts_tables(c("X1", "X2"), dir, columns = "close",
                          start = "2016-1-1 11:00:00",
                          end   = "2016-1-1 11:00:20")
    
    checkEquals(z12$data,
                structure(c(11, 12, 13, 14, 15, NA,
                            NA, 1, 2, 3, 4, 5),
                          .Dim = c(6L, 2L)))
    
    checkEquals(z12$timestamp,
                as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+0:5)
    
    
    
    ## check empty file
    writeLines('"timestamp","close"', file.path(dir, "empty"))
    em <- read_ts_tables("empty", dir)
    checkEquals(em$timestamp, structure(numeric(0), class = "Date"))
    checkEquals(em$data, structure(numeric(0), .Dim = 0:1))
    
}

test.write_ts_table <- function() {

    ## library("RUnit")
    ## library("tsdb")
    ## library("zoo")
    dir <- tempdir()

    x <- ts_table(data = 11:15,
                  timestamp = as.Date("2016-1-1") + 1:5,
                  columns = "x")
    if (file.exists(file.path(dir, "x")))
        file.remove(file.path(dir, "x"))
    ans <- write_ts_table(x, dir, "x")
    checkEquals(ans, nrow(x))
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15"))
    
    ## add = TRUE: one new data point is found
    x <- ts_table(data = 11:16,
                  timestamp = as.Date("2016-1-1") + 1:6,
                  columns = "x")
    ans <- write_ts_table(x, dir, "x", add = TRUE)
    checkEquals(ans, 1)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    
    ## ... write again: no new data point is written
    ans <- write_ts_table(x, dir, "x", add = TRUE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    
    ## add a single new data point
    x <- ts_table(data = 1,
                  timestamp = as.Date("2015-1-1"),
                  columns = "x")
    ans <- write_ts_table(x, dir, "x", add = TRUE)
    checkEquals(ans, 1)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    
    ## ... write again: no new data point is written
    x <- ts_table(data = 1,
                  timestamp = as.Date("2015-1-1"),
                  columns = "x")
    ans <- suppressMessages(write_ts_table(x, dir, "x"))
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))


    ## writing an empty ts_table does no harm
    x <- ts_table(numeric(0),
                  timestamp = Sys.Date()[0],
                  columns = "x")
    ans <- write_ts_table(x, dir, "x")
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    ans <- write_ts_table(x, dir, "x", add = TRUE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))
    
    ans <- write_ts_table(x, dir, "x", add = TRUE, overwrite = TRUE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    ans <- write_ts_table(x, dir, "x", add = FALSE, overwrite = TRUE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    ## overwrite vl
    x <- ts_table(data = 2,
                  timestamp = as.Date("2015-1-1"),
                  columns = "x")
    ans <- write_ts_table(x, dir, "x", add = TRUE, overwrite = FALSE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16436,1",  ## value remains unchanged
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    ans <- write_ts_table(x, dir, "x", add = TRUE, overwrite = TRUE)
    checkEquals(ans, 1)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"x\"",
                  "16436,2",  ## value is changed because of 'overwrite'
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    ## write an empty file
    x <- ts_table(numeric(0),
                  timestamp = Sys.Date()[0],
                  columns = "TEST")
    
    ans <- write_ts_table(x, dir, "EMPTY_FILE")
    checkEquals(ans, 0)
    checkTrue(file.exists(file.path(dir, "EMPTY_FILE")))



    ## add before 1970
    dates <- seq(from = as.Date("1969-12-25"),
                 to =   as.Date("1970-01-05"),
                 by = "1 day")
    x <- ts_table(seq_along(dates),
                  timestamp = dates,
                  columns = "TEST")
    write_ts_table(x, dir, "1970", replace.file = TRUE)
    x <- ts_table(99,
                  as.Date("1970-1-6"),
                  columns = "TEST")
    ans <- write_ts_table(x, dir, "1970", add = TRUE)
    checkEqualsNumeric(ans, 1)
    ans <- read_ts_tables("1970", dir, drop.weekends = FALSE)
    checkEquals(ans$timestamp, c(dates, as.Date("1970-1-6")))
    checkEqualsNumeric(ans$data, c(seq_along(dates), 99))
    
}

test.zoo <- function() {

    ## library("RUnit")
    ## library("tsdb")
    ## library("zoo", warn.conflicts = FALSE)

    y <- ts_table(11:15, as.Date("2016-1-1")-5:1, "close")
    checkEqualsNumeric(zoo::as.zoo(y),
                       zoo::zoo(as.matrix(y), as.Date("2016-1-1")-5:1))

    y <- zoo::zoo(11:15, as.Date("2016-1-1")-5:1)
    checkEquals(as.ts_table(y, columns = "close"),
                structure(11:15,
                          .Dim = c(5L, 1L),
                          timestamp = c(16796, 16797, 
                                        16798, 16799, 16800),
                          t.type = "Date", columns = "close",
                          class = "ts_table"))
}
