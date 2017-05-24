## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

test.ts_table <- function() {

    ## require("RUnit")
    ## require("tsdb")
    ## require("zoo")
    y <- ts_table(11:15, as.Date("2016-1-1")-5:1, "close")

    checkEquals(y,
                structure(11:15,
                          .Dim = c(5L, 1L),
                          timestamp = structure(c(16796, 16797, 16798, 16799, 16800),
                                                class = "Date"),
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
                          timestamp = structure(c(16796, 16797, 16798, 16799, 16800),
                                                class = "Date"),
                          t.type = "Date",
                          columns = "close",
                          class = "ts_table"))

    ## intraday
    y <- ts_table(11:15,
                  as.POSIXct("2016-1-1 10:00:00", tz = "UTC") + 0:4,
                  "close")
    checkEquals(y,
                structure(11:15, .Dim = c(5L, 1L),
                          timestamp = structure(c(1451642400, 1451642401,
                                                  1451642402, 1451642403,
                                                  1451642404),
                                                class = c("POSIXct", "POSIXt")),
                          t.type = "POSIXct",
                          columns = "close",
                          class = "ts_table"))

}

test.read_ts_tables <- function() {

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

    checkEquals(tmp[[1]],
                structure(c(11, 12, 13, 14, 15, 6, 7, 8, 9, 10),
                          .Dim = c(5L, 2L)))


    ## z1 <- ts_table(11:15, as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+0:4, "close")
    ## write_ts_table(z1, dir, "X1")
    ## z2 <- ts_table(1:5, as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+1:5, "close")
    ## write_ts_table(z2, dir, "X2")
    ## read_ts_tables(c("X1", "X2"), dir, columns = "close")

    
    ## check empty file
    writeLines('"timestamp","close"', file.path(dir, "empty"))
    em <- read_ts_tables("empty", dir)
    checkEquals(em$timestamp, structure(numeric(0), class = "Date"))
    checkEquals(em$data, structure(numeric(0), .Dim = 0:1))

}

test.write_ts_table <- function() {

    require("RUnit")
    require("tsdb")
    require("zoo")
    dir <- tempdir()

    x <- ts_table(data = 11:15,
                  timestamp = as.Date("2016-1-1") + 1:5,
                  columns = "X")
    if (file.exists(file.path(dir, "X")))
        file.remove(file.path(dir, "X"))
    ans <- write_ts_table(x, dir, "X")
    checkEquals(ans, nrow(x))
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15"))
    
    ## add = TRUE: one new data point is found
    x <- ts_table(data = 11:16,
                  timestamp = as.Date("2016-1-1") + 1:6,
                  columns = "X")
    ans <- write_ts_table(x, dir, "X", add = TRUE)
    checkEquals(ans, 1)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    
    ## ... write again: no new data point is written
    ans <- write_ts_table(x, dir, "X", add = TRUE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    
    ## add a single new data point
    x <- ts_table(data = 1,
                  timestamp = as.Date("2015-1-1"),
                  columns = "X")
    ans <- write_ts_table(x, dir, "X", add = TRUE)
    checkEquals(ans, 1)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
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
                  columns = "X")
    ans <- suppressMessages(write_ts_table(x, dir, "X"))
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    x <- ts_table(numeric(0),
                  timestamp = Sys.Date()[0],
                  columns = "X")
    ans <- write_ts_table(x, dir, "X")
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    ans <- write_ts_table(x, dir, "X", add = TRUE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))
    
    ans <- write_ts_table(x, dir, "X", add = TRUE, overwrite = TRUE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16436,1",
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    ans <- write_ts_table(x, dir, "X", add = FALSE, overwrite = TRUE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
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
                  columns = "X")
    ans <- write_ts_table(x, dir, "X", add = TRUE, overwrite = FALSE)
    checkEquals(ans, 0)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16436,1",  ## value remains unchanged
                  "16802,11",
                  "16803,12",
                  "16804,13", 
                  "16805,14",
                  "16806,15",
                  "16807,16"))

    ans <- write_ts_table(x, dir, "X", add = TRUE, overwrite = TRUE)
    checkEquals(ans, 1)
    checkEquals(readLines(file.path(dir, "x")),
                c("\"timestamp\",\"X\"",
                  "16436,2",  ## value remains unchanged
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
}
