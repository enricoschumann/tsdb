## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

test.ts_table <- function() {

    require("zoo")
    x <- ts_table(11:15, as.Date("2016-1-1")-5:1, "close")

    checkEquals(x,
                structure(11:15,
                          .Dim = c(5L, 1L),
                          timestamp = c(16796, 16797, 16798, 16799, 16800),
                          t.type = "Date",
                          columns = "close",
                          class = "ts_table"))

    tmp <- as.matrix(11:15); colnames(tmp) <- "close"    
    checkEquals(as.zoo(x),
                zoo(tmp, as.Date("2016-1-1")-5:1))

    checkEquals(as.ts_table(as.zoo(x)), x)


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

##    require(tsdb);require(RUnit)
    dir <- tempdir()

    x <- ts_table(data = 11:15,
                  timestamp = as.Date("2016-1-1") + 1:5,
                  columns = "X")
    ans <- write_ts_table(x, dir, "X")
    message(dir, "   ", ans, "  ", nrow(x))
    checkEquals(ans, nrow(x))
    read_ts_tables("X", dir)

    ## add = TRUE: one new data point is found
    x <- ts_table(data = 11:16,
                  timestamp = as.Date("2016-1-1") + 1:6,
                  columns = "X")
    ans <- write_ts_table(x, dir, "X", add = TRUE)
    checkEquals(ans, 1)

    ## ... write again: no new data point is written
    ans <- write_ts_table(x, dir, "X", add = TRUE)
    checkEquals(ans, 0)

    ## add a single new data point
    x <- ts_table(data = 1,
                  timestamp = as.Date("2015-1-1"),
                  columns = "X")
    ans <- write_ts_table(x, dir, "X", add = TRUE)
    checkEquals(ans, 1)

    ## ... write again: no new data point is written
    x <- ts_table(data = 1,
                  timestamp = as.Date("2015-1-1"),
                  columns = "X")
    ans <- write_ts_table(x, dir, "X")
    checkEquals(ans, 0)

}
