dir <- tempdir()

### dummy data

x <- ts_table(data = 11:15,
              timestamp = as.Date("2016-1-1") + 1:5,
              columns = "A")
y <- ts_table(cbind(1:5, 6:10),
              as.Date("2016-1-1") + 1:5,
              c("B", "A"))

write_ts_table(x, dir, "A", replace.file = TRUE)
write_ts_table(y, dir, "BA", replace.file = TRUE)



## ----------------

### a file should never be overwritten
### unless write_ts_table has 'add',
### 'overwrite' or 'replace.file' set

tstamp <- as.Date("2016-1-1") + 1:5
data <- 11:15
x <- ts_table(data = data,
              timestamp = tstamp,
              columns = "A")
write_ts_table(x, dir, "A", replace.file = TRUE)
x1 <- ts_table(data = 1,
               timestamp = as.Date("2016-1-1"),
               columns = "A")
suppressMessages(write_ts_table(x1, dir, "A"))

x2 <- read_ts_tables("A", dir)
expect_equal(x2$timestamp, tstamp)
expect_equal(unname(x2$data), as.matrix(data))



## ----------------

tstamp <- as.Date("2016-1-1") + 1:5
data <- 11:15
x <- ts_table(data = data,
              timestamp = tstamp,
              columns = "A")
write_ts_table(x, dir, "A", replace.file = TRUE)
x0 <- ts_table(data = 16,
               timestamp = as.Date("2016-1-1") + 6,
               columns = "A")
write_ts_table(x0, dir, "A", add = TRUE)
x1 <- read_ts_tables("A", dir)
expect_equal(x1$timestamp, as.Date("2016-1-1") + 1:6)
expect_equal(unname(x1$data), as.matrix(11:16))



## ----------------

### add

x <- ts_table(data = 11:15,
              timestamp = as.Date("2016-1-1") + 1:5,
              columns = "A")
write_ts_table(x, dir, "A", replace.file = TRUE)
x0 <- ts_table(data = 5,
               timestamp = as.Date("1965-1-1"),
               columns = "A")
write_ts_table(x0, dir, "A", add = TRUE)
x1 <- read_ts_tables("A", dir)
expect_equal(x1$timestamp,
             c(as.Date("1965-1-1"),
               as.Date("2016-1-1") + 1:5))
expect_equal(unname(x1$data), as.matrix(c(5, 11:15)))


## ----------------

### overwrite with more than one column

tstamp <- as.Date("2016-1-1") + 1:5

data0 <- cbind(1:5, 6:10)
x <- ts_table(data0, tstamp, c("B", "A"))
write_ts_table(x, dir, "BA", replace.file = TRUE)

data1 <- cbind(1:5, 11:15)
x1 <- ts_table(data1, tstamp, c("B", "A"))
write_ts_table(x, dir, "BA", add = TRUE)

x2 <-read_ts_tables("BA", dir)
expect_equal(x2$timestamp, tstamp)
expect_equal(unname(x2$data), data0)


x1 <- ts_table(data1, tstamp, c("B", "A"))
write_ts_table(x1, dir, "BA", overwrite = TRUE)
x2 <-read_ts_tables("BA", dir)
expect_equal(x2$timestamp, tstamp)
expect_equal(unname(x2$data), data1)



## ----------------

### timestamp
tstamp <- as.Date("2016-1-1") + 1:5
data <- 11:15
x <- ts_table(data = data,
              timestamp = tstamp,
              columns = "A")
write_ts_table(x, dir, "A", replace.file = TRUE)

y <- read_ts_tables("A", dir,
                    timestamp = as.Date("2016-1-1") + 1:2)

expect_equal(y$timestamp, as.Date("2016-1-1") + 1:2)
expect_equal(unname(y$data), as.matrix(data[1:2]))





















## --------------------------------------


y <- ts_table(11:15, as.Date("2016-1-1") - 5:1, "close")

expect_equal(y,
             structure(11:15,
                       .Dim = c(5L, 1L),
                       timestamp = c(16796, 16797, 16798, 16799, 16800),
                       t.type = "Date",
                       columns = "close",
                       class = "ts_table"))

tmp <- as.matrix(11:15); colnames(tmp) <- "close"
expect_equal(zoo::as.zoo(y),
             zoo::zoo(tmp, as.Date("2016-1-1")-5:1))

expect_equal(as.ts_table(zoo::as.zoo(y)), y)

## check unnaming
x <- as.matrix(11:15)
colnames(x) <- "test_name"
y <- ts_table(x, as.Date("2016-1-1")-5:1, "close")
expect_equal(y,
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
expect_equal(y,
             structure(11:15, .Dim = c(5L, 1L),
                       timestamp = c(1451642400, 1451642401,
                                     1451642402, 1451642403,
                                     1451642404),
                       t.type = "POSIXct",
                       columns = "close",
                       class = "ts_table"))

x <- ts_table(data = 11:15,
              timestamp = as.Date("2016-1-1") + 1:5,
              columns = "A")
y <- ts_table(cbind(1:5, 6:10),
              as.Date("2016-1-1") + 1:5,
              c("B", "A"))

dir <- tempdir()
write_ts_table(x, dir, "A", replace.file = TRUE)
write_ts_table(y, dir, "BA", replace.file = TRUE)
read_ts_tables("A", dir, columns = "A")
read_ts_tables("BA", dir, columns = "A")

tmp <- read_ts_tables(c("A", "BA"), dir, columns = c("A"),
                      start = "2016-1-1", drop.weekends = FALSE)

expect_equal(tmp$data,
             structure(c(11, 12, 13, 14, 15, 6, 7, 8, 9, 10),
                       .Dim = c(5L, 2L)))
expect_equal(tmp$timestamp,
             structure(c(16802, 16803, 16804,
                         16805, 16806),
                       class = "Date"))

tmp <- read_ts_tables(c("A", "BA"), dir, columns = c("A"),
                      start = "2016-1-1",
                      drop.weekends = FALSE,
                      return.class = "zoo")
colnames(tmp) <- c("A1", "A2")

expect_equal(tmp,
             structure(c(11, 12, 13, 14, 15, 6, 7, 8, 9, 10),
                       .Dim = c(5L, 2L),
                       .Dimnames = list(NULL, c("A1", "A2")),
                       index = structure(
                           c(16802, 16803, 16804, 16805, 16806),
                           class = "Date"),
                       class = "zoo"))



## POSIXct: no fractions
z1 <- ts_table(11:15,
               as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+0:4,
               "close")
suppressMessages(write_ts_table(z1, dir, "X1"))
z2 <- ts_table(1:5,
               as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+1:5,
               "close")
suppressMessages(write_ts_table(z2, dir, "X2"))
z12 <- read_ts_tables(c("X1", "X2"), dir, columns = "close",
                      start = as.POSIXct("2016-1-1 10:00:00", tz = "UTC"),
                      end = as.POSIXct("2016-1-1 10:00:20", tz = "UTC"))

expect_equal(z12$data,
             structure(c(11, 12, 13, 14, 15, NA,
                         NA, 1, 2, 3, 4, 5),
                       .Dim = c(6L, 2L)))

expect_equal(z12$timestamp,
             as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+0:5)

z12 <- read_ts_tables(c("X1", "X2"), dir, columns = "close",
                      start = "2016-1-1 11:00:00",
                      end   = "2016-1-1 11:00:20")

expect_equal(z12$data,
             structure(c(11, 12, 13, 14, 15, NA,
                         NA, 1, 2, 3, 4, 5),
                       .Dim = c(6L, 2L)))

expect_equal(z12$timestamp,
             as.POSIXct("2016-1-1 10:00:00", tz = "UTC")+0:5)






## POSIXct: fractions
x <- ts_table(data = 1:5,
              timestamp = as.POSIXct("2019-1-1 10:00:00", tz = "UTC") +
                  seq(0.1,0.5,by = 0.1),
              columns = "P")
write_ts_table(x, dir, "P", replace.file = TRUE)
ans <- read_ts_tables("P", dir = dir)
expect_equivalent(c(ans$data), 1:5)
expect_equivalent(unclass(ans$timestamp),
                  tsdb:::.timestamp(x))


## POSIXct: fractions && specify timestamp
ans <- read_ts_tables("P", dir = dir,
                      timestamp = as.POSIXct("2019-1-1 10:00:00",
                                             tz = "UTC") + 0.3)
expect_equivalent(c(ans$data), 3)
expect_equivalent(ans$timestamp,
                  as.POSIXct("2019-1-1 10:00:00", tz = "UTC") + 0.3)



x <- ts_table(data = 1,
              timestamp = as.Date("2016-1-1") ,
              columns = "A B")
dir <- tempdir()
write_ts_table(x, dir, "ab", replace.file = TRUE)
expect_equal(read_ts_tables("ab", dir)$columns, "A B")

x <- ts_table(data = cbind(1, 1),
              timestamp = as.Date("2016-1-1") ,
              columns = c("A B", "C D"))
dir <- tempdir()
write_ts_table(x, dir, "ab", replace.file = TRUE)
expect_equal(read_ts_tables("ab", dir)$columns, c("A B", "C D"))









## -

### set start and end (Date)
x <- ts_table(data = 0:100,
              timestamp = as.Date("2016-1-1") + 0:100,
              columns = "A")
dir <- tempdir()
write_ts_table(x, dir, "A", replace.file = TRUE)
expect_equal(read_ts_tables("A", dir)$columns, "A")

ts <- read_ts_tables("A", dir,
                     start = as.Date("2016-02-01"),
                     end =   as.Date("2016-03-01"))
expect_equal(min(ts$timestamp),
             as.Date("2016-02-01"))
expect_equal(max(ts$timestamp),
             as.Date("2016-03-01"))
expect_equivalent(31:60, drop(ts$data))

ts <- read_ts_tables("A", dir,
                     start = as.Date("2016-02-01"),
                     end =   as.Date("2019-03-01"))
expect_equal(min(ts$timestamp),
             as.Date("2016-02-01"))
expect_equal(max(ts$timestamp),
             max(as.Date("2016-1-1") + 0:100))
expect_equivalent(31:100, drop(ts$data))

ts <- read_ts_tables("A", dir,
                     start = as.Date("2012-02-01"),
                     end =   as.Date("2019-03-01"))
expect_equal(min(ts$timestamp),
             as.Date("2016-01-01"))
expect_equal(max(ts$timestamp),
             max(as.Date("2016-1-1") + 0:100))
expect_equivalent(as.matrix(0:100), ts$data)












dir <- tempdir()

x <- ts_table(data = 11:15,
              timestamp = as.Date("2016-1-1") + 1:5,
              columns = "x")
if (file.exists(file.path(dir, "x")))
    file.remove(file.path(dir, "x"))
ans <- write_ts_table(x, dir, "x")
expect_equal(ans, nrow(x))
expect_equal(readLines(file.path(dir, "x")),
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
expect_equal(ans, 1)
expect_equal(readLines(file.path(dir, "x")),
             c("\"timestamp\",\"x\"",
               "16802,11",
               "16803,12",
               "16804,13",
               "16805,14",
               "16806,15",
               "16807,16"))


## ... write again: no new data point is written
ans <- write_ts_table(x, dir, "x", add = TRUE)
expect_equal(ans, 0)
expect_equal(readLines(file.path(dir, "x")),
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
expect_equal(ans, 1)
expect_equal(readLines(file.path(dir, "x")),
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
expect_equal(ans, 0)
expect_equal(readLines(file.path(dir, "x")),
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
expect_equal(ans, 0)
expect_equal(readLines(file.path(dir, "x")),
             c("\"timestamp\",\"x\"",
               "16436,1",
               "16802,11",
               "16803,12",
               "16804,13",
               "16805,14",
               "16806,15",
               "16807,16"))

ans <- write_ts_table(x, dir, "x", add = TRUE)
expect_equal(ans, 0)
expect_equal(readLines(file.path(dir, "x")),
             c("\"timestamp\",\"x\"",
               "16436,1",
               "16802,11",
               "16803,12",
               "16804,13",
               "16805,14",
               "16806,15",
               "16807,16"))

ans <- write_ts_table(x, dir, "x", add = TRUE, overwrite = TRUE)
expect_equal(ans, 0)
expect_equal(readLines(file.path(dir, "x")),
             c("\"timestamp\",\"x\"",
               "16436,1",
               "16802,11",
               "16803,12",
               "16804,13",
               "16805,14",
               "16806,15",
               "16807,16"))

ans <- write_ts_table(x, dir, "x", add = FALSE, overwrite = TRUE)
expect_equal(ans, 0)
expect_equal(readLines(file.path(dir, "x")),
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
expect_equal(ans, 0)
expect_equal(readLines(file.path(dir, "x")),
             c("\"timestamp\",\"x\"",
               "16436,1",  ## value remains unchanged
               "16802,11",
               "16803,12",
               "16804,13",
               "16805,14",
               "16806,15",
               "16807,16"))

ans <- write_ts_table(x, dir, "x", add = TRUE, overwrite = TRUE)
expect_equal(ans, 1)
expect_equal(readLines(file.path(dir, "x")),
             c("\"timestamp\",\"x\"",
               "16436,2",  ## value is changed because of 'overwrite'
               "16802,11",
               "16803,12",
               "16804,13",
               "16805,14",
               "16806,15",
               "16807,16"))



## ----------------

## create empty ts_table; read/write an empty file
x <- ts_table(numeric(0),
              timestamp = Sys.Date()[0],
              columns = "TEST")

ans <- write_ts_table(x, dir, "EMPTY_FILE")
expect_equal(ans, 0)
expect_true(file.exists(file.path(dir, "EMPTY_FILE")))

expect_equal(x,
             ts_table(columns = "TEST"))


writeLines('"timestamp","close"', file.path(dir, "empty"))
em <- read_ts_tables("empty", dir)
expect_equal(em$timestamp, structure(numeric(0), class = "Date"))
expect_equal(mode(em$data), "numeric")
expect_equal(length(em$data), 0)


## ----------------


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
expect_equivalent(ans, 1)
ans <- read_ts_tables("1970", dir, drop.weekends = FALSE)
expect_equal(ans$timestamp, c(dates, as.Date("1970-1-6")))
expect_equivalent(ans$data, as.matrix(c(seq_along(dates), 99)))



y <- ts_table(11:15, as.Date("2016-1-1")-5:1, "close")
expect_equivalent(zoo::as.zoo(y),
                  zoo::zoo(as.matrix(y), as.Date("2016-1-1")-5:1))

y <- zoo::zoo(11:15, as.Date("2016-1-1")-5:1)
expect_equal(as.ts_table(y, columns = "close"),
             structure(11:15,
                       .Dim = c(5L, 1L),
                       timestamp = c(16796, 16797,
                                     16798, 16799, 16800),
                       t.type = "Date", columns = "close",
                       class = "ts_table"))
