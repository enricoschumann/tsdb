## The package uses code from Enrico Schumann's
## R package 'database'.
## Copyright (C) Enrico Schumann 2010-2021

## ---------------- time

ttime <- function(x, from = "datetime", to = "numeric",
                  tz = "", strip.attr = TRUE,
                  format = "%Y-%m-%d") {

    if (from == "datetime" && to == "numeric") {
        if (strip.attr)
            c(unclass(x))
        else
            unclass(x)
    } else if (from == "numeric" && to == "Date") {
        class(x) <- "Date"
        x
    } else if (from == "numeric" && to == "POSIXct") {
        .POSIXct(x, tz = tz)
    } else if (from == "character" && to == "Date") {
        as.Date(x, format = format)
    } else if (from == "character" && to == "POSIXct") {
        as.POSIXct(x, tz = tz)
    } else
        stop("unknown conversion type")
}



## --------------------- ts_table

ts_table <- function(data, timestamp, columns) {

    if (missing(data) && missing(timestamp)) {
        ans <- as.matrix(numeric(0))
        attr(ans, "timestamp") <- numeric(0)
        attr(ans, "t.type") <- "Date"
        attr(ans, "columns") <- columns
        class(ans) <- "ts_table"
        return(ans)
    }

    if (!inherits(timestamp, "Date") &&
        !inherits(timestamp, "POSIXt"))
        stop(sQuote("timestamp"), " must be Date or POSIXt")

    if (inherits(timestamp, "POSIXlt")) {
        timestamp <- ttime(as.POSIXct(timestamp))
        t.type <- "POSIXct"
    }
    if (inherits(timestamp, "POSIXct")) {
        timestamp <- ttime(timestamp)
        t.type <- "POSIXct"
    }
    if (inherits(timestamp, "Date")) {
        timestamp <- ttime(timestamp)
        t.type <- "Date"
    }
    ans <- as.matrix(data)
    if (missing(columns))
        columns <- colnames(ans)
    ans <- unname(ans)
    if (length(timestamp) != nrow(ans))
        stop("length of timestamp does not match number of rows")
    if (is.null(columns))
        stop("no column names, and ", sQuote("columns"), " not provided")
    if (ncol(ans) != length(columns))
        stop("more columns than column names")
    if (is.unsorted(timestamp)) {
        ii <- order(timestamp)
        timestamp <- timestamp[ii]
        ans <- ans[ii, , drop = FALSE]
    }
    attr(ans, "timestamp") <- timestamp
    attr(ans, "t.type") <- t.type
    attr(ans, "columns") <- columns
    class(ans) <- "ts_table"
    ans
}

write_ts_table <- function(ts, dir, file,
                           add = FALSE,
                           overwrite = FALSE,
                           replace.file = FALSE,
                           backend = "csv") {
    if (!(inherits(ts, "ts_table")))
        stop(sQuote("ts"), " must be a ", sQuote("ts_table"))

    timestamp <- ttime(.timestamp(ts))
    columns   <- .columns(ts)

    backend <- tolower(backend)
    ans <- dim(ts)[1L] ## ts_table is always a matrix
    if (backend == "csv") {
        dfile <- if (missing(dir))
                     file
                 else
                     file.path(dir, file)
        if (replace.file && file.exists(dfile)) {
            file.remove(dfile)
        }
        if (ans == 0L) {
            if (!file.exists(dfile))
                write.table(as.matrix(data.frame(timestamp, unclass(ts))),
                            file = dfile,
                            row.names = FALSE,
                            col.names = c("timestamp", columns),
                            sep = ",")
            return(invisible(0L))
        }
        dfile.exists <- file.exists(dfile)
        if (dfile.exists && overwrite) {
            in_db <- read_ts_tables(file, dir, drop.weekends = FALSE)
            if (any(in_db$columns != columns))
                stop("columns in file differ from columns in ", sQuote("ts"))
            keep <- !ttime(in_db$timestamp) %in% timestamp
            timestamp <- c(ttime(in_db$timestamp)[keep], timestamp)
            ts <- rbind(in_db$data[keep, , drop = FALSE], ts)
            if (is.unsorted(timestamp)) {
                ii <- order(timestamp)
                timestamp <- timestamp[ii]
                ts <- ts[ii, , drop = FALSE]
            }
        } else if (dfile.exists && add) {
            in_db <- read_ts_tables(file, dir, drop.weekends = FALSE)
            if (any(in_db$columns != columns))
                stop("columns in file differ from columns in ", sQuote("ts"))
            new <- !timestamp %in% ttime(in_db$timestamp)
            ans <- 0L
            if (any(new)) {
                ans <- sum(new)
                timestamp <- c(ttime(in_db$timestamp),
                               timestamp[new])
                ts <- rbind(in_db$data, ts[new, ,drop = FALSE])
                if (is.unsorted(timestamp)) {
                    ii <- order(timestamp)
                    timestamp <- timestamp[ii]
                    ts <- ts[ii, , drop = FALSE]
                }
            }
        }
        if (dfile.exists && !overwrite && !add) {
            ans  <- 0
            message("file exists; use ", sQuote("add = TRUE"),
                    " or ", sQuote("overwrite = TRUE"),
                    " to update file")
        } else if (ans > 0L) {
            ## only write if there are rows (ans > 0):
            ##   e.g., if 'add' was true but no new data were
            ##   found, there is no need to rewrite the table
            write.table(as.matrix(data.frame(timestamp, unclass(ts))),
                        file = dfile,
                        row.names = FALSE,
                        col.names = c("timestamp", columns),
                        sep = ",")
        }
    } else
        stop("unknown backend")
    invisible(ans)
}

read_ts_tables <- function(file, dir, t.type = "guess",
                           start, end, columns,
                           return.class = NULL,
                           drop.weekends = FALSE,
                           column.names = "%dir%/%file%::%column%",
                           backend = "csv",
                           read.fn = NULL,
                           frequency = "1 sec",
                           timestamp) {

    backend <- tolower(backend)

    if (backend == "csv") {

        if (missing(dir)) {
            dir <- ""
            dfile <- file
        } else {
            dfile <- file.path(dir, file)
        }

        if (length(dir) != length(file)) {
            if (length(dir) > 1L && length(file) > 1L)
                stop("file and dir lengths must match")

            if (length(file) > 1L) {
                dir <-  rep.int(dir, length(file))
            } else if (length(dir) > 1L)
                file <- rep.int(file, length(dir))
            else
                stop("check lengths of file and dir")
        }

        if (t.type == "guess" || missing(columns) || missing(start)) {
            samp <- readLines(dfile[[1]], n = 2L)
            samp.n <- length(samp)
        }

        if (missing(start) || t.type == "guess") {
            if (samp.n == 2L)
                timestamp1 <- as.numeric(
                    strsplit(samp[[2L]], ",", fixed = TRUE)[[1L]][[1L]])
            else
                timestamp1 <- numeric(0)
        }

        if (t.type == "guess") {
            t.type <- if (samp.n == 2L && timestamp1 > 43200) ## 86400/2
                          "POSIXct"
                      else
                          "Date"
        }

        if (missing(columns)) {
            tmp <- gsub("\"", "",
                        strsplit(samp[[1]], ",", fixed = TRUE)[[1]])
            columns <- tmp[-1L]
        }

        do.match <- (length(dfile) > 1L || !missing(timestamp)) &&
                    !is.na(frequency)
        if (t.type == "Date") {
            start <- if (missing(start) && length(timestamp1))
                         ttime(timestamp1, "numeric", "Date")
                     else if (missing(start))
                         ## a dummy date, only used for
                         ## empty files
                         as.Date("1970-01-01")
                     else
                         as.Date(start)

            end   <- if (missing(end))
                         if (drop.weekends)
                             previous_businessday(Sys.Date())
                         else
                             Sys.Date()
                     else
                         as.Date(end)
            if (do.match) {
                if (missing(timestamp))
                    timestamp <- seq(start, end , "1 day")
                if (drop.weekends)
                    timestamp <- timestamp[is_businessday(timestamp)]
                timestamp <- c(unclass(timestamp))
            }
        } else if (t.type == "POSIXct") {
            start <- if (missing(start))
                         ttime(timestamp1,
                               from = "numeric", to = "POSIXct")
                     else
                         as.POSIXct(start)  ## in case it is POSIXlt

            end   <- if (missing(end))
                         if (drop.weekends)
                             as.POSIXct(previous_businessday(Sys.Date()))
                         else
                             Sys.time()
                     else
                         as.POSIXct(end)

            if (!is.na(frequency) && frequency != "1 sec" && do.match) {
                start <- roundPOSIXt(start, frequency)
                end   <- roundPOSIXt(end,   frequency, up = TRUE)
            }
            if (do.match) {
                if (missing(timestamp))
                    timestamp <- seq(start, end , frequency)
                if (drop.weekends)
                    timestamp <- timestamp[is_businessday(timestamp)]
                timestamp <- c(unclass(timestamp))
            }
        } else
            stop("unknown ", sQuote("t.type"))

        nc <- length(columns)
        if (do.match)
            results <- array(NA_real_,
                             dim = c(length(timestamp),
                                     length(dfile)*nc))
        else if (length(dfile) > 1L)
            tmp.results <- list(data = as.list(rep(NA_real_, length(dfile))),
                                timestamp = as.list(rep(NA_real_, length(dfile))))
        for (i in seq_along(dfile)) {
            if (is.null(read.fn))
                tmp <- read.table(dfile[[i]],
                                  sep = ",",
                                  stringsAsFactors = FALSE,
                                  header = TRUE,
                                  colClasses = "numeric",
                                  check.names = FALSE)
            else if (read.fn == "fread")
                tmp <- data.table::fread(dfile[[i]],
                                         sep = ",",
                                         header = TRUE,
                                         data.table = FALSE,
                                         check.names = FALSE)
            else
                stop("unknown ", sQuote("read.fn"))

            tmp.names <- colnames(tmp)
            if (!all(columns %in% tmp.names)) {
                warning("columns missing")
                tmp <- cbind(tmp, rep(NA, sum(!(columns %in% tmp.names))))
                colnames(tmp) <- c(tmp.names,
                                   columns[!(columns %in% tmp.names)])
            }
            if (do.match) {
                ii <- fmatch(tmp[[1L]], timestamp, nomatch = 0L)
                res <- tmp[, columns, drop = FALSE][ii > 0L, , drop=FALSE]
                if (!is.null(res))
                    results[ii, (nc*(i-1)+1):(nc*i)] <- as.matrix(res)
            } else {
                timestamp <- tmp[[1L]]
                results <- as.matrix(tmp[, columns, drop = FALSE])

                ## if 'results' has no rows, 'as.matrix'
                ## will return an empty array of mode
                ## 'logical'
                if (storage.mode(results) == "logical")
                    storage.mode(results) <- "numeric"

                ii <- timestamp >= start & timestamp <= end
                timestamp <- timestamp[ii]
                results <- results[ii, , drop = FALSE]

                if (length(dfile) > 1L) {
                    ## more than one file ==> first
                    ## collect all data/timestamps
                    tmp.results$data[[i]] <- results
                    tmp.results$timestamp[[i]] <- timestamp
                }
            }
        }

        if (!do.match && length(dfile) > 1L) {
            timestamp <- sort(unique(unlist(tmp.results$timestamp)))
            results <- array(NA_real_,
                             dim = c(length(timestamp),
                                     length(dfile)*nc))
            for (i in seq_along(dfile)) {
                ii <- fmatch(tmp.results$timestamp[[i]], timestamp, nomatch = 0L)
                res <- tmp.results$data[[i]][, columns, drop = FALSE][ii > 0L, , drop = FALSE]
                if (!is.null(res))
                    results[ii, (nc*(i-1)+1):(nc*i)] <- as.matrix(res)
            }

        }

        rm <- rowSums(is.na(results)) == dim(results)[[2L]]
        results <- results[!rm, , drop = FALSE]
        timestamp <- timestamp[!rm]

        if (drop.weekends && !do.match) {
            ii <- is_businessday(ttime(timestamp, from = "numeric", t.type))
            results <- results[ii, , drop = FALSE]
            timestamp <- timestamp[ii]
        }
        colnames <- if (length(column.names) == 1L)
                        rep.int(column.names, dim(results)[[2L]])
                    else
                        column.names
        .dir <- rep(dir, each = length(columns))
        .file <- rep(file, each = length(columns))
        .columns <- rep(columns, length(.dir)/length(columns))
        for (i in seq_along(.dir)) {
            colnames[[i]] <- gsub("%dir%",    .dir[[i]],     colnames[[i]])
            colnames[[i]] <- gsub("%file%",   .file[[i]],    colnames[[i]])
            colnames[[i]] <- gsub("%column%", .columns[[i]], colnames[[i]])
        }

    } else
        stop("unknown backend")


    if (is.null(return.class)) {
        list(data = results,
             timestamp = ttime(timestamp, from = "numeric", t.type),
             columns = rep(columns, each = length(dfile)),
             file.path = paste(rep(dfile, each = length(columns)),
                               columns, sep = "::"))
    } else if (return.class == "zoo") {
        if (!requireNamespace("zoo"))
            stop("package ", sQuote("zoo"), " not available")
        if (!is.null(dim(results)))
            colnames(results) <- colnames
        zoo(results, ttime(timestamp, from = "numeric", t.type))
    } else if (return.class == "data.frame") {
        ans <- data.frame(ttime(timestamp, from = "numeric", t.type), results)
        colnames(ans) <- c("timestamp", colnames)
        ans
    } else if (return.class == "ts_table") {
        ans <- as.matrix(ans)
        dimnames(ans) <- NULL
        attr(ans, "t.type") <- t.type
        attr(ans, "timestamp") <- timestamp
        attr(ans, "columns") <- columns
        class(ans) <- "ts_table"
        ans
    } else
        stop("unknown ", sQuote("return.class"))
}

print.ts_table <- function(x, ...) {
    tmp <- .timestamp(x)
    from_to <- if (length(tmp))
                   ttime(range(tmp), "numeric", .t.type(x))
               else
                   c(NA, NA)
    if (nrow(x))
        cat(nrow(x), " rows [",
            as.character(from_to[[1L]]), " -> ",
            as.character(from_to[[2]]),
            "]: ",
            paste(attr(x, "columns"), collapse = ", "),
            "\n", sep = "")
    else
        cat(nrow(x), " rows : ",
            paste(attr(x, "columns"), collapse = ", "),
            "\n", sep = "")
    invisible(x)
}

dir_info <- function(dir = getwd()) {
    res <- dir()
    class(res) <- "dir_info"
    res
}

print.dir_info <- function(x, ...) {
    print(unclass(x), ...)
}


## --------------------- COERCION

as.ts_table <- function(x, ...) {
    UseMethod("as.ts_table")
}

as.ts_table.ts_table <- function(x, ...)
    x

as.ts_table.zoo <- function(x, columns, ...) {
    cols <- if (missing(columns))
                colnames(x)
            else columns
    if (is.null(cols))
        stop("no column names, and ", sQuote("columns"), " not provided")
    ts_table(unname(coredata(x)), index(x), cols)
}

as.zoo.ts_table <- function(x, ...) {
    ans <- zoo(unname(as.matrix(x)),
               ttime(.timestamp(x), "numeric", .t.type(x)))
    colnames(ans) <- .columns(x)
    ans
}

as.data.frame.ts_table <- function(x,
                                   row.names = NULL,
                                   optional = FALSE, ...) {
        timestamp <- attr(x, "timestamp")
        col <- attr(x, "columns")
        if (!is.null(row.names)) {
            ans <- data.frame(unclass(x), stringsAsFactors = FALSE)
            row.names(ans) <- as.character(timestamp)
            names(ans) <- col
        } else {
            ans <- cbind(timestamp = timestamp,
                         data.frame(unclass(x),
                                    stringsAsFactors = FALSE))
            names(ans) <- c("timestamp", col)
        }
        ans
}

as.matrix.ts_table <- function(x, ...) {
        timestamp <- .timestamp(x)
        col <- .columns(x)
        d <- dim(x)
        ans <- c(x)
        dim(ans) <- d
        colnames(ans) <- col
        rownames(ans) <- as.character(ttime(timestamp,
                                            from = "numeric",
                                            to = .t.type(x)))
        ans
}


## --------------------- file_info

file_info <- function(dir, file) {
    dfile <- if (missing(dir))
                 file
             else
                 file.path(dir, file)

    nf <- length(dfile)
    res <- data.frame(file = file,
                      dir_file = dfile,
                      exists = file.exists(dfile),
                      columns = character(nf),
                      nrows = NA,
                      t.type = NA,
                      min.timestamp = NA,
                      max.timestamp = NA,
                      stringsAsFactors = FALSE)

    for (i in seq_len(nf)) {
        if (!res[["exists"]][i])
            next
        fi <- try(read_ts_tables(dfile[i], return.class = NULL),
                  silent = TRUE)
        if (inherits(fi, "try-error"))
            next
        res[["nrows"]][i] <- length(fi$timestamp)
        if (length(fi$timestamp)) {
            res[["min.timestamp"]][i] <- min(fi$timestamp)
            res[["max.timestamp"]][i] <- max(fi$timestamp)
            res[["t.type"]][i] <- class(fi$timestamp)
        }
    }
    class(res) <- c("file_info", "data.frame")
    res
}

print.file_info <- function(x, ...) {
    print.data.frame(x, ...)
    invisible(x)
}



## --------------------- internal/incomplete functions

adjust_ts_table <- function(ts, dividends, splits, splits.first = TRUE) {

}

rm_ts_table <- function(file, dir, ..., trash.bin = ".trash.bin") {

}

.timestamp <- function(x)
    attr(x, "timestamp")

`.timestamp<-` <- function(x, value) {
    attr(x, "timestamp") <- value
    x
}

.columns <- function(x)
    attr(x, "columns")

`.columns<-` <- function(x, value) {
    attr(x, "columns") <- value
    x
}

.t.type <- function(x)
    attr(x, "t.type")

`.t.type<-` <- function(x, value) {
    attr(x, "t.type") <- value
    x
}
