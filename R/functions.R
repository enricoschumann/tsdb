## The package uses code from Enrico Schumann's
## R package 'database'.
## Copyright Enrico Schumann 2010-2017

## ---------------- time 

ttime <- function(x, from = "datetime", to = "numeric",
                  tz = "", strip.attr = FALSE,
                  format = "%Y-%m-%d") {
    
    if (from == "datetime" && to == "numeric") {
        if (strip.attr)
            c(unclass(x)) else unclass(x)
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


write_ts_table <- function(x, dir, file,
                           add = FALSE, overwrite = FALSE,
                           backend = "csv") {
    if (!(inherits(x, "ts_table")))
        stop(sQuote("x"), " must be a ts_table")

    save.scipen <- options(scipen = 1e5)
    on.exit(options(scipen = save.scipen))
    timestamp <- ttime(.timestamp(x))
    columns   <- .columns(x)

    backend <- tolower(backend)
    ans <- dim(x)[1L] ## a ts_table is always a matrix
    if (backend == "csv") {
        dfile <- if (missing(dir))
                     file
                 else
                     file.path(dir, file)
        if (ans == 0L) {
            if (!file.exists(dfile))
                write.table(as.matrix(data.frame(timestamp, unclass(x))),
                            file = dfile,
                            row.names = FALSE,
                            col.names = c("timestamp", columns),
                            sep = ",")
            return(invisible(0L))
        }
        if (add) {
            in_db <- read_ts_tables(file, dir, drop.weekends = FALSE)
            if (any(in_db$columns != columns))
                stop("columns in file differ from columns in ", sQuote("x"))
            new <- !timestamp %in% ttime(in_db$timestamp)
            ans <- 0
            if (any(new)) {
                ans <- sum(new)
                timestamp <- c(ttime(in_db$timestamp),
                               timestamp[new])
                x <- rbind(in_db$data, x[new, ,drop = FALSE])
                if (is.unsorted(timestamp)) {
                    ii <- order(timestamp)
                    timestamp <- timestamp[ii]
                    x <- x[ii, , drop = FALSE]
                }
            }
        }
        if (file.exists(dfile) && !overwrite && !add) {
            ans  <- 0
            message("file exists; use ", sQuote("add = TRUE"),
                    " or ", sQuote("overwrite = TRUE"),
                    " to change/overwrite file")
        } else if (add && sum(new) == 0L) {
            ans  <- 0
        } else {
            write.table(as.matrix(data.frame(timestamp, unclass(x))),
                        file = dfile,
                        row.names = FALSE,
                        col.names = c("timestamp", columns),
                        sep = ",")
        }
    } else if (backend == "monetdb") {

        if (!inherits(dir, "MonetDBEmbeddedConnection")) {
            dir <- dbConnect(MonetDBLite::MonetDBLite(), dir)
            on.exit(dbDisconnect(dir, shutdown = TRUE))
        }
        
        df <- data.frame(timestamp, unclass(x))
        colnames(df) <- c("timestamp", columns)
        dbWriteTable(dir, dbQuoteIdentifier(dir, file), df,
                     overwrite = overwrite)
        
    } else
        stop("unknown backend")
    invisible(ans)
}

read_ts_tables <- function(file, dir, t.type = "guess",
                           start, end, columns,
                           return.class = NULL,
                           drop.weekends = TRUE,
                           column.name = "%dir%/%file%::%column%",
                           backend = "csv") {

    backend <- tolower(backend)

    
    if (backend == "csv") {
    ### ****************

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

        if (t.type == "guess" || missing(columns))
            samp <- readLines(dfile[[1]], n = 2L)
        if (t.type == "guess") {
            if (length(samp) == 2L) {
                tmp <- as.numeric(
                    strsplit(samp[[2]], ",", fixed = TRUE)[[1L]][[1L]])
                t.type <- if (tmp < 43200) ## 86400/2
                              "Date" else "POSIXct"
            } else
                t.type <- "Date"
        }
        if (missing(columns)) {
            tmp <- gsub("\"", "",
                        strsplit(samp[[1]], ",", fixed = TRUE)[[1]])
            columns <- tmp[-1L]
        }
        
        if (t.type == "Date") {
            start <- if (missing(start))
                         as.Date("1970-01-01")
                     else
                         as.Date(start)
            
            end   <- if (missing(end))
                         previous_businessday(Sys.Date())
                     else
                         as.Date(end)
            
            timestamp <- seq(start, end , "1 day")
            if (drop.weekends)
                timestamp <- timestamp[is_businessday(timestamp)]
        } else if (t.type == "POSIXct") {
            warning("'Oh boy', said Helen, 'that's not really supported.'")
            start <- if (missing(start))
                         as.POSIXct(Sys.Date() - 365)
                     else
                         as.POSIXct(start)
            
            if (missing(end))
                end <- as.POSIXct(previous_businessday(Sys.Date()))
            else
                end <- as.POSIXct(end)
            timestamp <- seq(start, end , "1 sec")
        } else
            stop("unknown ", sQuote("t.type"))
        
        nc <- length(columns)
        results <- array(NA_real_,
                         dim = c(length(timestamp), length(dfile)*nc))
        for (i in seq_along(dfile)) {
            tmp <- read.table(dfile[[i]], sep = ",",
                              stringsAsFactors = FALSE,                           
                              header = TRUE, colClasses = "numeric")
            ii <- fmatch(tmp[[1L]], timestamp, nomatch = 0L)
            tmp.names <- colnames(tmp)
            if (!all(columns %in% tmp.names)) {
                warning("columns missing")
                tmp <- cbind(tmp, rep(NA, sum(!(columns %in% tmp.names))))
                colnames(tmp) <- c(tmp.names, columns[!(columns %in% tmp.names)])            
            }
            res <- tmp[ , columns, drop = FALSE][ii > 0L, ]
            if (!is.null(res))
                results[ii, (nc*(i-1)+1):(nc*i)] <- as.matrix(res)
        }
        rm <- rowSums(is.na(results)) == dim(results)[[2L]]
        results <- results[!rm, ,drop = FALSE]
        timestamp <- timestamp[!rm]
        colnames <- rep.int(column.name, dim(results)[[2L]])
        .dir <- rep(dir, each = length(columns))
        .file <- rep(file, each = length(columns))
        .columns <- rep(columns, length(.dir)/length(columns))
        for (i in seq_along(.dir)) {
            colnames[[i]] <- gsub("%dir%",    .dir[[i]],     colnames[[i]])
            colnames[[i]] <- gsub("%file%",   .file[[i]],    colnames[[i]])
            colnames[[i]] <- gsub("%column%", .columns[[i]], colnames[[i]])
        }
        
    } else if (backend == "monetdb") {
           ### ********************

        if (!inherits(dir, "MonetDBEmbeddedConnection")) {
            dir <- dbConnect(MonetDBLite::MonetDBLite(), dir)
            on.exit(dbDisconnect(dir, shutdown = TRUE))
        }
        
        dbGetQuery(dir, "SELECT * FROM file;")
        
    } else
        stop("unknown backend")

    if (is.null(return.class)) {
        list(data      = results,
             timestamp = timestamp,
             columns = rep(columns, each = length(dfile)),
             file.path = paste(rep(dfile, each = length(columns)), columns, sep = "::"))
    } else if (return.class == "zoo") {
        if (!requireNamespace("zoo"))
            stop("package ", sQuote("zoo"), " not available")
        if (!is.null(dim(results)))
            colnames(results) <- colnames
        zoo(results, timestamp)
    } else if (return.class == "data.frame") {
        ans <- data.frame(timestamp, results)
        colnames(ans) <- c("timestamp", colnames)
        ans
    } else {
        stop("unknown ", sQuote("return.class"))
    }        
}

## x <- scan("~/tsdb/daily/cmcier", what= list(numeric(0), numeric(0)),
##      skip = 1, sep = ",", multi.line=FALSE)
## read.table("~/tsdb/daily/cmcier", colClasses = "numeric", header = TRUE)


dir_info <- function(dir = getwd()) {
    res <- dir()
    class(res) <- "dir_info"
}

print.dir_info <- function(x, ...) {
    print(unclass(x))
}


file_info <- function(dir, file) {
    dfile <- if (missing(dir))
                 file
             else
                 file.path(dir, file)

    nf <- length(dfile)
    res <- data.frame(dir_file = dfile,
                      columns = character(nf),
                      min_timestamp = numeric(nf),
                      max_timestamp = numeric(nf),
                      stringsAsFactors = FALSE)
                      
    for (i in seq_len(nf)) {
        fi <- try(read_ts_tables(dfile[i]), silent = TRUE)
        res[["min_timestamp"]][i] <- suppressWarnings(min(fi$timestamp))
        res[["max_timestamp"]][i] <- suppressWarnings(max(fi$timestamp))
    }
    res[["min_timestamp"]][!is.finite(res[["min_timestamp"]])] <- NA
    res[["max_timestamp"]][!is.finite(res[["max_timestamp"]])] <- NA
    class(res) <- c("file_info", "data.frame")
    res
}

print.file_info <- function(x, ...) {
    print(x, ...)
}

update_ts_table <- function(x, timestamp, type, file)
    .NotYetImplemented()

ts_table <- function(data, timestamp, columns) {
    if (!inherits(timestamp, "Date") &&
        !inherits(timestamp, "POSIXt"))
        stop(sQuote("timestamp"), " must be Date or POSIXt")
    ## TODO if character, match regexp and then coerce
    ##      to Date/POSIXct?
    if (inherits(timestamp, "POSIXlt")) {
        timestamp <- c(as.POSIXct(timestamp))
        t.type <- "POSIXct"
    }
    if (inherits(timestamp, "POSIXct")) {
        timestamp <- c(timestamp)
        t.type <- "POSIXct"
    }
    if (inherits(timestamp, "Date")) {
        t.type <- "Date"
    }
    ans <- as.matrix(data)
    if (missing(columns))
        columns <- colnames(ans)
    ans <- unname(ans)
    if (is.null(columns))
        stop("no column names, and ", sQuote("columns"), " not provided")
    if (ncol(ans) != length(columns))
        stop("more columns than column names")
    if (is.unsorted(timestamp)) {
        ii <- order(timestamp)
        timestamp <- timestamp[ii]
        ans <- ans[ii, ,drop = FALSE]
    }
    attr(ans, "timestamp") <- timestamp
    attr(ans, "t.type") <- t.type
    attr(ans, "columns") <- columns
    class(ans) <- "ts_table"
    ans
}

as.ts_table <- function(x, ...) {
    UseMethod("as.ts_table")
}

as.ts_table.zoo <- function(x, columns, ...) {
    cols <- if (missing(columns))
                colnames(x)
            else columns
    if (is.null(cols))
        stop("no column names, and ", sQuote("columns"), " not provided")
    ts_table(unname(coredata(x)), index(x), cols)
}

as.zoo.ts_table <- function(x, ...) {
    ans <- zoo(unname(as.matrix(x)), .timestamp(x))
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
                         data.frame(unclass(x), stringsAsFactors = FALSE))
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
        rownames(ans) <- as.character(ttime(timestamp, from = "numeric", to = .t.type(x)))
        ans
}

print.ts_table <- function(x, ...) {
    tmp <- .timestamp(x)
    from_to <- if (length(tmp))
                   range(tmp)
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

