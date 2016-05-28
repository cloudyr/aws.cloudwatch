#' @rdname log_streams
#' @title Log streams
#' @description Get, create, and delete log streams
#' @template groupname
#' @template stream
#' @param prefix Optionally, a log stream \emph{prefix} used to restrict the returned results.
#' @param ascending A logical specifying whether earliest events should be returned first. Default is implicitly \code{TRUE}.
#' @param sort Optionally, a character string specifying the sort order of the results. Must be one of: \dQuote{LogStreamName} or \dQuote{LastEventTime}.
#' @template n
#' @template token
#' @template dots
#' @export
create_log_stream <- function(name, stream, ...) {
    query <- list(Action = "CreateLogStream") 
    if (!nchar(name) %in% 1:512) {
        stop("'name' must be 1:512 characters")
    }
    query$logGroupName <- name
    if (!nchar(stream) %in% 1:512) {
        stop("'stream' must be 1:512 characters")
    }
    if (grepl(":", stream, fixed = TRUE)) {
        stop("'stream' must not contain a colon '(:)'")
    }
    query$logStreamName <- stream
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

#' @rdname log_streams
#' @export
delete_log_stream <- function(name, stream, ...) {
    query <- list(Action = "DeleteLogStream") 
    query$logGroupName <- name
    query$logStreamName <- stream
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

#' @rdname log_streams
#' @export
get_log_streams <- function(name, prefix, ascending, sort, n, token, ...) {
    query <- list(Action = "DescribeLogStreams", logGroupName = name)
    if (!missing(prefix)) {
        query$logStreamNamePrefix <- prefix
    }
    if (!missing(n)) {
        query$limit <- n
    }
    if (!missing(token)) {
        query$nextToken <- token
    }
    if (!missing(ascending)) {
        query$descending <- !ascending
    }
    if (!missing(sort)) {
        stopifnot(sort %in% c("LogStreamName", "LastEventTime"))
        query$orderBy <- sort
    }
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

