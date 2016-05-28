#' @rdname log_events
#' @title Log events
#' @description Get and create log events
#' @template groupname
#' @template stream
#' @param events This is a list of two-element lists, where each list contains a character string named \code{message} and a unix epoch time in milliseconds (e.g., \code{list(message = "hello world!", timestamp = 1)}).
#' @param start Unix epoch time in milliseconds.
#' @param end Unix epoch time in milliseconds.
#' @param ascending A logical specifying whether earliest events should be returned first. Default is implicitly \code{FALSE}.
#' @template n
#' @template token
#' @template dots
#' @details \code{get_log_events} retrieves event data. \code{create_log_events} adds event data to the log.
#' @export
create_log_events <- 
function(name, stream, events, token, ...) {
    query <- list(Action = "PutLogEvents",
                  logGroupName = name,
                  logStreamName = stream)
    query$logEvents <- events
    if (!missing(token)) {
        query$sequenceToken <- token
    }
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

#' @rdname log_events
#' @export
get_log_events <- 
function(name, stream, start, end, ascending, n, token, ...) {
    query <- list(Action = "GetLogEvents",
                  logGroupName = name,
                  logStreamName = stream)
    if (!missing(n)) {
        query$limit <- n
    }
    if (!missing(start)) {
        # need to format correctly
        query$startTime <- start
    }
    if (!missing(end)) {
        # need to format correctly
        query$endTime <- end
    }
    if (!missing(ascending)) {
        query$startFromHead <- tolower(as.character(ascending))
    }
    if (!missing(token)) {
        query$nextToken <- token
    }
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)    
}

