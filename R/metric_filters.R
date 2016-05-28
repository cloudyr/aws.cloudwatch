#' @rdname metric_filters
#' @title Metric Filters
#' @description Get, create, delete, and test metric filters
#' @template groupname
#' @param filter The name of a metric filter.
#' @param prefix A filter name \emph{prefix} used to restrict the returned results.
#' @template n
#' @template token
#' @param pattern According to the API documentation: \dQuote{A symbolic description of how CloudWatch Logs should interpret the data in each log event. For example, a log event may contain timestamps, IP addresses, strings, and so on. You use the filter pattern to specify what to look for in the log event message.} 
#' @param messages A character vector of event messages.
#' @template dots
#' @export
create_metric_filter <- function() {}

#' @rdname metric_filters
#' @export
delete_metric_filter <- function(name, filter, ...) {
    query <- list(Action = "DeleteMetricFilter") 
    query$logGroupName <- name
    query$filterName <- filter
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

#' @rdname metric_filters
#' @export
get_metric_filters <- function(name, prefix, n, token, ...) {
    query <- list(Action = "DescribeMetricFilters",
                  logGroupName = name)
    if (!missing(n)) {
        query$limit <- n
    }
    if (!missing(prefix)) {
        query$filterNamePrefix <- prefix
    }
    if (!missing(token)) {
        query$nextToken <- token
    }
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

#' @rdname metric_filters
#' @export
test_metric_filter <- function(pattern, messages, ...) {
    query <- list(Action = "TestMetricFilter")
    if (!nchar(pattern) > 512) {
        stop("'pattern' must be <= 512 characters")
    }
    query$filterPattern <- pattern
    if (!length(messages) | length(messages) > 50) {
        stop("'messages' must be a character vector of 1-50 messages")
    }
    query$logEventMessages <- messages
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)

}
