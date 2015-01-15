create_log_group <- function(name, ...) {
    query <- list(Action = "CreateLogGroup")    
    if(!nchar(name) %in% 1:512)
        stop("'name' must be 1:512 characters")
    query$LogGroupName <- name
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

delete_log_group <- function(name, ...) {
    query <- list(Action = "DeleteLogGroup",
                  LogGroupName = name)
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

get_log_groups <- function(n, prefix, token, ...) {
    query <- list(Action = "DescribeLogGroups")
    if(!missing(n))
        query$Limit <- n
    if(!missing(prefix))
        query$LogGroupNamePrefix <- prefix
    if(!missing(token))
        query$NextToken <- token
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

create_log_stream <- function(name, stream, ...) {
    query <- list(Action = "CreateLogStream") 
    if(!nchar(name) %in% 1:512)
        stop("'name' must be 1:512 characters")
    query$LogGroupName <- name
    if(!nchar(stream) %in% 1:512)
        stop("'stream' must be 1:512 characters")
    if(grepl(":", stream, fixed = TRUE))
        stop("'stream' must not contain a colon '(:)'")
    query$LogStreamName <- stream
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

delete_log_stream <- function(name, stream, ...) {
    query <- list(Action = "DeleteLogStream") 
    query$LogGroupName <- name
    query$LogStreamName <- stream
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

get_log_streams <- function(name, n, prefix, token, ...) {
    query <- list(Action = "DescribeLogStreams",
                  LogGroupName = name)
    if(!missing(n))
        query$Limit <- n
    if(!missing(prefix))
        query$LogStreamNamePrefix <- prefix
    if(!missing(token))
        query$NextToken <- token
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}


put_metric_filter <- function() {}

delete_metric_filter <- function(name, filter, ...) {
    query <- list(Action = "DeleteMetricFilter") 
    query$LogGroupName <- name
    query$FilterName <- filter
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

get_metric_filters <- function(name, n, prefix, token, ...) {
    query <- list(Action = "DescribeMetricFilters",
                  LogGroupName = name)
    if(!missing(n))
        query$Limit <- n
    if(!missing(prefix))
        query$FilterNamePrefix <- prefix
    if(!missing(token))
        query$NextToken <- token
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

test_metric_filter <- function(pattern, messages, ...) {
    query <- list(Action = "TestMetricFilter")
    if(!nchar(pattern) > 512)
        stop("'pattern' must be <= 512 characters")
    query$FilterPattern <- pattern
    if(!length(messages) | length(messages) > 50)
        stop("'messages' must be a character vector of 1-50 messages")
    query$LogEventMessages <- messages
    r <- cloudwatchHTTP(query = query, ...)
    return(r)

}

put_retention <- function(name, days, ...) {
    query <- list(Action = "PutRetentionPolicy",
                  LogGroupName = name)
    v <- c(1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180, 365, 400, 545, 731, 1827, 3653)
    if(!days %in% v)
        stop("'days' must be one of: ", paste0(v, collapse = ","))
    query$RetentionInDays <- days
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

delete_retention <- function(name, ...) {
    query <- list(Action = "DeleteRetentionPolicy",
                  LogGroupName = name)
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}


put_log_events <- 
function(name, stream, events, token, ...) {
    query <- list(Action = "DescribeMetricFilters",
                  LogGroupName = name,
                  LogStreamName = stream)
    query$LogEvents <- events
    if(!missing(token))
        query$StreamToken <- token
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

get_log_events <- 
function(name, stream, n, start, end,
         ascending, ...) {
    query <- list(Action = "GetLogEvents",
                  LogGroupName = name,
                  LogStreamName = stream)
    if(!missing(n))
        query$Limit <- n
    if(!missing(start)) {
        # need to format correctly
        query$StartTime <- start
    }
    if(!missing(end)) {
        # need to format correctly
        query$EndTime <- end
    }
    if(!missing(ascending))
        query$StartFromHead <- tolower(as.character(ascending))
    if(!missing(token))
        query$NextToken <- token
    r <- cloudwatchHTTP(query = query, ...)
    return(r)    
}

