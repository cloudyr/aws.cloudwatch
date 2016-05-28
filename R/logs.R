#' @rdname log_groups
#' @title Cloudwatch log groups
#' @description Create, retrieve, and delete log groups
#' @template groupname
#' @param prefix Optionally, a log stream \emph{prefix} used to restrict the returned results.
#' @template n
#' @template token
#' @template dots
#' @export
create_log_group <- function(name, ...) {
    query <- list(Action = "CreateLogGroup")    
    if (!nchar(name) %in% 1:512) {
        stop("'name' must be 1:512 characters")
    }
    query$logGroupName <- name
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

#' @rdname log_groups
#' @export
delete_log_group <- function(name, ...) {
    query <- list(Action = "DeleteLogGroup",
                  logGroupName = name)
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

#' @rdname log_groups
#' @export
get_log_groups <- function(prefix, n, token, ...) {
    query <- list(Action = "DescribeLogGroups")
    if (!missing(n)) {
        query$limit <- n
    }
    if (!missing(prefix)) {
        query$logGroupNamePrefix <- prefix
    }
    if (!missing(token)) {
        query$nextToken <- token
    }
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}
