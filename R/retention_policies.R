#' @rdname retention_policies
#' @title Retention policies
#' @description Create and delete retention policies
#' @template groupname
#' @param days An integer specifying a number of days to retain logs for. There are a finite allowed set of values (see API documentation).
#' @template dots
#' @export
create_retention <- function(name, days, ...) {
    query <- list(Action = "PutRetentionPolicy",
                  logGroupName = name)
    v <- c(1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180, 365, 400, 545, 731, 1827, 3653)
    if (!days %in% v) {
        stop("'days' must be one of: ", paste0(v, collapse = ","))
    }
    query$retentionInDays <- days
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

#' @rdname retention_policies
#' @export
delete_retention <- function(name, ...) {
    query <- list(Action = "DeleteRetentionPolicy",
                  logGroupName = name)
    r <- cloudwatchHTTP(query = query, api = "logs", ...)
    return(r)
}

