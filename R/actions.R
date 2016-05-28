#' @rdname actions
#' @title Alarm Actions
#' @description Enable/disable alarm actions
#' @template alarm
#' @template dots
#' @export
enable_actions <- function(alarm, ...) {
    query <- list(Action = "EnableAlarmActions")
    if (length(alarm) > 100) {
        stop("'alarm' must be within length 1:100")
    }
    if (any(nchar(alarm) > 255)) {
        stop("'alarm' names must be max 255 characters")
    }
    a <- as.list(alarm)
    names(a) <- paste0("AlarmNames.member.",1:length(a))
    query <- c(query, a)
    r <- cloudwatchHTTP(query = query, api = "monitoring", ...)
    return(r)
}

#' @rdname actions
#' @export
disable_actions <- function(alarm, ...) {
    query <- list(Action = "DisableAlarmActions")
    if (length(alarm) > 100) {
        stop("'alarm' must be within length 1:100")
    }
    if (any(nchar(alarm) > 255)) {
        stop("'alarm' names must be max 255 characters")
    }
    a <- as.list(alarm)
    names(a) <- paste0("AlarmNames.member.",1:length(a))
    query <- c(query, a)
    r <- cloudwatchHTTP(query = query, api = "monitoring", ...)
    return(r)
}
