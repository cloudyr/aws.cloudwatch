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
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

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
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}


put_metric_data <- function() {}

put_alarm <- function() {}

#' @export
delete_alarms <- function(alarm, ...) {
    query <- list(Action = "DeleteAlarms")
    if (length(alarm) > 100) {
        stop("'alarm' must be within length 1:100")
    }
    a <- as.list(alarm)
    names(a) <- paste0("AlarmNames.member.",1:length(a))
    query <- c(query, a)
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

#' @export
alarm_history <- function(alarm, n, start, end, type, token, ...) {
    query <- list(Action = "DescribeAlarmHistory")
    if (!missing(alarm)) {
        query$AlarmName <- alarm
    }
    if (!missing(n)) {
        query$Limit <- n
    }
    if (!missing(start)) {
        # need to format correctly
        query$StartTime <- start
    }
    if (!missing(end)) {
        # need to format correctly
        query$EndTime <- end
    }
    if (!missing(type)) {
        vtypes <- c("ConfigurationUpdate", "StateUpdate", "Action")
        if(!type %in% vtypes)
            stop("'type' must be one of: ", paste0(vtypes, collapse = ", "))
        query$HistoryItemType <- type
    }
    if (!missing(token)) {
        query$NextToken <- token
    }
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}

get_alarms <- function() {
    query <- list(Action = "DescribeAlarms")
    
    # also ForMetric
    # query <- list(Action = "DescribeAlarmsForMetric")
    
}


#' @export
temp_alarm_state <- function(alarm, reason, state, ...) {
    query <- list(Action = "SetAlarmState")
    if (nchar(alarm) > 255) {
        stop("'alarm' must be between 1:255 characters")
    }
    if (nchar(reason) > 1023) {
        stop("'alarm' must be between 0:1023 characters")
    }
    query$StateReason <- reason
    # StateReasonData is json formatted reason, optional
    vstates <- c("OK", "ALARM", "INSUFFICIENT_DATA")
    if (!state %in% vstates) {
        stop("'state' must be one of: ", paste0(vstates, collapse = ", "))
    }
    query$StateValue <- state
    r <- cloudwatchHTTP(query = query, ...)
    return(r)
}


list_metrics <- function() {
}

get_metric_stats <- function() {
}
