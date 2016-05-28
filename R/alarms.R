#' @rdname alarms
#' @title CloudWatch Alarms
#' @description Get CloudWatch alarms and alarm information; delete alarms
#' @template alarm
#' @param prefix Rather than specify \code{alarm}, it is possible to specify the \emph{prefix} of a set of alarm names using this parameter.
#' @param reason A charater string specifying the alarm is being temporarily set to this state.
#' @param state An optional character string specifying an alarm state to be used when matching alarms. One of: \dQuote{OK}, \dQuote{ALARM}, or \dQuote{INSUFFICIENT_DATA}.
#' @param type Optionally, a character string specifying the type of alarm history to retrieve. One of: \dQuote{ConfigurationUpdate}, \dQuote{StateUpdate}, or \dQuote{Action}.
#' @param start Optionally, the start date of alarm history to retrieve.
#' @param end Optionally, the end date of alarm history to retrieve.
#' @template n
#' @template token
#' @template dots
#' @details These functions delete, modify, and retrieve details about CloudWatch alarms. \code{\link{create_alarms}} creates a new alarm. \code{delete_arms} deletes one or more alarms. \code{get_alarms} can retrieve a specific alarm or list available alarms, and \code{get_alarm_history} retrieve's an alarms history. \code{temp_alarm_state} temporarily changes an alarm's status, which can be useful for testing purposes.
#' @seealso \code{\link{enable_actions}}
#' @export
delete_alarms <- function(alarm, ...) {
    query <- list(Action = "DeleteAlarms")
    if (length(alarm) > 100) {
        stop("'alarm' must be within length 1:100")
    }
    a <- as.list(alarm)
    names(a) <- paste0("AlarmNames.member.",1:length(a))
    query <- c(query, a)
    r <- cloudwatchHTTP(query = query, api = "monitoring", ...)
    return(r)
}

#' @rdname alarms
#' @export
get_alarms <- function(alarm, prefix, state, n, token, ...) {
    query <- list(Action = "DescribeAlarms")
    
    if (!missing(alarm)) {
        if (length(alarm) > 100) {
            stop("'alarm' must be within length 1:100")
        }
        a <- as.list(alarm)
        names(a) <- paste0("AlarmNames.member.",1:length(a))
        query <- c(query, a)
    } else if (!missing(prefix)) {
        query$AlarmNamePrefix <- prefix
    }
    if (!missing(n)) {
        if (!n %in% 1:100) {
            stop("'n' must be between 1 and 100")
        }
        query$MaxRecords <- n
    }
    if (!missing(token)) {
        query$NextToken <- token
    }
    if (!missing(state)) {
        statevals <- c("OK", "ALARM", "INSUFFICIENT_DATA")
        if (state %in% statevals) {
            stop(paste0("'state' must be one of: ", paste0(statevals, collapse = ", ")))
        }
        query$StateValue <- state
    }
    r <- cloudwatchHTTP(query = query, api = "monitoring", ...)
    return(r)
    
    # also ForMetric
    # query <- list(Action = "DescribeAlarmsForMetric")
    
}

#' @rdname alarms
#' @export
get_alarm_history <- function(alarm, type, start, end, n, token, ...) {
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
    r <- cloudwatchHTTP(query = query, api = "monitoring", ...)
    return(r)
}

#' @rdname alarms
#' @export
temp_alarm_state <- function(alarm, reason, state, ...) {
    query <- list(Action = "SetAlarmState")
    if (nchar(alarm) > 255) {
        stop("'alarm' must be between 1:255 characters")
    }
    if (nchar(reason) > 1023) {
        stop("'reason' must be between 0:1023 characters")
    }
    query$StateReason <- reason
    # StateReasonData is json formatted reason, optional
    vstates <- c("OK", "ALARM", "INSUFFICIENT_DATA")
    if (!state %in% vstates) {
        stop("'state' must be one of: ", paste0(vstates, collapse = ", "))
    }
    query$StateValue <- state
    r <- cloudwatchHTTP(query = query, api = "monitoring", ...)
    return(r)
}
