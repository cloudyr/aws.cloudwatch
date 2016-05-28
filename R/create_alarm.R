#' @rdname create_alarms
#' @title Create CloudWatch Alarms
#' @description This currently doesn't work!!
#' @seealso \code{\link{get_alarms}}, \code{\link{delete_alarms}}
#create_alarms <- function(alarm, description, actions, enabled, metric, namespace, dimensions, statistic, threshold, unit, period, nperiods, comparator, ...) {
#    stop("This is not currently implemented")
#    # this is probably better to require a metric object (e.g., from create_metric_filters() or get_metric_filters()...)
#}
