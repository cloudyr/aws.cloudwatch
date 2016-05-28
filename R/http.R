#' @title cloudwatchHTTP
#' @description Execute CloudWatch HTTP Requests
#' @details This is the workhorse function to execute CloudWatch API requests. It should not normally be used directly.
#' @param query A list.
#' @param body The body of the request.
#' @param region A character string containing the AWS region.
#' If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. 
#' If missing, defaults to value stored in environment variable \dQuote{AWS_ACCESS_KEY_ID}.
#' @param secret A character string containing an AWS Secret Access Key. 
#' If missing, defaults to value stored in environment variable \dQuote{AWS_SECRET_ACCESS_KEY}.
#' @param \dots Additional arguments passed to \code{\link[httr]{GET}}.
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom XML xmlParse xmlToList
#' @importFrom aws.signature signature_v4_auth
#' @export
cloudwatchHTTP <- function(query, 
                           api = c("monitoring", "events", "logs"),
                           region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
                           key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                           secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                           ...) {
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    api <- match.arg(api)
    if (key == "") {
        H <- add_headers(`x-amz-date` = d_timestamp)
    } else {
        S <- signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "monitoring",
               verb = "GET",
               action = "/",
               query_args = query,
               canonical_headers = list(host = paste0(api, ".amazonaws.com"),
                                        `x-amz-date` = d_timestamp),
               request_body = "",
               key = key, secret = secret)
        H <- add_headers(`x-amz-date` = d_timestamp, 
                         `x-amz-content-sha256` = S$BodyHash,
                         Authorization = S$SignatureHeader)
    }
    r <- GET(paste0("https://", api, ".amazonaws.com"), H, query = query, ...)
    if (http_status(r)$category == "client error") {
        x <- try(xmlToList(xmlParse(content(r, "text"))), silent = TRUE)
        if (inherits(x, "try-error")) {
            x <- try(fromJSON(content(r, "text"))$Error, silent = TRUE)
        }
        warn_for_status(r)
        h <- headers(r)
        out <- structure(x, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- S$CanonicalRequest
        attr(out, "request_string_to_sign") <- S$StringToSign
        attr(out, "request_signature") <- S$SignatureHeader
    } else {
        out <- try(fromJSON(content(r, "text")), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text"), "unknown")
        }
    }
    return(out)
}
