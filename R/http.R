#' @title cloudwatchHTTP
#' @description Execute CloudWatch HTTP Requests
#' @details This is the workhorse function to execute CloudWatch API requests. It should not normally be used directly.
#' @param query A list.
#' @param api A character string specifying which CloudWatch API to call. One of \dQuote{monitoring}, \dQuote{events}, or \dQuote{logs}.
#' @param headers A list of headers to pass to the HTTP request.
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param region A character string containing the AWS region. If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string containing an AWS Secret Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token A character string containing an AWS Session Token. See \code{\link[aws.signature]{locate_credentials}}.
#' @param \dots Additional arguments passed to \code{\link[httr]{GET}}.
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 read_xml as_list
#' @importFrom aws.signature signature_v4_auth
#' @export
cloudwatchHTTP <- 
function(
  query, 
  api = c("monitoring", "events", "logs"),
  headers = list(), 
  verbose = getOption("verbose", FALSE),
  region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"),
  key = NULL,
  secret = NULL,
  session_token = NULL,
  ...
) {
    # locate and validate credentials
    credentials <- locate_credentials(key = key, secret = secret, session_token = session_token, region = region, verbose = verbose)
    key <- credentials[["key"]]
    secret <- credentials[["secret"]]
    session_token <- credentials[["session_token"]]
    region <- credentials[["region"]]
    
    # generate request signature
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    api <- match.arg(api)

    Sig <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = api,
           verb = "GET",
           action = "/",
           query_args = query,
           canonical_headers = list(host = paste0(api, ".amazonaws.com"),
                                    `x-amz-date` = d_timestamp),
           request_body = "",
           key = key, 
           secret = secret,
           session_token = session_token,
           verbose = verbose)
    # setup request headers
    headers[["x-amz-date"]] <- d_timestamp
    headers[["Authorization"]] <- Sig[["SignatureHeader"]]
    headers[["x-amz-content-sha256"]] <- Sig$BodyHash
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)

    # execute request
    r <- GET(paste0("https://", api, ".amazonaws.com"), H, query = query, ...)
    if (http_status(r)$category == "Client error") {
        x <- try(xml2::as_list(xml2::read_xml(content(r, "text", encoding = "UTF-8"))), silent = TRUE)
        if (inherits(x, "try-error")) {
            x <- try(jsonlite::fromJSON(content(r, "text", encoding = "UTF-8"))$Error, silent = TRUE)
        }
        warn_for_status(r)
        h <- headers(r)
        out <- structure(x, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- try(jsonlite::fromJSON(content(r, "text", encoding = "UTF-8")), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text", encoding = "UTF-8"), "unknown")
        }
    }
    return(out)
}
