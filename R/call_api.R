#' Make a call to the PriLIMS API
#'
#' Requires an API key provided by Priogen Corp.
#'
#' @param key An API key used for accessing PriLIMS API
#' @param endpoint Endpoint URL; e.g. "raw" if accessing Raw data table.
#'
#' @import httr2
#' @import jsonlite
#' @import dplyr
#'
#' @export

call_api <- function(key, endpoint) {
  url <- paste0("https://priogen.xyz/labdb/api/", endpoint)
  all_results <- list()

  repeat {
    req <- request(url) |>
      req_headers(Authorization = paste("Token", key))

    resp <- req_perform(req)
    data <- resp_body_json(resp, simplifyVector = TRUE)

    if (!is.null(data$results)) {
      all_results <- append(all_results, list(data$results))
    } else {
      stop("Response did not contain 'results'")
    }

    if (is.null(data$`next`) || data$`next` == "") break
    url <- data$`next`
  }

  return(bind_rows(all_results))
}
