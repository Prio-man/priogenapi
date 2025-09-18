#' Make a call to the PriLIMS API
#'
#' Requires an API key provided by Priogen Corp.
#'
#' @param key An API key used for accessing PriLIMS API
#' @param endpoint Endpoint URL; e.g. "results" if accessing Results data table.
#' @param order Character; the order number
#' @param sample Character; the sample name
#' @param reaction Character; the reaction name
#'
#' @import httr2
#' @import jsonlite
#' @import dplyr
#'
#' @export

call_api <- function(key, endpoint, order=NA, sample=NA, reaction=NA) {

  # Disincentivize user from querying the entire table.
  if (is.na(order) & is.na(sample) & is.na(reaction)) {
    cat("\n\n")
    answer <- "You are about to query the entire %s table. Do you wish to proceed? [Y/n]: " %>%
      sprintf(endpoint) %>%
      readline()
    if (tolower(answer) != "y") stop("API call stopped by user")
  }

  url <- paste0("https://priogen.xyz/labdb/api/", endpoint)
  all_results <- list()

  repeat {
    req <- request(url) %>%
      req_headers(Authorization = paste("Token", key))

    resp <- req %>%
      {if (!is.na(order)) req_url_query(.data, order=order) else .data} %>%
      {if (!is.na(sample)) req_url_query(.data, sample=sample) else .data} %>%
      {if (!is.na(reaction)) req_url_query(.data, reaction=reaction) else .data} %>%
      req_perform()

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
