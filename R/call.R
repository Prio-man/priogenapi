#' Make an API call to the PriLIMS database.
#' 
#' Requires an API key and an endpoint URL for the data type being accessed.
#' 
#' @param key An API key provided by Priogen.
#' @param endpoint The endpoint URL. E.g. if accessing the Raw table, then use "raw".
#' 
#' @return A data frame object of the API response.
#' 
#' @import httr2
#' @import jsonlite
#' @import dplyr
#' 
#' @export

API_KEY <- Sys.getenv("PRILIMS_API_KEY")
BASE_URL <- Sys.getenv("PRILIMS_BASE_URL")

call_api <- function(key, endpoint) {
  url <- paste0("https://priogen.xyz/labdb/api/", endpoint)
  all_results <- list()
  
  repeat {
    req <- request(url) |>
      req_headers(Authorization = paste("Token", API_KEY))
    
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
