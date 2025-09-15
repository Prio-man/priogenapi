library(httr2)
library(jsonlite)
library(tidyr)



API_KEY <- Sys.getenv("PRILIMS_API_KEY")
BASE_URL <- Sys.getenv("PRILIMS_BASE_URL")

get_all_pages <- function(endpoint) {
  url <- paste0(BASE_URL, endpoint)
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

# raw_data <- get_all_pages("raw/")
results <- get_all_pages("results/")
