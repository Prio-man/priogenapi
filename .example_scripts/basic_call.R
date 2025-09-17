library(priogenapi)

API_KEY <- Sys.getenv("PRILIMS_API_KEY")
df_ <- call_api(API_KEY, "results")
