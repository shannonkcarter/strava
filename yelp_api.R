library(tidyverse)
library(httr)

# https://billpetti.github.io/2017-12-23-use-yelp-api-r-rstats/

res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = credentials$client_id_yelp,
                        client_secret = credentials$secret_yelp))

token <- content(res)$access_token
token

yelp_business_search <- function(term = NULL, location = NULL, 
                                 categories = NULL, radius = NULL, 
                                 limit = 50, client_id = NULL, 
                                 client_secret = NULL) {
  yelp <- "https://api.yelp.com"
  url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                    query = list(term = term, location = location, limit = limit, 
                                 radius = radius, categories = categories))
  res <- GET(url, add_headers('Authorization' = paste("bearer", token)))
  results <- content(res)
  
  yelp_httr_parse <- function(x) {
    
    parse_list <- list(id = x$id, 
                       name = x$name, 
                       rating = x$rating, 
                       review_count = x$review_count, 
                       latitude = x$coordinates$latitude, 
                       longitude = x$coordinates$longitude, 
                       address1 = x$location$address1, 
                       city = x$location$city, 
                       state = x$location$state, 
                       distance = x$distance)
    
    parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
    
    df <- data_frame(id = parse_list$id,
                     name = parse_list$name, 
                     rating = parse_list$rating, 
                     review_count = parse_list$review_count, 
                     latitude = parse_list$latitude, 
                     longitude = parse_list$longitude, 
                     address1 = parse_list$address1, 
                     city = parse_list$city, 
                     state = parse_list$state, 
                     distance = parse_list$distance)
    df
  }
  results_list <- lapply(results$businesses, FUN = yelp_httr_parse)
  payload <- do.call("rbind", results_list)
  payload <- payload %>%
    filter(grepl(term, name))
  
  payload
}

results <- yelp_business_search(term = "Dunkin' Donuts", 
                                location = "Philadelphia, PA",
                                radius = 1000, # in meters; 40000m = 25mi is max 
                                client_id = client_id, 
                                client_secret = client_secret)
