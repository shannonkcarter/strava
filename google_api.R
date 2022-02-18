library(googleway)

key <- 'AIzaSyDRqBkQVT6xFn7XP2abbfOBoKEEHxhmZC4'

df_places <- google_places(search_string = "ice cream", 
                           location = c( -95.4, 29.7),   ## melbourne, AU
                           key = key)
