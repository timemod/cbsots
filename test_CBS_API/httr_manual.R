rm(list = ls())

library(httr)

r <- GET("http://httpbin.org/get")

status_code(r)
http_status(r)

headers(r)
content(r)

parse_result <- parse_json(r)

# with query:
r <- GET("http://httpbin.org/get", 
         query = list(key9 = "value1", key2 = "value2"))

url <- "http://httpbin.org/post"
body <- list(a = 1, b = 2, c = 3)

# Form encoded
r <- POST(url, body = body, encode = "form")

r2 <- GET(url, query = body)
