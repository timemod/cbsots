url <- "http://opendata.cbs.nl/ODataFeed/odata/80590ned/TypedDataSet"
query <- list(`$format` = "json",
              `$filter` = "(Perioden eq '2021MM04') and (Geslacht eq 'T001038') and (Leeftijd eq '52052   ')")
library(httr)

response_get <- GET(url, query = query)
print(response_get)
data <- content(response_get, as = "text", encoding = "UTF-8")
data <- jsonlite::fromJSON(data)

print(unique(data$value$Perioden))
print(unique(data$value$Geslacht))
print(unique(data$value$Leeftijd))

# nu met POST?

url_post <- "http://opendata.cbs.nl/ODataFeed/odata/$batch"
url_get <- "https://opendata.cbs.nl/ODataFeed/odata/80590ned/TypedDataSet?$format=json&$filter=(Perioden eq '2021MM04') and (Geslacht eq 'T001038') and (Leeftijd eq '52052   ')"
url_get <-  utils::URLencode(url_get)
body <- 
'{
  "requests": [
    {
      "id": "1",
      "method": "GET",
      "url":, "_GET_URL_"
    },
  ]
}'

body <- sub("_GET_URL_", url_get, body)
cat(body)

response2 <- POST(url_post, body = body, encode = "json")
print(response2)
