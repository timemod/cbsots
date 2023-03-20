rm(list = ls())

base_url <- cbsodataR:::BASE_URL   # TODO: check MLZ
id <- "80590ned"

filters <- readRDS("big_filter.rds")
writeLines(filters$Perioden, "periode_filter.txt")

#filters$Perioden <- filters$Perioden[220:221]

url_basis <- whisker::whisker.render("{{BASEURL}}/{{BULK}}/{{id}}/{{DATASET}}?$format=json"
                               , list( BASEURL = base_url
                                       , BULK    = cbsodataR:::BULK
                                       , id      = id
                                       , DATASET = "TypedDataSet" 
                               ))

print(url)

arguments <- c(filters, list(select = NULL))

filter <- do.call(cbsodataR:::get_query, arguments)
filter <- sub("^&\\$filter=", "", filter)

writeLines(filter, "filter_small.txt")

#
# method 1:
#
url <- paste0(url_basis, "&$filter=", filter)

#url <- utils::URLencode(url)
#lines <- readLines(url, warn = FALSE)

# data1 <- jsonlite::read_json(url, simplifyVector = TRUE)
# print(data1)

# Now try httrp package
# 

library(httr2)

response1 <- request(url)  %>%
  req_perform

con <- url(url, method = "libcurl")
lines <- readLines(con, con)

data2 <- content(response1, as = "text", encoding = "UTF-8")
data2 <- jsonlite::fromJSON(data2)

print(all.equal(data1, data2))

# POST method (gaat niet goed)
url <- "http://opendata.cbs.nl/ODataFeed/odata/80590ned/TypedDataSet"
query <- list("$format" = "json", "$filter" = filter)
response_post <- POST(url, body = query, encode = "form", verbose())
data3 <- content(response_post, as = "text")
data3 <- jsonlite::fromJSON(data3)
print(data3)
print(all.equal(data, data2))

