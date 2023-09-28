library(httr)

rm(list=ls())

base_url <- cbsodataR:::BASE_URL   # TODO: check MLZ
id <- "80590ned"

filters <- readRDS("big_filter.rds")

filters$Perioden <- filters$Perioden[220:221]

print(filters)

#
# met "format" = "json" geef je aan dat de response JSON-encoded moet zijn.
#
arguments <- append(list("format" = "json"), filters)
print (arguments)

url <- whisker::whisker.render("{{BASEURL}}/{{BULK}}/{{id}}/{{DATASET}}"
                               , list( BASEURL = base_url
                                       , BULK    = cbsodataR:::BULK
                                       , id      = id
                                       , DATASET = "TypedDataSet" ))
                                       

# in body stuur je de argumenten mee en met encode = "json" geef je 
# aan dat de argumenten JSON-encoded meegegeven moeten worden.
#
response <- POST(url, body = arguments, encode = "json", verbose())
print(response)
data2 <- jsonlite::fromJSON(content(response, as = "text"))
print(data2)
                                       