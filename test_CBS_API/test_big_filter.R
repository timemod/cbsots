rm(list = ls())

base_url <- cbsodataR:::BASE_URL   # TODO: check MLZ
id <- "80590ned"

filters <- readRDS("big_filter.rds")

url <- whisker::whisker.render("{{BASEURL}}/{{BULK}}/{{id}}/{{DATASET}}?$format=json"
                               , list( BASEURL = base_url
                                       , BULK    = cbsodataR:::BULK
                                       , id      = id
                                       , DATASET = "TypedDataSet" 
                               ))

arguments <- c(filters, list(select = NULL))
#arguments <- c(list(select = NULL))

url <- paste0(url, do.call(cbsodataR:::get_query, arguments))

url <- utils::URLencode(url)

#cat("url:\n")  
#print(url)

print(nchar(url))


lines <- readLines(url, warn = FALSE)
