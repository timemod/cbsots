#' @importFrom cbsodataR get_meta
#' @import data.table 
create_new_table <- function(id, base_url = NULL){
  
  if (is.null(base_url)) {
    info <- get_meta(id = id, cache = TRUE)
  } else {
    info <- get_meta(id = id, cache = TRUE, base_url = base_url)
  }
  
  data_properties <- as.data.table(info$DataProperties)
  dimensies_en_topics <- get_dimensies_en_topics(data_properties)
  dimensies <- dimensies_en_topics$dimensies
  topics <- dimensies_en_topics$topics
  topics$Code <- ""
  topics$Select <- FALSE
  # TODO: handle description
  topics$Description <- ""
  topics$OrigKeyOrder <- topics$Key
  topics <- topics[, c("Key", "Select", "Code", "Title",  "OrigKeyOrder")]
  
  codes <- list(Topic = topics)
  
  create_dimcode <- function(dimensie) {
    ret <- info[[dimensie]]
    ret$Code <- ""
    ret$Select <- FALSE
    ret$OrigKeyOrder <- ret$Key
    ret <- ret[, c("Key", "Select", "Code", "Title",  "OrigKeyOrder")]
    return(as.data.table(ret))
  }
  dimcodes <- sapply(dimensies, FUN = create_dimcode, simplify = FALSE)
  codes <- c(codes, dimcodes)
  return(structure(list(short_title = info$TableInfos$ShortTitle,
              order = names(codes), codes = codes), class = "table_code"))
}