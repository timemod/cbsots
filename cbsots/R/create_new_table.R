#' @importFrom cbsodataR cbs_get_meta
#' @import data.table 
create_new_table <- function(id, base_url = NULL){
  
  if (is.null(base_url)) {
    info <- cbs_get_meta(id = id, cache = TRUE)
  } else {
    info <- cbs_get_meta(id = id, cache = TRUE, base_url = base_url)
  }
  
  data_properties <- as.data.table(info$DataProperties)
  dimensions_and_topics <- get_dimensions_and_topics(data_properties)
  dimensions <- dimensions_and_topics$dimensions
  topics <- dimensions_and_topics$topics
  topics$Code <- ""
  topics$Select <- FALSE
  # TODO: handle description
  topics$Description <- ""
  topics$OrigKeyOrder <- topics$Key
  topics <- topics[, c("Key", "Select", "Code", "Title",  "OrigKeyOrder")]
  
  codes <- list(Topic = topics)
  
  create_dimcode <- function(dimension) {
    ret <- info[[dimension]]
    ret$Code <- ""
    ret$Select <- FALSE
    ret$OrigKeyOrder <- ret$Key
    ret <- ret[, c("Key", "Select", "Code", "Title",  "OrigKeyOrder")]
    return(as.data.table(ret))
  }
  dimcodes <- sapply(dimensions, FUN = create_dimcode, simplify = FALSE)
  codes <- c(codes, dimcodes)
  
  cbs_key_order <- rep(TRUE,  length(dimensions) + 1)
  names(cbs_key_order) <- c("Topic", dimensions)
  
  return(structure(list(short_title = info$TableInfos$ShortTitle,
              order = names(codes), cbs_key_order = cbs_key_order,
              codes = codes), class = "table_code"))
}