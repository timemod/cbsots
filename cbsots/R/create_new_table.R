#' @import data.table 
create_new_table <- function(id){
  
  info <- get_meta(id = id, cache = TRUE)
  info <<- info
  
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
  }
  dimcodes <- sapply(dimensies, FUN = create_dimcode, simplify = FALSE)
  codes <- c(codes, dimcodes)
  return(structure(list(last_modified = Sys.time(), 
                        short_title = info$TableInfos$ShortTitle,
              order = names(codes), codes = codes), class = "table_code"))
}

# Haal de dimensies, topics en bijbehorende omschrijvingen uit
# de data_properties data frame zoals ingelezen / gedownload van het CBS.
get_dimensies_en_topics <- function(data_properties) {
  
  dimensies <- data_properties[endsWith(Type, "Dimension") & 
                                 Type != "TimeDimension"]$Key
  
  if (any(data_properties$Type == "TopicGroup")) {
    
    # Combineer de Title van de TopicGroup met de Title van de Topics,
    # zodat we voor elke Topic een volledige omschrijving (Title) hebben,
    # deze gebruiken we om labels voor de tijdreeksen te maken.
    
    topic <- ""
    prev_is_topic_group <- FALSE
    
    for (i in seq_len(nrow(data_properties))) {
      type <- data_properties$Type[i]
      title <- data_properties$Title[i]
      if (type == "TopicGroup") {
        if (prev_is_topic_group) {
          topic <- paste(topic, title , sep = " - ")
        } else {
          topic <- title
        }
        prev_is_topic_group <- TRUE
      } else {
        if (type == "Topic" && topic != "") {
          data_properties$Title[i] <- paste(topic, title, sep = " - ")
        }
        prev_is_topic_group <- FALSE
      }
    }  
  }
  
  topics <- data_properties[Type == "Topic", .(Key, Title)]
  
  return(list(dimensies = dimensies, topics = topics))
}
