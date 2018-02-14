# Returns the cbs code based on a cbs_table object
get_cbs_code <- function(meta_data) {
  
  data_properties <- as.data.table(meta_data$DataProperties)
  dimensies_en_topics <- get_dimensies_en_topics(data_properties)
  list2env(dimensies_en_topics, env = environment())
  
  get_code <- function(x) {
    df <- as.data.table(meta_data[[x]])
    df <- df[, .(Key, Title)]
    return(df)
  }
  code <- sapply(dimensies, FUN = get_code, simplify = FALSE)
  code$Topic <- topics
  return(code)
}

# Haal de dimensies, topics en bijbehorende omschrijvingen uit
# de data_properties data frame zoals ingelezen / gedownload van het CBS.
get_dimensies_en_topics <- function(data_properties) {
  
  dimensies <- data_properties[endsWith(Type, "Dimension") & 
                                 Type != "TimeDimension"]$Key
  
  if ("Perioden" %in% dimensies) {
    if ("TimeDimension" %in% data_properties$Type) {
      stop(paste("PROBLEEM: dimensie met naam \"Perioden\" gevonden naast een",
                 "\"TimeDimension\""))
    } else {
      # Geen Timedimension maar wel een dimensie met naam "Perioden" gevonden,
      # in dat geval gaan we ervan uit dat dit toch een  TimeDimension is.
      dimensies <- setdiff(dimensies, "Perioden")
    }
  }

  if (any(data_properties$Type == "TopicGroup")) {
  
    # Combineer de Title van de TopicGroup met de Title van de Topics,
    # zodat we voor elke Topic een volledige omschrijving (Title) hebben,
    # deze gebruiken we om labels voor de tijdreeksen te maken.

    topic <- ""
    prev_is_topic_group <- FALSE
    prev <- ""
    for (i in seq_len(nrow(data_properties))) {
      type <- data_properties$Type[i]
      title <- data_properties$Title[i]
      if (type == "TopicGroup") {
        if (prev_is_topic_group && title != prev) {
          topic <- paste(topic, title , sep = " - ")
        } else {
          topic <- title
        }
        prev_is_topic_group <- TRUE
      } else {
        if (type == "Topic" &&  topic != "" && title != prev) {
          data_properties$Title[i] <- paste(topic, title, sep = " - ")
        }
        prev_is_topic_group <- FALSE
        topic <- ""
      }
      prev <- title
    }  
  }
  
  topics <- data_properties[Type == "Topic", .(Key, Title)]
  
  return(list(dimensies = dimensies, topics = topics))
}