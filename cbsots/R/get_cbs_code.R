# Returns the cbs code based on a cbs_table object
get_cbs_code <- function(meta_data) {
  
  data_properties <- as.data.table(meta_data$DataProperties)
  dimensies_en_topics <- get_dimensies_en_topics(data_properties)
 
  # prevent notes from R CMD check about no visible binding for global
  # or no visible global function
  `.` <- NULL; Key <- NULL; Title <- NULL
  
  get_code <- function(x) {
    df <- as.data.table(meta_data[[x]])
    df <- df[, .(Key, Title)]
    return(df)
  }
  code <- sapply(dimensies_en_topics$dimensies, FUN = get_code, simplify = FALSE)
  code$Topic <- dimensies_en_topics$topics
  
  code <- code[c("Topic", dimensies_en_topics$dimensies)]
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
  
    # Combine the Title of the TopicGroup(s) with the Title of the Topic,
    # so that we have a complete description for each Topic.
    
    for (i in seq_len(nrow(data_properties))) {
      title <- data_properties$Title[i]
      parent_id <- data_properties$ParentID[i]
      if (!is.na(parent_id)) {
        rownr <- match(parent_id, data_properties$ID)
        prev_title <- data_properties$Title[rownr]
        if (prev_title != title) {
          data_properties$Title[i] <- paste(data_properties$Title[rownr], "-", 
                                            title)
        }
      }
    }
  }
    
  # prevent notes from R CMD check about no visible binding for global
  # or no visible global function
  `.` <- NULL; Type <- NULL; Key <- NULL; Title <- NULL
  
  topics <- data_properties[Type == "Topic", .(Key, Title)]
  
  return(list(dimensies = dimensies, topics = topics))
}