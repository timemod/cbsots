# Get the CBS code for the Topics and dimensions from the CBS meta data
get_cbs_code <- function(meta_data) {
  
  data_prop <- meta_data$DataProperties
  
  #############################################################################
  # get list of dimensions
  #############################################################################

  dimensions <- data_prop[endsWith(Type, "Dimension") & 
                          Type != "TimeDimension"]$Key
  if ("Perioden" %in% dimensions) {
    if ("TimeDimension" %in% data_prop$Type) {
      stop(paste("PROBLEEM: dimensie met naam \"Perioden\" gevonden naast een",
                 "\"TimeDimension\""))
    } else {
      # Geen Timedimension maar wel een dimensie met naam "Perioden" gevonden,
      # in dat geval gaan we ervan uit dat dit toch een  TimeDimension is.
      dimensions <- setdiff(dimensions, "Perioden")
    }
  }
  
  #############################################################################
  # handle topics
  #############################################################################
  
  if (any(data_prop$Type == "TopicGroup")) {
    
    # Combine the Title of the TopicGroup(s) with the Title of the Topic,
    # so that we have a complete description for each Topic.
    for (i in seq_len(nrow(data_prop))) {
      title <- data_prop$Title[i]
      parent_id <- data_prop$ParentID[i]
      if (!is.na(parent_id)) {
        rownr <- match(parent_id, data_prop$ID)
        prev_title <- data_prop$Title[rownr]
        if (prev_title != title) {
          data_prop$Title[i] <- paste(data_prop$Title[rownr], "-", title)
        }
      }
    }
  }
  
  # prevent notes from R CMD check about no visible binding for global
  # or no visible global function
  `.` <- NULL; Type <- NULL; Key <- NULL; Title <- NULL
  
  topics <- data_prop[Type == "Topic", .(Key, Title, Unit)]

  ##############################################################################
  # create output list  
  ##############################################################################
  code <- list(Topic = topics)
  get_dimension_code <- function(dim) {
    return(meta_data[[dim]][, .(Key, Title)])
  }
  code[dimensions] <- lapply(dimensions, FUN = get_dimension_code)
  return(code)
}