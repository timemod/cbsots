read_table <- function(data_dir, code, min_year, frequencies) {
  
  read_ok <- FALSE
  
  meta <- read_meta_data(data_dir)
  
  # prevent notes from R CMD check about no visible binding for global
  # or no visible global function
  Perioden <- NULL
  
  if (!is.null(meta)) {
    check_language(meta)
    cbs_code <- get_cbs_code(meta)
    code <- check_code(code, cbs_code)
    data <- read_data_file(data_dir, cbs_code$Topic$Key)
    if (!is.null(data)) {
      period_keys <- get_period_keys(meta, min_year, frequencies)
      read_ok <- check_read_data(data, code, period_keys = period_keys)
      if (read_ok && (!is.null(min_year) || !is.null(frequencies))) {
        data <- data[Perioden %in% period_keys]
      }
    }
  }
  
  if (read_ok) {
    return(list(meta = meta, data = data, cbs_code = cbs_code))
  } else {
    return(NULL)
  }
}

# read meta data from downloaded csv files in directory dir
#' @importFrom data.table fread
#' @importFrom data.table as.data.table
#' @importFrom utils read.csv
read_meta_data <- function(dir) {
  
  if (!dir.exists(dir)) {
    return(NULL)
  }
  
  read_meta_csv <- function(name) {
    
    filename <- file.path(dir, paste0(name, ".csv"))
    
    # check if the file is empty. If the file is empty read.csv file give
    # an error
    tmp <- read.csv(filename, nrows = 1, header = FALSE)
    if (nrow(tmp) == 0) {
      # empty file
      return(NULL)
    } else {
      return(read.csv(filename, stringsAsFactors = FALSE, 
                      colClasses = "character"))
    }
  }
  
  tryCatch({
    
    ret <- sapply(c("TableInfos", "DataProperties", "CategoryGroups"),
                  FUN = read_meta_csv, simplify = FALSE)
    
    ret$TableInfos$ID <- as.numeric(ret$TableInfos$ID)
    ret$DataProperties$ID <- as.numeric(ret$DataProperties$ID)
    ret$DataProperties$Position <- as.numeric(ret$DataProperties$Position)
    ret$DataProperties$ParentID <- as.numeric(ret$DataProperties$ParentID)
    
    dp <- as.data.table(ret$DataProperties)
    
    # prevent notes from R CMD check about no visible binding for global
    Type <- NULL
    
    dimensions <- dp[endsWith(Type, "Dimension")]$Key
    
    dimension_data <- sapply(dimensions, FUN = read_meta_csv, 
                             simplify = FALSE)
    
    return(structure(c(ret, dimension_data), class = "cbs_table"))
  },
  error = function(e) {
    print(e)
    warning(paste("Error reading meta data files in directory", dir, "."))
  })
  
  # error
  return(NULL)
}

# Read raw cbs data from  the csv file
# RETURN  the data as data.table, or NULL if a read error occurred
read_data_file <- function(dir, topic_keys) {
  
  if (!dir.exists(dir)) {
    return(NULL)
  }
  
  data_file <- file.path(dir, "data.csv")
  
  data <- NULL
  
  tryCatch({
    
    # read data. Use colClasses = "character" to prevent that empty columns
    # get the column class "logical". We do not use colClasses = "numeric" 
    # for the data columns because in the csv file a numbers are represented
    # with a character string such as "     560", therefore fread will read
    # this data as a column.
    
    # use numeric column clases, this ensures that empty columns do not become 
    # logical columns
    col_classes <- rep("numeric", length(topic_keys))
    names(col_classes) <- topic_keys

    data <- fread(data_file, drop = "ID", colClasses = col_classes,
                  data.table = FALSE)
    
    data_col_is_character <- sapply(data[ , topic_keys], FUN = is.character)

    if (any(data_col_is_character)) {
      
      # NA-string used in older versions of cbsodataR (see code below)
      na_strings_old <- c("       .", ".", "       -")  
    
      convert_character_data_cols <- function(x) {
        
        # convert numeric columns to numeric
      
        # The raw cbs data downloaded with older versions of cbsots /cbsodataR 
        # contained strings such as "       ." for NA values. Therefore replace 
        # them with NA_character_. Note that it was not possible to use argument 
        # na.strings of function fread, because fread does not support NA strings 
        # with spaces.
      
        ret <- ifelse(x %in% na_strings_old, NA_character_, x)
      
        return(as.numeric(ret))
      }
    
      cols <- topic_keys[data_col_is_character]
      data[ , cols] <- lapply(data[ , cols, drop = FALSE], 
                              FUN = convert_character_data_cols)
    }
    
    data <- as.data.table(data)
  },
  error = function(e) {
    print(e)
    warning(paste("Error reading file", data_file, "."))
  }
  )
  
  if (is.null(data)) {
    return(NULL)
  } else {
    return(data)
  }
}

# this function returns TRUE if data contains all keys in code and period_keys
check_read_data <- function(data, code, period_keys) {
  
  # check if all dimensions are present 
  dimensions <- setdiff(names(code), "Topic")
  missing_dimensions <- setdiff(dimensions, colnames(data))
  if (length(missing_dimensions) > 0) {
    return(FALSE)
  }
  
  # check if all topics are present in the columns of data
  topic_keys <- code$Topic$Key
  missing_topics <- setdiff(topic_keys, colnames(data))
  if (length(missing_topics) > 0) {
    return(FALSE)
  }
  
  # check dimension keys
  for (dimension in setdiff(names(code), "Topic")) {
    if (any(!code[[dimension]]$Key %in% data[[dimension]])) {
      return(FALSE)
    }
  }
  return(all(period_keys %in% data$Perioden))
}

