# This function reads a downloaded table from a file. 
# If read_downloaded_data == TRUE, then the  file has just been downloaded.
# If read_downloaded_data == FALSE, thenn the file was downloaded in a previous
# call of get_ts.
#
read_table <- function(id, data_dir, code, selected_code, min_year, frequencies,
                       read_downloaded_data = FALSE) {
  
  read_ok <- FALSE
  
  meta <- read_meta_data(data_dir)
  
  # prevent notes from R CMD check about no visible binding for global
  # or no visible global function
  Perioden <- NULL
  
  if (!is.null(meta)) {
    check_language(meta)
    cbs_code <- get_cbs_code(meta)
    check_unknown_keys(id, selected_code, cbs_code)
    data <- read_data_file(data_dir, cbs_code$Topic$Key)
    if (!is.null(data)) {
      period_keys <- get_period_keys(meta, min_year, frequencies)
      read_ok <- check_read_data(data, selected_code, period_keys = period_keys)
      if (read_ok && (!is.null(min_year) || !is.null(frequencies))) {
        data <- data[Perioden %in% period_keys]
      }
    } 
  }
  
  
  if (read_ok) {
    if (!read_downloaded_data) {
      check_code(id, code, selected_code, cbs_code, downloaded = FALSE)
    }
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
    warning(e)
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
    
    data <- fread(data_file, drop = "ID", data.table = FALSE, 
                  na.strings = c(".", ""))
    
    #
    # fix character columns, character columns typically arise when 
    # the data contains old style NA strings 
    #
    
    data_cols <- match(topic_keys, colnames(data))
    
    data_col_classes <- sapply(data[, data_cols, drop = FALSE], FUN = class)
    
    # check if there are data columns with stange types
    weird_col_classes <- ! data_col_classes %in% c("numeric", "integer", 
                                                   "character", "logical")
    
    if (any(weird_col_classes)) {
      stop(paste("Column with illegal classes",
                 paste(unique(data_col_classes[weird_col_classes]), collapse = ","),
                 "found"))
    }
    
    data_col_is_character <- data_col_classes == "character"
    
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
      
      cols <- data_cols[data_col_is_character]
      data[ , cols] <- lapply(data[ , cols, drop = FALSE], 
                              FUN = convert_character_data_cols)
    }
    
    #
    # convert integer columns to numeric
    #
    data_col_is_integer <- data_col_classes == "integer"
    if (any(data_col_is_integer)) {
      cols <- data_cols[data_col_is_integer]
      data[ , cols] <- lapply(data[ , cols, drop = FALSE], FUN = as.numeric) 
    }
    
    #
    # fix logical data columns. logical columns arise when a column is 
    # completely empty
    #
    data_col_is_logical <- data_col_classes == "logical"
    # TODO: check if any logical column contains any non-NA value,
    # otherwise give an error message
    if (any(data_col_is_logical)) {
      
      convert_logical_data_cols <- function(x) {
        
        # Logical columns may arise if a column is completely
        # empty. In that case replace the result with NA_real_.
        # Otherwise give an error
        
        if (all(is.na(x))) {
          return(rep(NA_real_, length(x)))
        } else {
          stop("Error reading data ... found logical data columns")
        }
      }
      
      cols <- data_cols[data_col_is_logical]
      data[ , cols] <- lapply(data[ , cols, drop = FALSE], 
                              FUN = convert_logical_data_cols)
    }
    
    # finally convert to data.table
    data <- as.data.table(data)
  },
  error = function(e) {
    data <<- NULL
    warning(e)
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
    cat("missing topics\n")
    return(FALSE)
  }
  
  # check dimension keys
  for (dimension in setdiff(names(code), "Topic")) {
    if (any(!code[[dimension]]$Key %in% data[[dimension]])) {
      return(FALSE)
    }
  }
  
  
  if (!all(period_keys %in% data$Perioden)) {
    cat("missing periods\n")
    print(period_keys)
    print(data$Perioden)
  }
  
  return(all(period_keys %in% data$Perioden))
}

