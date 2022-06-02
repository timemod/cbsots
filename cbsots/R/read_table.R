# This function reads a downloaded table from a file. 
# If read_downloaded_data == TRUE, then the file has just been downloaded.
# If read_downloaded_data == FALSE, then the file was downloaded in a previous
# call of get_ts.
read_table <- function(id, data_dir, code, selected_code, dimensions,
                       min_year, frequencies, read_downloaded_data = FALSE) {
  
  read_ok <- FALSE
  
  meta <- read_meta_data(data_dir)
  
  if (is.null(meta)) return(NULL)
  
  check_language(meta)
  cbs_code <- get_cbs_code(meta)
  check_unknown_keys(id, selected_code, cbs_code)
  
  period_keys <- get_period_keys(meta, min_year, frequencies)

  data <- read_data_file(data_dir, 
                         selected_code = selected_code,
                         period_keys = period_keys,
                         id = id)

  if (is.null(data)) return(NULL)
  
  if (!read_downloaded_data) {
    check_code(id, code, selected_code, cbs_code, downloaded = FALSE)
  }
  
  return(list(meta = meta, data = data, cbs_code = cbs_code))
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
      return(as.data.table(read.csv(filename, stringsAsFactors = FALSE, 
                           colClasses = "character")))
    }
  }
  
  tryCatch({
    
    ret <- sapply(c("TableInfos", "DataProperties", "CategoryGroups"),
                  FUN = read_meta_csv, simplify = FALSE)
    
    ret$TableInfos$ID <- as.numeric(ret$TableInfos$ID)
    ret$DataProperties$ID <- as.numeric(ret$DataProperties$ID)
    ret$DataProperties$Position <- as.numeric(ret$DataProperties$Position)
    ret$DataProperties$ParentID <- as.numeric(ret$DataProperties$ParentID)
    
    # prevent notes from R CMD check about no visible binding for global
    Type <- NULL
    
    dimensions <- ret$DataProperties[endsWith(Type, "Dimension")]$Key
    
    dimension_data <- sapply(dimensions, FUN = read_meta_csv, 
                             simplify = FALSE)
    
    return(c(ret, dimension_data))
  },
  warning = function(e) {
    warning(e)
    warning(paste0("Error reading meta data files in directory ", dir, "."))
  },
  error = function(e) {
    warning(e)
    warning(paste0("Error reading meta data files in directory ", dir, "."))
  })
  
  # error
  return(NULL)
}

# Read raw cbs data from  the csv file
# RETURN  the data as data.table, or NULL if a read error occurred
read_data_file <- function(dir, selected_code, period_keys, id) {
  
  if (!dir.exists(dir)) {
    return(NULL)
  }

  data_file <- file.path(dir, "data.csv")

  tryCatch({
    data <- fread(data_file, drop = "ID", integer64 = "numeric")
  },
  warning = function(e) {
    warning(e)
    warning(paste0("Error reading file ", data_file, "."))
    return(NULL)
  },
  error = function(e) {
    warning(e)
    warning(paste0("Error reading file ", data_file, "."))
    return(NULL)
  })
  
  if (anyDuplicated(colnames(data))) {
    stop("Duplicate columns in downloaded data for table '", id,
         "'. Something is wrong with this table.")
  }
  
  dimensions <- setdiff(names(selected_code), "Topic")
  topic_keys <- selected_code$Topic$Key
  
  # In weird cases (e.g. table 84328NED), there are duplicate keys in
  # the meta data. As long as there a no duplicate columns in data (see the 
  # test above), we can skip the duplicate columns
  topic_keys <- unique(topic_keys)
  
  # check if all dimensions are present 
  missing_dimensions <- setdiff(dimensions, colnames(data))
  if (length(missing_dimensions) > 0) {
    return(NULL)
  }
  
  # check if all topics are present in the columns of data
  missing_topics <- setdiff(topic_keys, colnames(data))
  if (length(missing_topics) > 0) {
    return(NULL)
  }
  
  if (length(dimensions) > 0) {
    
    # convert dimension columns to character
    data[, (dimensions) := lapply(.SD, as.character), .SDcols = dimensions]
    
    # check dimension keys
    for (dimension in dimensions) {
      if (!all(selected_code[[dimension]]$Key %in% data[[dimension]])) {
        return(NULL)
      }
    }
  }

  # check if data contains a column 'Perioden'
  if (!"Perioden" %in% colnames(data)) {
    stop("Table ", id, " does not contain timeseries")
  }
  
  if (!all(period_keys %in% data$Perioden)) {
    return(NULL)
  }
  
  # now select the columns that are actually needed
  data <- data[, c("Perioden", dimensions, topic_keys), with = FALSE]
  
  
  # Prevent notes from R CMD check about no visible binding for global
  # or no visible global function:
  Perioden <- NULL
  
  # select periods
  data <- data[Perioden %in% period_keys]
  
  data_cols <- topic_keys
  data_col_classes <- data[ , sapply(.SD, class), .SD = data_cols]
  
  # check if there are data columns with stange types
  weird_col_classes <- ! data_col_classes %in% c("numeric", "integer", 
                                                 "character", "logical")
  if (any(weird_col_classes)) {
    stop(paste("Columns with illegal classes",
               paste(unique(data_col_classes[weird_col_classes]), 
                     collapse = ","),
               "found"))
  }
  
  #
  # Fix character columns for topics
  # Character columns typically arise when the csv file contains quoted strings
  # that represents an NA. Note that for these string, argument na.strings
  # of function fread cannot be used, because fread never treats a quoted
  # field in a csv file an an NA. For example,  in ,"", the "" is always
  # treated an an empty string (a string of length 0), and never as an NA.
  # strings. These ttype of quoted string typically occur in old data files

  quoted_na_strings <- c("", ".",  "-")
  fix_quoted_na_string <- function(x) {
    return(ifelse(trimws(x) %in% quoted_na_strings, NA_character_, x))
  }
  
  col_sel <- data_col_classes == "character"
  if (any(col_sel)) {
    cols <- data_cols[col_sel]
    
    # fix quoted na strings
    data[ , (cols) := lapply(.SD, fix_quoted_na_string), .SDcols = cols]
    
    char_cols_before <- data[, cols, with = FALSE]

    # now convert character data to numeric
    suppressWarnings(
      data[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    )
  
    # now check for problematic texts in the data columns
    problem_sel <- is.na(data[, cols, with = FALSE]) & !is.na(char_cols_before)
    if (any(problem_sel)) {
      problem_sel <- which(problem_sel, arr.ind = TRUE)
      problem_list <- split(problem_sel[, "row"], problem_sel[, "col"])
      for (colnr in as.numeric(names(problem_list))) {
        colname <- cols[colnr]
        row_sel <- problem_list[[as.character(colnr)]]
        problem_texts <- unique(char_cols_before[row_sel, colnr, 
                                                        with = FALSE][[1]])
        problem_texts <- trimws(problem_texts)
        header_line <- paste0("Topic '", colname, "' contains text data:\n")
        next_lines <- paste(paste0("\"", problem_texts, "\""), 
                            collapse = ", ")
        warning(paste0(header_line, next_lines, "."))
      }
    }
  }
  
  #
  # convert integer and logical columns to numeric
  #
  col_sel <- data_col_classes %in% c("integer", "logical")
  if (any(col_sel)) {
    cols <- data_cols[col_sel]
    data[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  }
  
  return(data)
}
