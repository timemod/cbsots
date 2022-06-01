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
    # NOTE: if the csv file contains a field "" (for example, ,"", the ""
    # is not treated as a space. The data sometimes contains a "" between 
    # columns, fread cannot treat that as NA
    data <- fread(data_file, drop = "ID", na.strings = c(".", ""),
                  integer64 = "numeric")
  },
  warning = function(e) {
    # if a warning occurs, we do not accept the result and data is set to NULL
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
    warning("Duplicate columns in downloaded data for table ", id,
            " Something is wrong with this table.")
    return(NULL)
  }
  
  dimensions <- setdiff(names(selected_code), "Topic")
  topic_keys <- selected_code$Topic$Key
  
  # In weird cases (e.g. table 84328NED), there are duplicate keys in
  # the meta data. As long as there a no duplicate columns in the data read,
  # we can skip the duplicate columns
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
    # TODO:  improve error message: specify the names of the columns
    stop(paste("Column with illegal classes",
               paste(unique(data_col_classes[weird_col_classes]), 
                     collapse = ","),
               "found"))
  }
  
  #
  # Fix character columns for topics
  # Character columns typically arise when the data contains old style NA 
  # strings.
  
  # NA-string used in older versions of cbsodataR (see code below)
  na_strings_old <- c("       .",  "       -")
  na_strings_empty <- ""  # fread cannot handle na.strings == ""
  na_strings_weird <- c(na_strings_empty, na_strings_old)
  
  data_col_is_character <- data_col_classes == "character"

  if (any(data_col_is_character)) {
    for (colname in data_cols[data_col_is_character]) {

      col_data <- data[[colname]]

      # convert numeric columns to numeric

      # The raw cbs data downloaded with older versions of cbsots /cbsodataR
      # contained strings such as "       ." for NA values. 
      # Also more recent data sometimes contain a field with "", fread cannot
      # treat these fiels as NA. 
      # Therefore replace all of these characters with NA_character_. 
      # Note that it was not possible to use argument
      # na.strings of function fread, because fread does not support NA
      # strings with spaces.
      col_data <- ifelse(col_data %in% na_strings_weird, NA_character_, 
                          col_data)
      
      na_sel_before <- is.na(col_data)
      col_data_new <- suppressWarnings(as.numeric(col_data))
      na_sel_after <- is.na(col_data_new)
      problem_sel <- na_sel_after & !na_sel_before

      if (any(problem_sel)) {
        header_line <- paste0("Topic '", colname, "' contains text data:\n")
        next_lines <- paste(paste0("'", unique(trimws(col_data[problem_sel])), "'"),
                           collapse = ", ")
        next_lines <- strwrap(next_lines, width = 80, exdent = 2)
        msg <-  paste0(header_line, next_lines, ".")
        stop(msg)
      }

      data[, (colname) := col_data_new]
    }
  }

  #
  # convert integer columns to numeric
  #
  data_col_is_integer <- data_col_classes == "integer"
  if (any(data_col_is_integer)) {
    cols <- data_cols[data_col_is_integer]
    data[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  }
  
  #
  # Fix logical data columns. Logical columns arise when a column is 
  # completely empty.
  # TODO: betere foutmelding net als bij controle op character-kolommen
  data_col_is_logical <- data_col_classes == "logical"
  
  if (any(data_col_is_logical)) {
    
    fix_logical_col <- function(x) {
      
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
    data[, (cols) := lapply(.SD, fix_logical_col), .SDcols = cols]
  }  
  
  return(data)
}
