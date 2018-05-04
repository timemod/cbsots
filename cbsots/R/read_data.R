# read raw cbs data from  the csv file
# RETURN  the data as data.table, or NULL if a read error occurred
read_data <- function(dir) {
  
  if (!dir.exists(dir)) {
    return(NULL)
  }
  
  data_file <- file.path(dir, "data.csv")
  
  
  
  data <- NULL
  
  tryCatch({
    
    data <- fread(data_file)
    
    # The raw cbs data downloaded with cbsodataR versions prior to 0.3 
    # contained strings such as "       ." for NA values. Therefore replace 
    # them with NA_character_. Note that we cannot use argument na.string of
    # function fread, because fread does not support NA strings with spaces
    # any more.
    na_strings_old <- c("       .", ".", "       -")  
    check_na_strings<- function(x) {
      if (is.character(x)) {
        return(ifelse(x %in% na_strings_old, NA_character_, x))  
      } else {
        return(x)
      }
    }
    data <- as.data.table(lapply(data, FUN = check_na_strings))
  },
  error = function(e) {
    warning(paste("Error reading file", data_file, "."))
  }
  )
  
  if (is.null(data)) {
    return(NULL)
  } else {
    return(data)
  }
}