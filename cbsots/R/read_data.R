# read raw cbs data from  the csv file
# RETURN  the data as data.table, or NULL if a read error occurred
read_data <- function(dir, na_strings) {
  
  if (!dir.exists(dir)) {
    return(NULL)
  }
  
  data_file <- file.path(dir, "data.csv")
  
  data <- NULL

  tryCatch({
      data <- fread(data_file, drop = "ID", na.strings = na_strings)
    },
    error = function(e) {
      warning(paste("Error reading file", data_file, "."))
    }
  )
  
  if (is.null(data)) {
    return(NULL)
  } else {
    return(as.data.table(data))
  }
}