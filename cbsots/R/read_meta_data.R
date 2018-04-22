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