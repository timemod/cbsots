# read meta data from downloaded csv files in directory dir
#' @importFrom data.table fread
#' @importFrom data.table as.data.table
read_meta_data <- function(dir) {
  
  if (!dir.exists(dir)) {
    return(NULL)
  }
  
  read_meta_csv <- function(name) {
    filename <- file.path(dir, paste0(name, ".csv"))
    return(fread(filename, data.table = FALSE))
  }
  
  tryCatch({
      ret <- sapply(c("TableInfos", "DataProperties", "CategoryGroups"),
                  FUN = read_meta_csv, simplify = FALSE)
    
      dp <- as.data.table(ret$DataProperties)
      
      # prevent notes from R CMD check about no visible binding for global
      Type <- NULL
      
      dimensions <- dp[endsWith(Type, "Dimension")]$Key
      
      dimension_data <- sapply(dimensions, FUN = read_meta_csv, 
                               simplify = FALSE)
    
      return(structure(c(ret, dimension_data), class = "cbs_table"))
    },
    error = function(e) {
      warning(paste("Error reading files data from directory", dir, "."))
      return(NULL)
  })  
}