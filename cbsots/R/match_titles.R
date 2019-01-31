#' @importFrom stringdist stringdist
match_titles <- function(base_titles, code_titles) {
  
  convert_title <- function(x) {
    x <- tolower(x)
    return(gsub("[  ,;.:]", "_", x))
  }
  

  base_titles <- convert_title(base_titles)
  code_titles <- convert_title(code_titles)
  
 
  dfun <- function(x) {
    return(stringdist(x, code_titles, method = "jw"))
  }
  
  ret <- lapply(base_titles, FUN = dfun)
  distmat <- do.call(rbind, ret)
  
  nr <- nrow(distmat)
  nc <- ncol(distmat)
  matdim <- dim(distmat)
  
  c_matches <- numeric(nr)
  dist <- numeric(nr)
  
  wmat <- distmat

  #
  # step 1: find titles in base_titles that occur in code_titles
  #
  for (i in seq_len(nr)) {
    
    # Sometimes cbs keys Totaal-Middelen
    contained_in <- which(grepl(paste0("_-_", base_titles[i]), code_titles) |
                           base_titles[i] == code_titles)
    # if (length(contained_in) > 1) {
    #   cat("probleem\n")
    #   print(base_titles[i])
    #   print(code_titles[contained_in])
    # }
    if (length(contained_in) == 1) {
      r_idx <- i
      c_idx <- contained_in
      c_matches[i] <- c_idx
      dist[i] <- distmat[r_idx, c_idx]
      wmat[r_idx, ] <- Inf
      wmat[ , c_idx] <- Inf
    } else {
      c_matches[i] <- NA
      dist[i] <- NA
    }
  }
  
  
  #
  # next step: from remaining items, try to find the closest match
  #
  
  for (i in seq_len(length(which(is.na(c_matches))))) {

    idx <- which.min(wmat)
    rc_idx <- arrayInd(idx, matdim)
    r_idx <- rc_idx[1, 1]
    c_idx <- rc_idx[1, 2]
    minval <- distmat[idx]
    dist[r_idx] <- minval
    if (minval > 0.2 || length(which(distmat[r_idx, ] == minval)) > 1) {
      # for reach row, only a unique match is allowed
      c_idx <- NA
      dist[r_idx] <- NA
    }


    c_matches[r_idx] <- c_idx

    wmat[r_idx, ] <- Inf
    wmat[ , c_idx] <- Inf
  }

  
  # ma <- data.frame(base = base_titles, 
  #                  code = code_titles[c_matches],
  #                  dist = dist)
  # View(ma)
  
  return(c_matches)
}