check_duplicates <- function(values) {
  # TODO: also check other dimension than Topic
  if (anyDuplicated(values$Topic$Code)) {
    return(values$Topic$Code[duplicated(values$Topic$Code)])
  } else {
    return(NULL)
  }
}