library(cbsodataR)

id <- "82601NED"
dp <- get_meta(id, cache = TRUE)$DataProperties

print(system.time({
  for (i in seq_len(nrow(dp))) {
    type <- dp$Type[i]
    title <- dp$Title[i]
    parent_id <- dp$ParentID[i]
    if (!is.na(parent_id)) {
      rownr <- match(parent_id, dp$ID)
      prev_title <- dp$Title[rownr]
      if (prev_title != title) {
        dp$Title[i] <- paste(dp$Title[rownr], "-", title)
      }
    }
  }
}))