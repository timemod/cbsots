files <-list.files(pattern = "test.+\\.R")
for (file in files) {
  lines <- readLines(file, warn = FALSE)
  lines <- lines[!grepl("context", lines)]
  writeLines(lines, file)
}
