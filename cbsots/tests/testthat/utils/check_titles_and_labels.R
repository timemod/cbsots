#
# To check titles and labels, we have to take care of the encoding.
# The expected_output_files are assummed to be created with UTF-8 encoding.
# COnvert the code to native encoding on Windows.
#

convert_encoding <- function(x) {
  # file files in directory expected_output should have been created with
  # Linux (using UTf-8 encoding)
  Encoding(x) <- "UTF-8"
  return(enc2native(x))
}

expect_ts_names_equal <- function(ts_names, filename, update = TRUE) {
  if (.Platform$OS.type != "windows") {
    expect_known_value(ts_names, filename, update = update)
  } else {
    # Convert the encoding in ts_names. The file with expected output
    # should have been created with UTF encoding.
    expected <- readRDS(filename)
    expected[] <- lapply(expected, FUN = convert_encoding)
    expect_equal(ts_names, expected)
    if (update) {
      stop("update not allowed on windows")
    }
  }
  
  return(invisible())
}


expect_ts_labels_equal <- function(ts_labels, filename, update = TRUE) {
  
  if (.Platform$OS.type != "windows") {
    expect_known_value(ts_labels, filename, update = update)
  } else {
    # Convert the encoding in ts_names. The file with expected output
    # should have been created with UTF encoding.
    expected <- readRDS(filename)
    expected <- convert_encoding(expected)
    expect_equal(ts_labels, expected)
    if (update) {
      stop("update not allowed on windows")
    }
  }

  return(invisible())
}