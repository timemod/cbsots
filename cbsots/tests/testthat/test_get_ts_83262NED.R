library(cbsots)
library(testthat)

rm(list = ls())

id <- "83262NED"

update_expected <- FALSE

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8")

ts_code <- readRDS(sprintf("tscode/tscode_%s.rds",id))

source("utils/check_ts_table.R")
source("utils/read_match_report.R")
source("utils/check_titles_and_labels.R")

raw_cbs_dir <- "raw_cbs_data"


test_that(id, {
  expect_silent(result1 <- get_ts(id, ts_code, download = FALSE))
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  
  expected_output_file <- file.path("expected_output", paste0(id, "_y.rds"))
  result1_data_y <- result1$Y
  ts_labels(result1_data_y) <- NULL
  expect_known_value(result1_data_y, expected_output_file, 
                     update = update_expected)
  
  expected_output_file <- file.path("expected_output", paste0(id, "_h.rds"))
  result1_data_h <- result1$H
  ts_labels(result1_data_h) <- NULL
  expect_known_value(result1_data_h, expected_output_file, 
                     update = update_expected)
  
 
  expected_ts_names_file <- file.path("expected_output", 
                                      paste0(id, "_ts_names.rds"))
  expect_ts_names_equal(result1$ts_names, expected_ts_names_file,
                        update_expected)

  expected_label_file <- file.path("expected_output", paste0(id, "_labels.rds"))
  expect_ts_labels_equal(ts_labels(result1$Y), expected_label_file, 
                                   update = update_expected)
  expect_ts_labels_equal(ts_labels(result1$H), expected_label_file, 
                                   update = FALSE)
 
  expect_silent(result2 <- get_ts(id, ts_code, download = FALSE,
                                  frequencies = "H",
                                  include_meta = FALSE))
  expect_equal(result2$H, result1$H)
  expect_equal(names(result2), c("H", "ts_names"))
  
  expect_warning(result3 <- get_ts(id, ts_code, download = FALSE,
                                  frequencies = "hq",
                                  include_meta = FALSE),
                 "Frequencies Q not present in CBS data")
  expect_equal(result2, result3)
  
  expect_error(get_ts(id, ts_code, download = FALSE, frequencies = "Z"),
               "Unknown frequencies Z specified")
  
  # now try with downloading
  raw_cbs_dir_2 <- tempdir()
  expect_output(result4 <- get_ts(id, ts_code, download = TRUE, 
                                  frequencies = "H", raw_cbs_dir = raw_cbs_dir_2))
  check <- check_ts_table(result4, id, raw_cbs_dir = raw_cbs_dir_2)
  expect_true(check)
  expect_equal(colnames(result2$H), colnames(result4$H))
})



