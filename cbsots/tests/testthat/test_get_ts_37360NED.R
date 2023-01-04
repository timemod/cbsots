library(cbsots)
library(testthat)

rm(list = ls())

id <- "37360NED"

context(paste("get_ts", id))

update_expected <- FALSE

dum <- Sys.setlocale("LC_COLLATE", "C")

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8")

ts_code <- readRDS(sprintf("tscode/tscode_%s.rds",id))

source("utils/check_ts_table.R")
source("utils/read_match_report.R")
source("utils/check_titles_and_labels.R")

raw_cbs_dir <- "raw_cbs_data"

wmsg <- "Unknown frequencies TM in CBS data"

test_that(paste0(id, "downloading"), {
  msg <- "Unknown frequencies TM in CBS data"
  expect_output(
    expect_warning(
      result1 <- get_ts(id, ts_code, download = TRUE),
                 msg, fixed = TRUE)
  )
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)

  expected_label_file <- file.path("expected_output", paste0(id, "_1_labels.rds"))
  expect_ts_labels_equal(ts_labels(result1$Y), expected_label_file, 
                         update = update_expected)
})


test_that(paste0(id, "no downloading"), {
  msg <- "Unknown frequencies TM in CBS data"
  expect_silent(
    expect_warning(result1 <- get_ts(id, ts_code),
                   msg, fixed = TRUE)
  )
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  
  expected_label_file <- file.path("expected_output", paste0(id, "_1_labels.rds"))
  expect_ts_labels_equal(ts_labels(result1$Y), expected_label_file, 
                         update = FALSE)
})

