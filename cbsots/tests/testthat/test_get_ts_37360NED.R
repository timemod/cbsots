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

test_that("downloading", {
  
  expect_output(
    expect_warning(result1 <- get_ts(id, ts_code, download = TRUE, 
                                     frequencies = "YQ"),
                   "Frequencies Q not present in CBS data", fixed = TRUE)
  )
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  
  expected_label_file <- file.path("expected_output", 
                                   paste0(id, "_1_labels.rds"))
  expect_ts_labels_equal(ts_labels(result1$Y), expected_label_file, 
                         update = update_expected)
  
  expect_output(
    expect_warning(
      result_2010 <- get_ts(id, ts_code, download = TRUE,
                            min_year = 2010),
      "Unknown frequencies TM in CBS data", fixed = TRUE)
  )
  
  expect_identical(result_2010$Y, result1$Y["2010/"])
  
  
  expect_output(
    expect_warning(
      result2 <- get_ts(id, ts_code, download = TRUE),
      "Unknown frequencies TM in CBS data", fixed = TRUE)
  )
  
  
  expect_identical(result1, result2)
})


test_that("no download", {
  expect_silent(
    expect_warning(result1 <- get_ts(id, ts_code),
                   "Unknown frequencies TM in CBS data", fixed = TRUE)
  )
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  
  expected_label_file <- file.path("expected_output", paste0(id, "_1_labels.rds"))
  expect_ts_labels_equal(ts_labels(result1$Y), expected_label_file, 
                         update = FALSE)
  
  expect_silent(
    expect_warning(result2 <- get_ts(id, ts_code, frequencies = "YQ"),
                   "Frequencies Q not present in CBS data", fixed = TRUE)
  )
  
  expect_identical(result1, result2)
  
  expect_silent(
    expect_warning(result1 <- get_ts(id, ts_code, min_year = 2000),
                   "Unknown frequencies TM in CBS data", fixed = TRUE)
  )
  
})

test_that("errors", {
  emsg <- paste("There is no data available for years >= 2022.\nThe last",
                "year with data is 2021.")
  expect_output(
    expect_warning(
      expect_error(get_ts(id, ts_code, download = TRUE, min_year = 2022),
                 emsg, fixed = TRUE),
      "Unknown frequencies TM in CBS data")
  )
  expect_silent(
    expect_warning(
      expect_error(get_ts(id, ts_code, download = FALSE, min_year = 2022),
                   emsg, fixed = TRUE),
      "Unknown frequencies TM in CBS data")
  )
  
  expect_output(
    expect_warning(
      expect_error(
        get_ts(id, ts_code, download = TRUE, frequencies = "Q"),
        "None of the requested frequencies is present in the CBS data"),
      "Frequencies Q not present in CBS data"
    )
  )
  
  expect_warning(
    expect_error(
      get_ts(id, ts_code, download = FALSE, frequencies = "Q"),
      "None of the requested frequencies is present in the CBS data"),
    "Frequencies Q not present in CBS data"
  )
})