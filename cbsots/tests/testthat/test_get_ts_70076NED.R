library(cbsots)
library(testthat)
library(regts)

rm(list = ls())

id <- "70076ned"


dum <- Sys.setlocale("LC_COLLATE", "C")

# Use UTF-8 encoding, because the files in raw_cbs_dir contain diacritics
# and have been created using UTF_8 encodng.
options(encoding = "UTF-8")

ts_code <- readRDS("tscode/tscode2.rds")

update_expected <- FALSE

source("utils/check_ts_table.R")

test_that(paste(id, "no refresh"), {
   
   raw_cbs_dir <- "raw_cbs_data"
   
   result1 <- expect_silent(get_ts(id, ts_code, refresh = FALSE))
   
   check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
   expect_true(check)
})

test_that(paste(id, "refresh"), {
  
  raw_cbs_dir <- tempdir()
  
  result1 <- expect_output(get_ts(id, ts_code, refresh = TRUE, min_year = 2016,
                                  frequencies = "Y", raw_cbs_dir = raw_cbs_dir))
  
  expected_output_file <- file.path("expected_output", paste0(id, ".rds"))
  
  result1_data <- result1$Y
  ts_labels(result1_data) <- NULL
  expect_known_value(result1_data, expected_output_file, 
                     update = update_expected)
  
  expected_ts_names_file <- file.path("expected_output", 
                                      paste0(id, "_ts_names.rds"))
  expect_known_value(result1$ts_names, expected_ts_names_file, 
                     update = update_expected)
  
  expected_label_file <- file.path("expected_output", paste0(id, "_labels.rds"))
  expect_known_value(ts_labels(result1$Y), expected_label_file, 
                     update = update_expected)

  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
})
