library(cbsots)
library(testthat)

rm(list = ls())

id <- "81261ned"

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

test_that(id, {
  
  expect_silent(result1 <- get_ts(id, ts_code, download = FALSE))
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)

  expected_label_file <- file.path("expected_output", paste0(id, "_1_labels.rds"))
  expect_ts_labels_equal(ts_labels(result1$Y), expected_label_file, 
                         update = update_expected)
})


test_that(paste(id, "- errrors"), {
  
  ts_code <- readRDS(sprintf("tscode/tscode_%s_err.rds",id))
  msg <- "Topic 'EenheidVanGoederensoort_5' contains text data:\n'kg', 'TJ', '', '1000 kWh'."
  expect_error(get_ts(id, ts_code, download = FALSE),
               msg, fixed = TRUE)
})

