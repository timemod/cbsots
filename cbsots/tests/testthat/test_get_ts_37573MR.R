library(cbsots)
library(testthat)

rm(list = ls())

id <- "37573MR"

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
    result1 <<- get_ts(id, ts_code, download = TRUE, raw_cbs_dir = raw_cbs_dir)
  )
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  
  expect_identical(colnames(result1$Y), "Wneg_13")
  expect_identical(colnames(result1$M), "Wneg_13")
})

test_that("reading", {
  expect_silent(
    result2 <- get_ts(id, ts_code, download = FALSE, raw_cbs_dir = raw_cbs_dir,
                      frequencies = "M")
  )
  check <- check_ts_table(result2, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_identical(result2$M, result1$M)
})
