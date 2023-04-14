library(cbsots)
library(testthat)

rm(list = ls())

id <- "81261ned"

test_name <- paste0(id, "_many_keys")

update_expected <- FALSE

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8")

ts_code <- readRDS(sprintf("tscode/tscode_%s.rds",id))
ts_code <- update_tables(ts_code, ids = id)

ts_code$`81261ned`$codes$MineraleBrandstoffenEnChemie$Select <- TRUE
ts_code$`81261ned`$codes$MineraleBrandstoffenEnChemie$Code <- 
  ts_code$`81261ned`$codes$MineraleBrandstoffenEnChemie$Key
ts_code$`81261ned`$codes$MineraleBrandstoffenEnChemie$Select[1] <- FALSE

source("utils/check_ts_table.R")
source("utils/read_match_report.R")
source("utils/check_titles_and_labels.R")

raw_cbs_dir <- tempfile()

test_that(id, {
  report1 <- capture_output(
    result1 <- get_ts(id, ts_code, download = TRUE,
                      frequencies = "Y", min_year = 2021,
                      raw_cbs_dir = raw_cbs_dir)
  )
                                  
  expect_known_output(cat(report1),  file.path("expected_output", 
                                               paste0(test_name, "_report_a1.txt")),
                      print = TRUE, update = update_expected)
  expect_identical(names(result1), c("Y", "ts_names", "meta"))
  expected_label_file <- file.path("expected_output", 
                                   paste0(test_name, "_labels_a1.rds"))
  expect_ts_labels_equal(ts_labels(result1$Y), expected_label_file, 
                         update = update_expected)
  expect_identical(start_period(result1$Y), period(2021))
})



