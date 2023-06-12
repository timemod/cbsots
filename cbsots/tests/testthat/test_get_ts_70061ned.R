library(cbsots)
library(testthat)

rm(list = ls())

id <- "70061ned"

update_expected <- TRUE

dum <- Sys.setlocale("LC_COLLATE", "C")

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8")

ts_code <- readRDS(sprintf("tscode/tscode_%s.rds",id))

raw_cbs_dir <- "raw_cbs_data"

test_that("reading the previous result", {
  expect_silent(
    result1 <<- get_ts(id, ts_code, download = FALSE, raw_cbs_dir = raw_cbs_dir)
  )
  # to check the result, we cannot use check_ts_table, because this table
  # has missing values for  periods 2004, 2018 and 2020.
  data_y_expected <- fread("raw_cbs_data/70061ned/data.csv")[, -1]
  data_y_expected$Perioden <- sub("JJ00", "Y",  data_y_expected$Perioden)
  data_y_expected <- as.regts(data_y_expected, time_column = "Perioden",
                              strict = FALSE)
  colnames(data_y_expected) <- "tot_leden"
  ts_labels(data_y_expected) <- "Totaal leden vakverenigingen (x 1000)" 
  expect_equal(result1$Y, data_y_expected)
})
