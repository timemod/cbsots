library(cbsots)
library(testthat)

rm(list = ls())

id <- "80590ned"

test_name <- "dst"

update_expected <- FALSE

dum <- Sys.setlocale("LC_COLLATE", "C")

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8")

ts_code <- readRDS("tscode/NLdata_dst.rds")

raw_cbs_dir <- "raw_cbs_data"

source("utils/check_ts_table.R")
source("utils/check_titles_and_labels.R")

test_that("no selections", {
  report1 <- capture_output(
    result_complete <<- get_ts(id, ts_code, refresh = TRUE,
                               raw_cbs_dir = raw_cbs_dir)
  )
  expect_known_output(cat(report1),  file.path("expected_output", 
                                               paste0(test_name, "_report_a1.txt")),
                      print = TRUE, update = update_expected)
  check <- check_ts_table(result_complete, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_equal(names(result_complete), c("Y", "Q", "M", "ts_names", "meta"))
})

test_that("argument frequencies specified", {
  report1 <- capture_output(
    result1 <- get_ts(id, ts_code, refresh = TRUE, frequencies = "M",
                      raw_cbs_dir = raw_cbs_dir)
  )
  expect_known_output(cat(report1),  file.path("expected_output", 
                                              paste0(test_name, "_report_b1.txt")),
                      print = TRUE, update = update_expected)
  expect_identical(result1$M, result_complete$M)
  expect_identical(names(result1), c("M", "ts_names", "meta"))
  expected_label_file <- file.path("expected_output", 
                                   paste0(test_name, "_labels_b1.rds"))
  expect_ts_labels_equal(ts_labels(result1$M), expected_label_file, 
                         update = update_expected)
  
  expect_silent(
    expect_warning(
      result2 <- get_ts(id, ts_code, refresh = FALSE, frequencies = "MH",
                        raw_cbs_dir = raw_cbs_dir),
      "Frequencies H not present in CBS data"
    )
  )
  expect_identical(result1, result2)
  
  expect_output(
    expect_warning(
      result3 <- get_ts(id, ts_code, refresh = TRUE, frequencies = "MH",
                        raw_cbs_dir = raw_cbs_dir),
      "Frequencies H not present in CBS data"
    )
  )
  expect_identical(result1, result3)
  
  expect_warning(
    expect_error(
      get_ts(id, ts_code, refresh = FALSE, frequencies = "H",
             raw_cbs_dir = raw_cbs_dir),
      "None of the requested frequencies is present in the CBS data"
    ),
    "Frequencies H not present in CBS data"
  )
})

test_that("argument min_year specified", {
  report1 <- capture_output(
    result1 <- get_ts(id, ts_code, refresh = TRUE, min_year = 2018,
                      raw_cbs_dir = raw_cbs_dir)
  )
  expect_known_output(cat(report1), 
                      file.path("expected_output", 
                                paste0(test_name, "_report_c1.txt")),
                      print = TRUE, update = update_expected)
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_equal(names(result1), c("Y", "Q", "M", "ts_names", "meta"))
  expect_equal(start_period(result1$Q), period("2018Q1"))
  expect_equal(start_period(result1$M), period("2018M1"))
  expect_equal(start_period(result1$Y), period("2018"))
  expect_identical(result1$M, result_complete$M["2018/"])
  expect_identical(result1$Q, result_complete$Q["2018/"])
  expect_identical(result1$Y, result_complete$Y["2018/"])
  
  report2 <- capture_output(
    result2 <- get_ts(id, ts_code, refresh = FALSE, min_year = 2017,
                      raw_cbs_dir = raw_cbs_dir)
  )
  expect_known_output(cat(report2), 
                      file.path("expected_output", 
                                paste0(test_name, "_report_c2.txt")),
                      print = TRUE, update = update_expected)
  expect_equal(start_period(result2$Q), period("2017Q1"))
  expect_equal(start_period(result2$M), period("2017M1"))
  expect_equal(start_period(result2$Y), period("2017"))
  
  report3 <- capture_output(
    result3 <- get_ts(id, ts_code, refresh = FALSE, min_year = 2000,
                      raw_cbs_dir = raw_cbs_dir)
  )
  expect_known_output(cat(report3), 
                      file.path("expected_output", 
                                paste0(test_name, "_report_c3.txt")),
                      print = TRUE, update = update_expected)
  expect_identical(result3, result_complete)
  
  expect_output(  
    expect_error(
       get_ts(id, ts_code, refresh = TRUE, min_year = 2030,
              raw_cbs_dir = raw_cbs_dir),
      "There is no data available for years >= 2030.\nThe last year with data is 2023.",
      fixed = TRUE
    )
  )
  
  expect_silent(
    expect_error(
      get_ts(id, ts_code, refresh = FALSE, min_year = 2030,
             raw_cbs_dir = raw_cbs_dir),
      "There is no data available for years >= 2030.\nThe last year with data is 2023.",
      fixed = TRUE
    )
  )
})


test_that("argument frequencies AND min_year specified (1)", {
  report1 <- capture_output(
    result1 <- get_ts(id, ts_code, refresh = TRUE, frequencies = "MY",
                      min_year = 2019, raw_cbs_dir = raw_cbs_dir)
  )
  expect_known_output(cat(report1), 
                      file.path("expected_output", 
                                paste0(test_name, "_report_d1.txt")),
                      print = TRUE, update = update_expected)
  expect_equal(names(result1), c("Y", "M", "ts_names", "meta"))
  expect_identical(result1$M, result_complete$M["2019/"])
  expect_identical(result1$Y, result_complete$Y["2019/"])
  
  report2 <- capture_output(
    result2 <- get_ts(id, ts_code, refresh = TRUE, frequencies = "QY",
                      min_year = 2019, raw_cbs_dir = raw_cbs_dir)
  )
  expect_known_output(cat(report2), 
                      file.path("expected_output", 
                                paste0(test_name, "_report_d2.txt")),
                      print = FALSE, update = update_expected)
  expect_equal(names(result2), c("Y", "Q", "ts_names", "meta"))
  expect_identical(result2$Q, result_complete$Q["2019/"])
  expect_identical(result2$Y, result_complete$Y["2019/"])
  
  expect_silent(
    result3 <- get_ts(id, ts_code, refresh = FALSE, frequencies = "Y",
                      min_year = 2021, raw_cbs_dir = raw_cbs_dir)
  )
  expect_equal(names(result3), c("Y", "ts_names", "meta"))
  expect_identical(result3$Y, result_complete$Y["2021/"])
  
  expect_output(  
    expect_error(
      get_ts(id, ts_code, refresh = TRUE, min_year = 2030, frequencies = "M",
             raw_cbs_dir = raw_cbs_dir),
      "There is no data available for years >= 2030.\nThe last year with data is 2023.",
      fixed = TRUE
    )
  )
  
  expect_silent(
    expect_error(
      get_ts(id, ts_code, refresh = FALSE, min_year = 2030, frequencies = "M",
             raw_cbs_dir = raw_cbs_dir),
      "There is no data available for years >= 2030.\nThe last year with data is 2023.",
      fixed = TRUE
    )
  )
})

test_that("argument frequencies AND min_year specified (2)", {
  report1 <- capture_output(
    result1 <- get_ts(id, ts_code, refresh = TRUE, frequencies = "M",
                      min_year = 2003,  raw_cbs_dir = raw_cbs_dir)
  )
  expect_known_output(cat(report1), 
                      file.path("expected_output", 
                                paste0(test_name, "_report_e1.txt")),
                      print = TRUE, update = update_expected)
  expect_equal(names(result1), c("M", "ts_names", "meta"))
  expect_identical(start_period(result1$M), period("2003M01"))
  expect_identical(result1$M, result_complete$M["2003/"])
})
