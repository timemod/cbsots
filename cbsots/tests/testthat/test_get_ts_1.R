library(cbsots)
library(testthat)

rm(list = ls())

# 
# In this test we check the refresh and min_year arguments. Note that we cannot 
# compare the results with expected results, because the table data is downloaded
# from the CBS and the table may change over time.
#
context("get_ts: refresh and min_year")

options(encoding = "native.enc")

ts_code <- readRDS("tscode/tscode.rds")

source("utils/check_ts_table.R")

id <- "83460NED"

raw_cbs_dir <- tempdir()

test_that(id, {
  
  result1 <- expect_warning(expect_output(get_ts(id, ts_code,  refresh = TRUE,
                                                 raw_cbs_dir = raw_cbs_dir,
                                                 frequencies = "QM", 
                                                 min_year = 2008,
                                                 include_meta = FALSE)), 
                            "Frequencies M not present in table")
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  
  result2 <- expect_silent(get_ts(id, ts_code, refresh = FALSE, 
                                  raw_cbs_dir = raw_cbs_dir,
                                  frequencies = "Q", min_year = 2008,
                                  include_meta = FALSE))
  expect_identical(result1, result2)
  
  result3 <- expect_silent(get_ts(id, ts_code, refresh = FALSE, min_year = 2010,
                                  raw_cbs_dir = raw_cbs_dir,
                                  frequencies = "Q", include_meta = FALSE))
  expected_result_2010 <- result1
  expected_result_2010$Q <- expected_result_2010$Q["2010/"]
  expected_result_2010$Y <- expected_result_2010$Y["2010/"]
  expect_identical(result3, expected_result_2010)
  
  result4 <- expect_output(get_ts(id, ts_code, refresh = TRUE,
                                  min_year = 2010, 
                                  frequencies = "Q",
                                  raw_cbs_dir = raw_cbs_dir,
                                  include_meta= FALSE))
  expect_identical(result4, expected_result_2010)
  
  # check download = FALSE, we should get an error message
 
  # Now we want the timeseries again for all years. 
  
  expect_error(get_ts(id, ts_code, download = FALSE, frequencies = "Q"),
               paste("The files in directory raw_cbs_data/83460NED are",
                     "incomplete or corrupt. Please download the data again."))
  
  # We expect some output, because the data has to be downloaded again:
  # the last download only downloaded data starting from 2010
  result5 <- expect_output(get_ts(id, ts_code, refresh = FALSE, 
                                  raw_cbs_dir = raw_cbs_dir,
                                  frequencies = "Q", min_year = 2008,
                                  include_meta = FALSE))
  expect_identical(result5, result1)
  
  # Now we want both annual and quarterly timeseries. 
  
  expect_error(get_ts(id, ts_code, download = FALSE, min_year = 2010, 
                      frequencies = "QY"),
               paste("The files in directory raw_cbs_data/83460NED are",
                     "incomplete or corrupt. Please download the data again."))
  
  # We expect some output, because the data has to be downloaded again: the 
  # last download only downloaded only quaterly timeseries.
  result6 <- expect_output(get_ts(id, ts_code, refresh = FALSE,
                                  raw_cbs_dir = raw_cbs_dir,
                                  min_year = 2010,
                                  include_meta = FALSE))
  
  expect_identical(names(result6), c("Y", "Q", "ts_names"))
  result6_Q <- result6
  result6_Q$Y <- NULL
  expect_identical(result6_Q, expected_result_2010)
  
  xlsx_file <- "output/get_ts_1.xlsx"
  write_table_ts_xlsx(result6, xlsx_file)
  
  data_y <- read_ts_xlsx(xlsx_file, sheet = "annual")
  data_q <- read_ts_xlsx(xlsx_file, sheet = "quarterly")
  expect_equal(result6$Y, data_y)
  expect_equal(result6$Q, data_q)
  ts_names <- as.data.frame(readxl::read_excel(xlsx_file, sheet = "ts_names",
                                               trim_ws = FALSE, 
                                               col_types = "text"))
  expect_equal(result6$ts_names, ts_names)
})