library(cbsots)
library(testthat)

rm(list = ls())

context("get_ts table 82595NED")

options(encoding = "native.enc")

ts_code_file_1 <- "tscode/tscode_82595NED_1.rds"
ts_code_file_2 <- "tscode/tscode_82595NED_2.rds"
ts_code_file_3 <- "tscode/tscode_82595NED_3.rds"

ts_code_1 <- readRDS(ts_code_file_1)
ts_code_2 <- readRDS(ts_code_file_2)
ts_code_3 <- readRDS(ts_code_file_3)

source("utils/check_ts_table.R")

id <- "82595NED"

raw_cbs_dir <- tempdir()

corrupt_data_dir <- "corrupt_data_files"

test_that(id, {
  
  result1 <- expect_output(get_ts(id, ts_code_1, refresh = TRUE, 
                                  min_year = 2008, raw_cbs_dir = raw_cbs_dir))
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_equal(ncol(result1$Y), 2)
  
  result2 <- expect_silent(get_ts(id, ts_code_2, refresh = FALSE, 
                                  min_year = 2008, raw_cbs_dir = raw_cbs_dir))
  check <- check_ts_table(result2, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_equal(ncol(result2$Y), 4)
})


test_that(paste(id, "download_all_keys"), {
  
  result1 <- expect_output(get_ts(id, ts_code_1, refresh = TRUE, 
                                  min_year = 2017, frequencies = "Y",
                                  raw_cbs_dir = raw_cbs_dir, 
                                  download_all_keys = TRUE))
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_equal(ncol(result1$Y), 2)
  
  result2 <- expect_silent(get_ts(id, ts_code_3, refresh = FALSE, 
                                  min_year = 2017, frequencies = "Y", 
                                  raw_cbs_dir = raw_cbs_dir))
  check <- check_ts_table(result2, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_equal(ncol(result2$Y), 6)
})

test_that(paste(id, "deleted keys (1)"), {

  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic <- topic[Key != 'InvoerVanGoederen_3']
  
  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic

  msg <- paste0("Keys in code for dimension Topic in table 82595NED do not",
                " agree with the keys on file.\n",
                "Download the data with function get_ts using argument",
                " refresh or download.")
  expect_warning(
    result1 <- get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
                      frequencies = "Y"),
    msg)
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
})

test_that(paste(id, "deleted keys (2)"), {
  
  # in this test we force a new download
  
  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic <- topic[Key != 'InvoerVanGoederen_3']
  
  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic
  
  msg <- paste0("Keys in code for dimension Topic in table 82595NED do not",
                " agree with CBS keys.\n",
                "Update the table coding with the shiny application edit_ts_code.")
  expect_warning(expect_message(expect_output(
    result1 <- get_ts(id, ts_code, min_year = 2016, raw_cbs_dir = raw_cbs_dir,
                      frequencies = "Y"))),
    msg)
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
})

test_that("non unique key differences", {

  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic <- topic[Key != 'Totaal_135']

  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic

  # this statement should give an error because of the running numbers
  msg <- paste0("Keys in code for dimension Topic in table 82595NED do not",
                " agree with the keys on file.\n",
                "The problem keys without running number are not unique.\n",
                "Download the data with function get_ts using argument",
                " refresh or download.")
  expect_error(get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
                      frequencies = "Y"),
               msg)
})

test_that("modified title", {

  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic$Title[151] <- "xxx"

  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic

  msg <- paste0("Titles in code for dimension Topic in table 82595NED do not",
                " agree with the keys on file.\n",
                "Download the data with function get_ts using argument",
                " refresh or download.")
  expect_warning(
    result1 <- get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
                      frequencies = "Y"),
    msg)

  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir,
                          min_year = 2017)
  expect_true(check)
})


test_that(paste(id, "modified key and downloading"), {

  topic <- copy(ts_code_1$table_code$`82595NED`$codes$Topic)
  topic[Key == 'InvoerVanGoederen_3', Key := "aap_3"]

  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic

  msg <- paste0("Keys in code for dimension Topic in table 82595NED do not",
                " agree with CBS keys.\n",
                "Update the table coding with the shiny application edit_ts_code.")
  expect_warning(expect_message(expect_output(
    result1 <- get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
                      frequencies = "Y", download = TRUE))),
    msg)

  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
})

test_that("non unique key differences with download", {

  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic <- rbind(topic,
                 data.table(Key = "Totaal_136", Select = FALSE, Code  ="xxx",
                            Title = "dummy2", OrigKeyOrder = "Totaal_136"))

  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic

  # this statement should give an error because of the running numbers
  msg <- paste0("Keys in code for dimension Topic in table 82595NED do not",
                " agree with CBS keys.\n",
                "The problem keys without running number are not unique.\n",
                "Update the table coding with the shiny application edit_ts_code.")
  expect_error(expect_output(
    get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
           download = TRUE)),
    msg)
})

test_that("unknown key", {
  
  topic <- ts_code_1$table_code$`82595NED`$codes$Topic
  topic <- rbind(topic,
                 data.table(Key = "Goudimport", Select = TRUE, Code  ="goudi",
                            Title = "dummy2", OrigKeyOrder = "Goudimport"))
  
  ts_code <- ts_code_1
  ts_code$table_code$`82595NED`$codes$Topic <- topic
  
  msg <- "Unknown keys in code for dimension Topic in table 82595NED:\nGoudimport\n."
  
  expect_error(
    get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
           download = FALSE),
    msg)
  
  expect_error(expect_output(
    get_ts(id, ts_code, min_year = 2017, raw_cbs_dir = raw_cbs_dir,
           download = TRUE)),
    msg)
})


#
# extra tests for corrupt data files
#

data_dir <- file.path(raw_cbs_dir, id)
data_file <- file.path(data_dir, "data.csv")

copy_corrupt_data_file <- function(suffix) {
  corrupt_data_file <- file.path(corrupt_data_dir, paste0("data_", 
                                                          suffix, ".csv"))
  return(file.copy(corrupt_data_file, data_file, overwrite = TRUE))
}

test_that("corrupt data (1)", {
  
  # corrupt file 1  (text instead of number)
  
  ok <- copy_corrupt_data_file("corrupt1")
  expect_true(ok)
 
  wmsg <- "Topic 'Totaal_1' contains text data:\n\"piet\"."
  expect_warning(
    result1 <- get_ts(id, ts_code_1, download = FALSE, 
                          min_year = 2017, 
                          raw_cbs_dir = raw_cbs_dir),
    wmsg, fixed = TRUE)
  
    # now with refresh = FALSE
  expect_warning(
    result1 <- get_ts(id, ts_code_1, refresh = FALSE,  min_year = 2017, 
                      raw_cbs_dir = raw_cbs_dir),
    wmsg, fixed = TRUE)
  
  # save result in global environment for the test with corrupt3
  result_corrupt1 <<- result1
  
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_equal(ncol(result1$Y), 2)
})

test_that("data with logical column", {
  
  ok <- copy_corrupt_data_file("corrupt2")
  expect_true(ok)
  
  expect_silent(
    result1 <- get_ts(id, ts_code_1, download = FALSE, 
                        min_year = 2017, 
                        raw_cbs_dir = raw_cbs_dir))
  
  tot1_s01_q <- result1$Q$tot1_s01
  tot1_s01_y <- result1$Y$tot1_s01
  expect_equal(tot1_s01_q, 
               regts(c(1, 0, NA, NA), start = "2017Q1"),
               check.attributes = FALSE)
  expect_equal(tot1_s01_y, 
               regts(0, start = "2017"),
               check.attributes = FALSE)
})


test_that("corrupt data (3)", {
  
  # corrupt file 3  (text instead of number in two columkns)
  
  ok <- copy_corrupt_data_file("corrupt3")
  expect_true(ok)
  
  warnings <- capture_warnings(
    result1 <- get_ts(id, ts_code_1, download = FALSE, 
                      min_year = 2017, 
                      raw_cbs_dir = raw_cbs_dir)
  )
    
  
  expected_warnings <- c("Topic 'Totaal_1' contains text data:\n\"piet\".",
             "Topic 'Totaal_7' contains text data:\n\"jan\".")
  expect_equal(warnings, expected_warnings)
 
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_equal(ncol(result1$Y), 2)
  
  expect_equal(result1, result_corrupt1)
})



