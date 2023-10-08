library(cbsots)
library(testthat)

rm(list = ls())

test_that("table_ids", {
  ids <- c("7116shfo", "70076ned")
  ts_code_1 <- select_tables(ts_code_example, ids =  ids)
  expect_identical(names(ts_code_1), sort(ids))
  
  id <- "00372"
  ts_code_2 <- select_tables(ts_code_example, ids =  "00372")
  expect_identical(names(ts_code_2), id)
  
  result <- join_tables(ts_code_1, ts_code_2)
  
  expected_result <- ts_code_example
  attr(expected_result, "package_version") <- packageVersion("cbsots")
  expect_identical(result, expected_result)
})

test_that("table numbers", {
  all_ids <- names(ts_code_example)
  ids <- c("7116shfo", "70076ned")
  ts_code_1 <- select_tables(ts_code_example, ids =  match(ids,  all_ids))
  expect_identical(names(ts_code_1), sort(ids))
  
  id <- "00372"
  ts_code_2 <- select_tables(ts_code_example, ids =  match(id, all_ids))
  expect_identical(names(ts_code_2), id)
  
  result <- join_tables(ts_code_1, ts_code_2)
  
  expected_result <- ts_code_example
  attr(expected_result, "package_version") <- packageVersion("cbsots")
  expect_identical(result, expected_result)
})
                     