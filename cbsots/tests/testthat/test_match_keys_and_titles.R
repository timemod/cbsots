library(cbsots)
library(testthat)

rm(list = ls())

context("match_keys_and_titles")

test_that("first simple test", {
  
  code <- data.frame(Key = c("x", "a", "y", "d", "xxxx"),
                    Title = c("oil", "coal and iron ", "crop", " fish", "xx"),
                    stringsAsFactors = FALSE)
  
  base <- data.frame(Key = c("a", "b", "c", "d"),
                     Title = c("oil", "coal", "crop  ", "fish"),
                     stringsAsFactors = FALSE)
  
  ret <- cbsots:::match_keys_and_titles(code, base)
  
  expected_result <- list(code_rows = c(2L, 4L, 3L), base_rows = c(1L, 4L, 3L))
  expect_identical(ret, expected_result)
})

test_that("code and base identical", {
  
  code <- data.frame(Key = c("x", "a", "y", "d", "xxxx"),
                     Title = c("oil", "coal and iron ", "crop", " fish", "xx"),
                     stringsAsFactors = FALSE)
  
  base <- code
  
  ret <- cbsots:::match_keys_and_titles(code, base)
  
  expected_result <- list(code_rows = 1:5, base_rows = 1:5)
  expect_identical(ret, expected_result)
})

test_that("no common keys and titles", {
  
  code <- data.frame(Key = c("x", "a"),
                     Title = c("oil", "coal and iron "),
                     stringsAsFactors = FALSE)
  
  base <- data.frame(Key = c("p", "q"),
                     Title = c(" black", "white"),
                     stringsAsFactors = FALSE)
  
  ret <- cbsots:::match_keys_and_titles(code, base)
  
  expected_result <- list(code_rows = integer(0), base_rows = integer(0))
  expect_identical(ret, expected_result)
})

