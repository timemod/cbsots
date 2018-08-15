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

  ret <- cbsots:::match_keys_and_titles(code, base, base$Key, base$Title)

  expected_result <- list(code_rows = c(2L, 3L, 4L), base_rows = c(1L, 3L, 4L))
  expect_identical(ret, expected_result)
})

test_that("code and base identical", {

  code <- data.frame(Key = c("x", "a", "y", "d", "xxxx"),
                     Title = c("oil", "coal and iron ", "crop", " fish", "xx"),
                     stringsAsFactors = FALSE)

  base <- code

  ret <- cbsots:::match_keys_and_titles(code, base, base$Key, base$Title)

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

  ret <- cbsots:::match_keys_and_titles(code, base, base$Key, base$Title)

  expected_result <- list(code_rows = integer(0), base_rows = integer(0))
  expect_identical(ret, expected_result)
})

test_that("keys with running numbers (1)", {

  code <- data.frame(Key = c("Totaal_1", "Invoer_2", "Uitvoer_3", "Export_4",
                             "Totaal_5", "Totaal_6", "steenkool_7"),
                     Title = c("R", "onzin", "uitzxdsfdaa", "textiel",
                               "pluimvee", "rundvee",
                              "steenkool en bruinkool"),
                    stringsAsFactors = FALSE)

  base <- data.frame(Key = c("Totaal_1", "Invoer_2", "Uitvoer_3", "Totaal_4",
                             "Totaal_5", "steenkool_6"),
                     Title = c("middelen-totaal", "invoer", "uitvoer",
                               "pluimvee", "varkens", "steenkool en bruinkool"),
                     stringsAsFactors = FALSE)

  ret <- cbsots:::match_keys_and_titles(code, base, base$Key, base$Title)

  expected_result <- list(code_rows = c(2, 3, 5, 7), base_rows = c(2, 3, 4, 6))
  expect_equal(ret, expected_result)
})

test_that("keys with running numbers (2)", {

  code <- data.frame(Key = c("Totaal_1", "Invoer_2", "Uitvoer_3", "Export_4",
                             "Totaal_5", "Totaal_6", "steenkool_7"),
                     Title = c("middelen totaal", "onzin", "uitzxdsfdaa", "textiel",
                               "pluimvee", "rundvee",
                               "steenkool en bruinkool"),
                     stringsAsFactors = FALSE)

  base <- data.frame(Key = c("Totaal_1", "Invoer_2", "Uitvoer_3", "Totaal_4",
                             "Totaal_5", "steenkool_6"),
                     Title = c("middelen-totaal", "invoer", "uitvoer",
                               "pluimvee", "varkens", "steenkool en bruinkool"),
                     stringsAsFactors = FALSE)

  ret <- cbsots:::match_keys_and_titles(code, base, base$Key, base$Title)

  expected_result <- list(code_rows = c(1, 2, 3, 5, 7),
                          base_rows = c(1, 2, 3, 4, 6))
  expect_equal(ret, expected_result)
})



