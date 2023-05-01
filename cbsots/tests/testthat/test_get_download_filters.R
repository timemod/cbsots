library(cbsots)
library(testthat)

rm(list = ls())

id <- "999DUMMY"

update_expected <- FALSE

create_cbs_code <- function(...) {
  dim_sizes <- list(...)
  create_dim_code <- function(dim_name) {
    dim_size <- dim_sizes[[dim_name]]
    return(data.frame(Key = paste0(tolower(dim_name), "_", 1:dim_size)))
  }
  return(sapply(names(dim_sizes), FUN = create_dim_code, simplify = FALSE))
}

create_period_keys <- function(start_year, end_year) {
  p <- period_range(start_year, end_year)
  periods_m <- sub("M", "MM", get_periods(change_frequency(p, 12)))
  periods_q <- sub("Q", "KW0", get_periods(change_frequency(p, 4)))
  periods_y <- paste0(get_periods(p), "JJ00")
  return(c(periods_m, periods_q, periods_y))
}

test_that("a - table without dimensions", {
  cbs_code <- create_cbs_code(Topic = 10)
  selected_code <- cbs_code
  period_keys <- create_period_keys(1990, 2020)
  expect_known_output(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code, 
                                           frequencies = "M",
                                           min_year = 1995,
                                           period_keys = period_keys,
                                           download_all_keys = FALSE),
    file = "expected_output/test_get_download_filters_a1.txt",
    update = update_expected
  )
  
  selected_code$Topic <- selected_code$Topic[-1, , drop = FALSE]
  expect_known_output(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code, 
                                             frequencies = "M",
                                             min_year = NA,
                                             period_keys = period_keys,
                                             download_all_keys = FALSE),
    file = "expected_output/test_get_download_filters_a2.txt",
    update = update_expected
  )
  
  cbs_code <- create_cbs_code(Topic = 1000)
  selected_code <- cbs_code
  selected_code$Topic <- selected_code$Topic[-1, , drop = FALSE]
  
  expect_known_output(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code, 
                                             frequencies = "M",
                                             min_year = NA,
                                             period_keys = period_keys,
                                             download_all_keys = FALSE),
    file = "expected_output/test_get_download_filters_a3.txt",
    update = update_expected
  )
  
  cbs_code <- create_cbs_code(Topic = 300)
  selected_code <- cbs_code
  selected_code$Topic <- selected_code$Topic[-1, , drop = FALSE]
  
  expect_known_output(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code, 
                                             frequencies = c("M", "Y", "Q"),
                                             min_year = NA,
                                             period_keys = period_keys,
                                             download_all_keys = FALSE),
    file = "expected_output/test_get_download_filters_a4.txt",
    update = update_expected
  )
})


test_that("b - table with 3 dimensions", {
  period_keys <- create_period_keys(1990, 2020)
  cbs_code <- create_cbs_code(Topic = 10, Dim1 = 10, Dim2 = 10)
  selected_code <- cbs_code
  expect_known_output(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code, 
                                             frequencies = "M",
                                             min_year = 1995,
                                             period_keys = period_keys,
                                             download_all_keys = FALSE),
    file = "expected_output/test_get_download_filters_b1.txt",
    update = update_expected
  )
  
  selected_code$Topic <- selected_code$Topic[-1, , drop = FALSE]
  selected_code$Dim2 <- selected_code$Dim2[-1, , drop = FALSE]
  expect_known_output(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code, 
                                             frequencies = c("Y", "Q", "M"),
                                             min_year = 1995,
                                             period_keys = period_keys,
                                             download_all_keys = FALSE),
    file = "expected_output/test_get_download_filters_b2.txt",
    update = update_expected
  )
  
  cbs_code <- create_cbs_code(Topic = 10, Dim1 = 1000, Dim2 = 1000)
  selected_code <- cbs_code
  selected_code$Topic <- selected_code$Topic[-1, , drop = FALSE]
  selected_code$Dim2 <- selected_code$Dim2[-1, , drop = FALSE]
  selected_code$Dim1 <- selected_code$Dim1[-1, , drop = FALSE]
  expect_known_output(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code, 
                                             frequencies = c("Y", "Q", "M"),
                                             min_year = 1995,
                                             period_keys = period_keys,
                                             download_all_keys = FALSE),
    file = "expected_output/test_get_download_filters_b3.txt",
    update = update_expected
  )
  
  cbs_code <- create_cbs_code(Topic = 10, Dim1 = 200, Dim2 = 200)
  selected_code <- cbs_code
  selected_code$Topic <- selected_code$Topic[-1, , drop = FALSE]
  selected_code$Dim2 <- selected_code$Dim2[-1, , drop = FALSE]
  selected_code$Dim1 <- selected_code$Dim1[-1, , drop = FALSE]
  expect_known_output(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code,
                                             frequencies = c("Y", "Q", "M"),
                                             min_year = 1995,
                                             period_keys = period_keys,
                                             download_all_keys = FALSE),
    file = "expected_output/test_get_download_filters_b4.txt",
    update = update_expected
  )
})


test_that("warning about missing frequencies are not present", {
  cbs_code <- create_cbs_code(Topic = 10)
  selected_code <- cbs_code
  period_keys <- create_period_keys(1990, 2020)
  period_keys <- grep("JJ", period_keys, value = TRUE)
  expect_warning(
    filters <- cbsots:::get_download_filters(id, selected_code, cbs_code,
                                             frequencies = c("Y", "Q", "M"),
                                             min_year = NA,
                                             period_keys = period_keys,
                                             download_all_keys = FALSE),
    "Frequencies Q, M not present in CBS data",
    fixed = TRUE
  )
  expect_equal(unname(filters), list())
})
