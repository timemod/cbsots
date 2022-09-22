library(cbsots)
library(testthat)

rm(list = ls())

id <- "7137shih"

context("get_ts: compare expected output")

dum <- Sys.setlocale("LC_COLLATE", "C")

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8")

update_expected <- FALSE

# 
# In this test we will compare the results with expected output, 
# therefore we use refresh = FALSE, and the files in directory
# raw_cbs_data should be added to the Git repository.
# We should make sure that subsequent calls of get_ts do not
# modify the data.
# 

ts_code <- readRDS("tscode/tscode.rds")

source("utils/check_ts_table.R")


test_that(id, {

  # we don't expect any output because refresh = FALSE and the data is
  # present in directory raw_cbs_data
  result1 <- expect_silent(get_ts(id, ts_code, refresh = FALSE, 
                                  min_year = 2015))
  
  expect_equal(class(result1$ts_names), "data.frame")
  expect_equal(unname(sapply(result1$meta, FUN = class, USE.NAMES = FALSE)), 
               rep("data.frame", length(result1$meta)))
  
  # 
  # compare print resut
  #
  expected_output_file <- file.path("expected_output",
                                    paste0(id, "_print_result.txt"))
  expect_known_output(print(result1), expected_output_file,
                      update = update_expected)

  
  expected_output_file <- file.path("expected_output", paste0(id, "_y.rds"))
  result1_data_y <- result1$Y
  ts_labels(result1_data_y) <- NULL
  expect_known_value(result1_data_y, expected_output_file, 
                     update = update_expected)
  
  expected_output_file <- file.path("expected_output", paste0(id, "_m.rds"))
  result1_data_m <- result1$M
  ts_labels(result1_data_m) <- NULL
  expect_known_value(result1_data_m, expected_output_file, 
                     update = update_expected)
  
  expected_ts_names_file <- file.path("expected_output", 
                                      paste0(id, "_ts_names.rds"))
  expect_known_value(result1$ts_names, expected_ts_names_file, 
                     update = update_expected)
  
  expected_label_file <- file.path("expected_output", paste0(id, "_labels.rds"))
  expect_known_value(ts_labels(result1$Y), expected_label_file, 
                     update = update_expected)
  expect_known_value(ts_labels(result1$M), expected_label_file, 
                     update = update_expected)
  
  check <- check_ts_table(result1, id)
  expect_true(check)
  
  xlsx_file <- "output/get_ts_2.xlsx"
  write_table_ts_xlsx(result1, xlsx_file)
  
  
  data_y <- read_ts_xlsx(xlsx_file, sheet = "annual")
  data_m <- read_ts_xlsx(xlsx_file, sheet = "monthly")
  expect_equal(result1$Y, data_y)
  expect_equal(result1$M, data_m)
  
  meta <- result1$meta
  for (name in names(meta)) {
    meta_data <- meta[[name]]
    meta_data[] <- lapply(meta_data, FUN = as.character)
    meta_data[] <- lapply(meta_data, 
                          FUN = function(x) ifelse(is.na(x) | x == "", 
                                                   NA_character_, x))
    sheet_name <- paste0("meta_data_", name)
    if (nchar(sheet_name) > 31) {
      sheet_name <- substr(sheet_name, 1, 31)
    }
    meta_data_sheet <- readxl::read_excel(xlsx_file, sheet = sheet_name,
                                          trim_ws = FALSE, 
                                          col_types = "text")
    meta_data_sheet <- as.data.frame(meta_data_sheet)
    meta_data_sheet[] <- lapply(meta_data_sheet, 
                                FUN = function(x) gsub("\r", "", x))
    expect_equal(meta_data_sheet, meta_data)
  }
})
