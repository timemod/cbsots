library(cbsots)
library(testthat)

rm(list = ls())

id <- "84328NED"

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8") 

ts_code <- readRDS(sprintf("tscode/tscode_%s.rds",id))

source("utils/check_ts_table.R")
source("utils/read_match_report.R")

raw_cbs_dir <- "raw_cbs_data"

os_type <- .Platform$OS.type

test_that(id, {
  msg <- paste("Duplicate keys in cbs meta data for dimension Topic in",
               "table 84328NED:\n'KapitaalgoederenvoorraadEindbalans_1'\\.")
  expect_warning(result1 <- get_ts(id, ts_code, download = FALSE),
                 msg)
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
})


test_that(paste(id, "errors"), {
  ts_code_err <- ts_code
  ts_code_err$`84328NED`$codes$Topic[9, "Select"] <- TRUE
  ts_code_err$`84328NED`$codes$Topic[9, "Code"] <- "xxx"
  msg <-  paste0("Duplicate keys selected in timeseries coding for dimension",
                " Topic in table 84328NED:\n",
                "'KapitaalgoederenvoorraadBeginbalans_3', 'Afschrijvingen_6'",
                ", 'KapitaalgoederenvoorraadEindbalans_1'\\.") 
  expect_error(get_ts(id, ts_code_err, download = FALSE), msg)
  
  
  ts_code_err <- ts_code
  ts_code_err$`84328NED`$codes$Topic[7, "Code"] <- "wnd"
  
  expect_warning(result1 <- get_ts(id, ts_code_err, download = FALSE))
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_known_value(ts_labels(result1$Y),
                     file.path("expected_output", paste0(id, "_1_labels_",
                                                         os_type, ".rds")))
  
  ts_code_err$`84328NED`$codes$Topic[7, "Select"] <- TRUE
  msg <- "Duplicate codes found for Topic:\nwnd\n."
  expect_error(get_ts(id, ts_code_err, download = FALSE), msg)
})

test_that(paste(id,  "alt"), {
  ts_code <- readRDS(sprintf("tscode/tscode_%s_2.rds",id))
  msg <- paste("Duplicate keys in cbs meta data for dimension Topic in",
               "table 84328NED:\n'KapitaalgoederenvoorraadEindbalans_1'\\.")
  expect_warning(result1 <- get_ts(id, ts_code, download = FALSE),
                 msg)
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  expect_known_value(ts_labels(result1$Y),
                     file.path("expected_output", paste0(id, "_2_labels_", 
                                                         os_type, ".rds")))
})

