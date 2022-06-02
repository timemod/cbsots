library(cbsots)
library(testthat)

rm(list = ls())

id <- "81261ned"

context(paste("get_ts", id))

update_expected <- FALSE

dum <- Sys.setlocale("LC_COLLATE", "C")

# Use UTF-8 encoding, because the Titles contains diacritical characters 
# and the data files have been created with UTF-8 encoding.
options(encoding = "UTF-8")

ts_code <- readRDS(sprintf("tscode/tscode_%s.rds",id))

source("utils/check_ts_table.R")
source("utils/read_match_report.R")
source("utils/check_titles_and_labels.R")

raw_cbs_dir <- "raw_cbs_data"

test_that(id, {
  
  expect_silent(result1 <<- get_ts(id, ts_code, download = FALSE))
  check <- check_ts_table(result1, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)

  expected_label_file <- file.path("expected_output", 
                                   paste0(id, "_1_labels.rds"))
  expect_ts_labels_equal(ts_labels(result1$Y), expected_label_file, 
                         update = update_expected)
})


test_that(paste(id, "- errrors"), {
  
  ts_code <- readRDS(sprintf("tscode/tscode_%s_err.rds",id))
  msg <- "Topic 'EenheidVanGoederensoort_5' contains text data:\n'kg', 'TJ', '', '1000 kWh'."
  expect_warning(result2 <- get_ts(id, ts_code, download = FALSE),
               msg, fixed = TRUE)
  
  check <- check_ts_table(result2, id, raw_cbs_dir = raw_cbs_dir)
  expect_true(check)
  
  result2_m_e__bn_ <- select_columns(result2$M, "^e__bn_") 
  expect_true(all(is.na(result2_m_e__bn_)))
  result2_y_e__bn_ <- select_columns(result2$M, "^e__bn_") 
  expect_true(all(is.na(result2_y_e__bn_)))
  
  colnames_e__bn_ <- c("e__bn_elek_eu", "e__bn_elek_neu", "e__bn_elek_tot",
                         "e__bn_gas1_eu", "e__bn_gas1_neu", "e__bn_gas1_tot",
                         "e__bn_gas2_eu", "e__bn_gas2_neu", "e__bn_gas2_tot",
                         "e__bn_gas3_eu", "e__bn_gas3_neu", "e__bn_gas3_tot", 
                         "e__bn_gascond_eu", "e__bn_gascond_neu", 
                         "e__bn_gascond_tot", "e__bn_rolie_eu", 
                         "e__bn_rolie_neu","e__bn_rolie_tot")
  colnames_e__bn_ <- sort(colnames_e__bn_)
  expect_equal(sort(colnames(result2_m_e__bn_)), colnames_e__bn_)
  expect_equal(sort(colnames(result2_y_e__bn_)), colnames_e__bn_)
  
  expected_label_file <- file.path("expected_output", 
                                   paste0(id, "_2_labels.rds"))
  expect_ts_labels_equal(ts_labels(result2$Y), expected_label_file, 
                         update_expected)
  
  lbls2 <- ts_labels(result2$Y)
  lbls2 <- lbls2[!names(lbls2) %in% colnames_e__bn_]
  expect_equal(lbls2, ts_labels(result1$Y))
})

