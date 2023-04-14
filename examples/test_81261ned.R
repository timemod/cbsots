#cpblib::use_cpblib()
library(cbsots)

rm(list = ls())

id <- "81261ned"
ts_code_file <- sprintf("tscode/tscode_%s.rds", id)

#edit_ts_code(ts_code_file)



ts_code <- readRDS(ts_code_file)

ts_code$`81261ned`$codes$MineraleBrandstoffenEnChemie$Select <- TRUE
ts_code$`81261ned`$codes$MineraleBrandstoffenEnChemie$Code <-  
  ts_code$`81261ned`$codes$MineraleBrandstoffenEnChemie$Key

ts_code$`81261ned`$codes$MineraleBrandstoffenEnChemie$Select[1:2405] <- FALSE

# door gebruik te maken van dez e code kunnen we checken
x <- get_ts(id, ts_code, frequencies = "Y", download = TRUE, min_year = 2022)