#!/usr/bin/Rscript
if (!require(devtools)) {
  stop('devtools not installed')
}
devtools::install_deps('cbsots', dependencies = TRUE,
		       upgrade = "never")


