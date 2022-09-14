# Use devtools::test_dir to run the testset.
# In contrast devtools::cbsots, this will use the installed version of the
# package and not the local source of the package.

library(cbsots)
testthat::test_dir("cbsots/tests/testthat")
