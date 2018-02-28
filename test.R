#!/usr/bin/Rscript

if (!require(devtools)) {
    stop('devtools not installed')
}
devtools::test('cbsots')
