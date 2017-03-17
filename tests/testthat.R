## load dependencies
library(testthat)
library(unpie)
library(codecov.R)

## test package
test_check('codecov.R')
test_check("unpie")
