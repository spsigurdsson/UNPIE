context("pmt: Test calculations of annuity payments")

test_that("1 test that the function calculates the true annuity", {
  rate=0.04
  inflation=.02
  nper=35
  fv=1000
  a=pmt(rate,inflation,nper,fv)
  expect_identical(a,ts(rep(39.756610937342266,35)))
})

test_that("2 test that the function calculates the true annuity when rate is given as ts", {
  rate=ts(0.04,start = 200, end = 205)
  inflation=.02
  nper=35
  fv=1000
  a=pmt(rate,inflation,nper,fv)
  expect_identical(a,ts(rep(39.756610937342266,6),start=200))
})

test_that("3 test that the function calculates the true annuity when inflation is given as ts", {
  rate=0.04
  inflation=ts(0.02,start = 12200, end = 12205)
  nper=35
  fv=1000
  a=pmt(rate,inflation,nper,fv)
  expect_identical(a,ts(rep(39.756610937342266,6),start=12200))
})

