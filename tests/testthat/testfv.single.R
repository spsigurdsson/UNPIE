context("fv.single: Future value of single payment")

test_that("1 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  v = -1000
  r=0.03
  nper=30
  infl = ts(rep(0.02,30),start = 2000)
  pmt=0
  pmtinfladj = FALSE
  res = fv.single(ts(rep(0.03,30),start = 2000),0.02,nper,v)-fv.single(r,infl,nper,v)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("2 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  v = -10000000
  r=0.03
  nper=300
  infl = ts(rep(0.02,300),start = 2000)
  pmt=0
  pmtinfladj = TRUE
  res = fv.single(ts(rep(0.03,300),start = 2000),0.02,nper,v)-fv.single(r,infl,nper,v)
  expect_identical(res,ts(rep(0,300),start =2000))
})
