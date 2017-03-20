test_that("1 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  fv = -10000
  r1=0.02
  r2=ts(rep(0.02,30),start = 2000)
  nper=30
  infl = ts(rep(0.02,30),start = 2000)
  res = infladj.annuity(fv,r1,infl,nper)-infladj.annuity(fv,r2,infl,nper)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("2 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  fv = ts(rep(-1000,30),start = 2000)
  r1=0.02
  r2=ts(rep(0.02,30),start = 2000)
  infl = 0.02
  nper = 30
  res = infladj.annuity(fv,r1,infl,nper)-infladj.annuity(fv,r2,infl,nper)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("3 test that start of a timeseries is set correct with no timeseries as input", {
  fv = -555
  r=0.02
  infl = 0.02
  nper = 30
  res = infladj.annuity(fv,r,infl,nper)-infladj.annuity(fv,r,infl,nper)
  expect_identical(res,ts(rep(0,30),start =1))
})
