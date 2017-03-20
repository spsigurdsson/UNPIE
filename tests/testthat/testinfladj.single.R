test_that("1 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  fv = ts(rep(-1000,30),start = 2000)
  nper=30
  infl = 0.02
  res = infladj.single(fv,infl,nper)-infladj.single(fv,infl,nper)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("2 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  fv = ts(rep(-1000,30),start = 2000)
  infl1=0.02
  infl2=ts(rep(0.02,30),start = 2000)
  nper = 30
  res = infladj.single(fv,infl1,nper)-infladj.single(fv,infl2,nper)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("3 test that start of a timeseries is set correct with no timeseries as input", {
  fv = -555
  infl = 0.02
  nper = 30
  res = infladj.single(fv,infl,nper)-infladj.single(fv,infl,nper)
  expect_identical(res,ts(rep(0,30),start =1))
})
