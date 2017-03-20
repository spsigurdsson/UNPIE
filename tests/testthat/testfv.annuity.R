test_that("1 test that start of a timeseries is set correct with timeseries on inflation but not rate and pmt", {
  v = 0
  r1=0.02
  r2=ts(rep(0.02,30),start = 2000)
  nper=30
  infl = ts(rep(0.02,30),start = 2000)
  pmt=-1000
  pmtinfladj = FALSE
  res = fv.annuity(r1,infl,nper,pmt,pmtinfladj,pmtUltimo = FALSE)-fv.annuity(r2,infl,nper,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("2 test that start of a timeseries is set correct with timeseries on rate and rate but not inflation and pmt", {
  v = 0
  r1=ts(rep(0.02,30),start = 2000)
  r2=0.02
  nper=30
  infl1 = 0.02
  infl2 = ts(rep(0.02,30),start = 2000)
  pmt=-1000
  pmtinfladj = FALSE
  res = fv.annuity(r1,infl1,nper,pmt,pmtinfladj,pmtUltimo = FALSE)-fv.annuity(r2,infl2,nper,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("3 test that start of a timeseries is set correct with no timeseries given as input", {
  v = 0
  r=0.02
  nper=30
  infl = 0.02
  pmt=-1000
  pmtinfladj = FALSE
  res = fv.annuity(r,infl,nper,pmt,pmtinfladj,pmtUltimo = FALSE)-fv.annuity(r,infl,nper,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_identical(res,ts(rep(0,30),start =1))
})
