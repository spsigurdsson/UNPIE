context("Test of  inflation adjusted future value of cashflow with (constant) annuity payments")

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
  res = infladj.annuity(fv,rate=r1,inflation=infl,nper)-infladj.annuity(fv,r2,infl,nper)
  expect_identical(res,ts(rep(0,30),start =2000))
  })

test_that("3 test that start of a timeseries is set correct with no timeseries as input.", {
    fv = -555
    r=0.02
    infl = 0.02
    nper = 30
    res = infladj.annuity(fv,r,infl,nper)-infladj.annuity(fv,r,infl,nper)
    expect_identical(res,ts(rep(0,30),start =1))
})

test_that("4 no ts input", {
  A = infladj.annuity(fv=1000,rate=0.01, inflation=0, nper=10)
  expect_equal(A,ts(c(990.09900990099004, 980.29604940692093, 970.59014792764435, 960.98034448281624,
                      951.46568760674870, 942.04523525420666, 932.71805470713537, 923.48322248231216,
                      914.33982423991313, 905.28695469298327)))
})

test_that("5 test that start of a timeseries is set correct with r as only ts input", {
  fv = -555
  r=ts(rep(0.02,30),start = 2000)
  infl = 0.02
  nper = 30
  res = infladj.annuity(fv,r,infl,nper)-infladj.annuity(fv,r,infl,nper)
  expect_identical(res,ts(rep(0,30),start =2000))
})
