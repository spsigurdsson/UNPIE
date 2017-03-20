library(unpie)
# devtools:test()

context("fv: Future value")

test_that("01 fv test: init test", {

  r = fv(pv=1000,nper=35,rate=0.04)
  expect_equal(r[35],-3946.089)
  expect_equal(length(r),35)
})

test_that("02 fv test: init test", {
  v = 1000
  r = 0.06
  res = fv(pv=v,nper=35,rate=r)
  expect_identical(res[1],-v*(1+r))
  expect_identical(res[2],-v*(1+r)^2)
  expect_identical(res[35],-v*(1+r)^35)
  expect_identical(start(res),c(1,1))
  expect_identical(end(res),c(35,1))
  expect_identical(frequency(res),1)
})

test_that("03 fv & fv.single are aligned when input is pv, nper and rate and all are scalars", {
  v = -1000
  r = 0.06
  res = fv(pv=v,nper=35,rate=r)==fv.single(pv=v,nper=35,rate=r)
  expect_identical(res,as.ts(rep(TRUE,35),start =1,frequency=1))
})

test_that("04 fv & fv.single are aligned when input is pv (scalar), nper(scalar) and rate (ts with shorter period)", {
  v = -1000
  r = ts((1:30)/100,start =2000)
  res = fv(pv=v,nper=35,rate=r)==fv.single(pv=v,nper=35,rate = r)
  expect_identical(res,ts(rep(TRUE,30),start =2000))
  expect_identical(end(res),c(2029, 1))
})

test_that("05 fv & fv.single are aligned when input is pv (scalar), nper(scalar) and rate (ts with longer period)", {
  v = -1000
  r = ts((1:40)/100,start =2000)
  res = fv(pv=v,nper=35,rate=r)==fv.single(pv=v,nper=35,rate = r)
  expect_identical(res,ts(rep(TRUE,35),start =2000))
  expect_identical(end(res),c(2034, 1))
})

test_that("06 testing that primo/ultimo does not affect result when there is no payment", {
  v = -1000
  r=0.04
  nper=30
  infl = 0
  pmt=0
  pmtinfladj = FALSE
  res = fv(r,infl,nper,v,pmt,pmtinfladj,FALSE)-fv(r,infl,nper,v,pmt,pmtinfladj,TRUE)
  expect_identical(res,ts(rep(0,30),start =1))
})

test_that("07 testing that primo/ultimo does not affect result when there is no payment but inflation", {
  v = -1000
  r=0.04
  nper=30
  infl = 0.02
  pmt=0
  pmtinfladj = FALSE
  res = fv(r,infl,nper,v,pmt,pmtinfladj,TRUE)-fv(r,infl,nper,v,pmt,pmtinfladj,FALSE)
  expect_identical(res,ts(rep(0,30),start =1))
})

test_that("08 testing ultimo on payment", {
  v = 0
  r=0.04
  nper=35
  infl = 0
  pmt=-1000
  pmtinfladj = FALSE
  res = fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_equal(res[1],1040)
  expect_equal(res[10],12486.351407877000)
  expect_equal(res[35],76598.31384951049)

})

test_that("09 testing primo on payment", {
  v = 0
  r=0.04
  nper=35
  infl = 0
  pmt=-1000
  pmtinfladj = FALSE
  res = fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = TRUE)
  expect_equal(res[1],1000)
  expect_equal(res[10],12006.107122958600)
  expect_equal(res[35],73652.224855298600)

})

test_that("10 testing resutt with pv and pmt specified and pmt = ultimo ", {
  v = -10000
  r=0.04
  nper=35
  infl = 0
  pmt=-1000
  pmtinfladj = FALSE
  res = fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = TRUE)
  expect_equal(res[1],11400.000000000000)
  expect_equal(res[10],26808.549972142100)
  expect_equal(res[35],113113.114797418000)

})

test_that("11 testing resutt with pv and pmt specified and pmt = primo ", {
  v = -10000
  r=0.04
  nper=35
  infl = 0
  pmt=-1000
  pmtinfladj = FALSE
  res = fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_equal(res[1],11440.000000000000)
  expect_equal(res[10],27288.794257060400)
  expect_equal(res[35],116059.203791630000)
})

test_that("12 test that start of a timeseries is set correct with timeseries on pmt but not rate", {
  v = 0
  r1=0.02
  r2=ts(rep(0.02,30),start = 2000)
  nper=30
  infl = 0
  pmt=ts(rep(-1000,30),start =2000)
  pmtinfladj = FALSE
  res = fv(r1,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)-fv(r2,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("13 test that start of a timeseries is set correct with timeseries on inflation but not rate and pmt", {
  v = 0
  r1=0.02
  r2=ts(rep(0.02,30),start = 2000)
  nper=30
  infl = ts(rep(0.02,30),start = 2000)
  pmt=-1000
  pmtinfladj = FALSE
  res = fv(r1,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)-fv(r2,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("14 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  v = -1000
  r=0.02
  nper=30
  infl = ts(rep(0.02,30),start = 2000)
  pmt=0
  pmtinfladj = FALSE
  res = fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)-fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("15 test that start of a timeseries is set correct with timeseries on inflation but not rate with frequency = monthly", {
  v = -1000
  r=rate.convert(0.04,1,12)
  nper=30*12
  infl = ts(rep(0.002,30*12),start = 2000, frequency=12)
  pmt=0
  pmtinfladj = FALSE
  res = fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)-fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_identical(res,ts(rep(0,30*12),start =2000,frequency = 12))
})

test_that("16 test frequency = monthly", {
  v = -1000
  r=rate.convert(0.04,1,12)
  nper=35*12
  infl = ts(rep(rate.convert(0.02,1,12),35*12),start = 2000, frequency=12)
  pmt=0
  pmtinfladj = FALSE
  res=fv(r,infl,nper,v,pmt,pmtinfladj,pmtUltimo = FALSE)
  expect_equal(res[35*12],1973.153461879180)
})
