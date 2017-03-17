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

