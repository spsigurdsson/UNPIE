library(unpie)


context("fv: Future value")

test_that("01 fv test: init test", {

  r = fv(pv=1000,nper=35,rate=0.04)
  expect_equal(r[35],-3946.089)
  expect_equal(length(r),35)
})

