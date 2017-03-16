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
  expect_equal(res[1],-v*(1+r))
  expect_equal(res[2],-v*(1+r)^2)
  expect_equal(res[35],-v*(1+r)^35)

})

