context("Test of required specs are fulfilled")

test_that("01 FV single payment nominal", {
  A = fv(rate=0.04,inflation=.00, nper=35,pv=-1000,pmt=0,pmtinfladj=FALSE, pmtUltimo=TRUE)
  expect_equal(A[35],3946.08899421194)
})

test_that("02 FV single payment adj. for infl", {
  A = fv(rate=0.04,inflation=.00, nper=35,pv=-1000,pmt=0,pmtinfladj=FALSE, pmtUltimo=TRUE)
  B = fv(rate=0.04,inflation=.02, nper=35,pv=-1000,pmt=0,pmtinfladj=FALSE, pmtUltimo=TRUE)
  a=fv.single(0.04,0,35,-1000)
  b=fv.single(0.04,0.02,35,-1000)

  expect_equal(A[35],3946.08899421194)
  expect_equal(B[35],1973.15346187919)
  expect_equal(B,infladj.single(a,0.02))
  expect_equal(B,b)
})

test_that("03 FV annuity nominal", {
  A = fv(rate=0.04,inflation=.0, nper=35,pv=0,pmt=-1000,pmtinfladj=FALSE, pmtUltimo=TRUE)
  B = fv.annuity(rate=0.04,inflation=.0, nper=35,pmt=-1000,pmtinfladj=FALSE, pmtUltimo=TRUE)
  expect_equal(A[35],73652.2248552986)
  expect_equal(B[35],73652.2248552986)

})

test_that("04 FV const. ann. adj. for infl.", {
  A = fv(rate=0.04,inflation=0.02, nper=35,pv=0,pmt=-1000,pmtinfladj=FALSE, pmtUltimo=TRUE)
  B = fv.annuity(rate=0.04,inflation=.02, nper=35,pmt=-1000,pmtinfladj=FALSE, pmtUltimo=TRUE)
  expect_equal(A[35],37327.1650067979)
  expect_equal(B[35],37327.1650067979)
})

test_that("05 FV real ann. adj. for infl.", {
  A = fv(rate=0.04,inflation=.02, nper=35,pv=0,pmt=-1000,pmtinfladj=TRUE, pmtUltimo=TRUE)
  B = fv.annuity(rate=0.04,inflation=.02, nper=35,pmt=-1000,pmtinfladj=TRUE, pmtUltimo=TRUE)

  #FV(payments), not adj. for inflation
  pmt_infladj=fv.single(0.02,0,35,-1000/1.02)
  a=fv.annuity(rate=0.04,inflation=0.0, nper=35,pmt=-pmt_infladj,pmtinfladj=FALSE, pmtUltimo=TRUE)

  expect_equal(A[35],49630.82656) #FV(payments), adj. for inflation
  expect_equal(B[35],49630.82656) #FV(payments), adj. for inflation
  expect_equal(a[35],97309.9720774742) #FV(payments), not adj. for inflation
})



