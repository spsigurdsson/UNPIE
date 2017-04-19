context("Test the present value of an single payment (pv)")

test_that("1 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  fv = -10000
  r1=0.02
  r2=ts(rep(0.02,30),start = 2000)
  nper=30
  infl = ts(rep(0.02,30),start = 2000)
  res = pv.single(r1,infl,nper,fv)-pv.single(r2,infl,nper,fv)
  expect_identical(res,ts(rep(0,30),start =2000))
})

test_that("2 test that start of a timeseries is set correct with timeseries on inflation but not rate", {
  fv = -123654
  r1=0.02
  nper=30
  infl = ts(rep(0.02,30),start = 2000)
  res = pv.single(r1,infl,nper,fv)
  true= ts(c(118852.364475201830,114237.182309882584,109801.213292851375,105537.498359142046,
             101439.348672762455,97500.335133374130,93714.278290440328,90075.238649019928,
             86577.507351999157,83215.597224143741,79984.234163921312,76878.348869589885,
             73893.068886572350,71023.710963641250,68265.773705921994,65614.930513189145,
             63067.022792377109,60618.053433657347,58264.180539847519,56001.711399315180,
             53827.096692921165,51736.924925914223,49727.917076042118,47796.921449482994,
             45940.908736527294,44156.967259253455,42442.298403742265,40794.212229663841,
             39210.123250349716,37687.546376729828),start=2000)
  expect_identical(res,true)
})

