test_that("1 test rate.converts from annual to monthly, and back againg still give the same result", {
  rate=0.04
  new.rate = rate.convert(rate,frequency.input=1,frequency.output=12)
  old.rate = rate.convert(new.rate,frequency.input=12,frequency.output=1)
  expect_equal(rate, old.rate)
})

test_that("2 test rate.converts from annual to monthly", {
  rate=0.04
  method.rate = rate.convert(rate,frequency.input=1,frequency.output=12)
  calc.rate = (1+rate)^(1/12)-1
  expect_equal(method.rate, calc.rate)
})

test_that("3 test rate.converts from quaterly to annual", {
  rate=0.04
  method.rate = rate.convert(rate,frequency.input=4,frequency.output=1)
  calc.rate = (1+rate)^(4/1)-1
  expect_equal(method.rate, calc.rate)
})

test_that("4 test rate.converts returns error when rate is not of type scalar", {
  rate=c(1,1)
  expect_error(rate.convert(rate,frequency.input=4,frequency.output=1), "rate must be of type scalar", fixed=TRUE)
})
