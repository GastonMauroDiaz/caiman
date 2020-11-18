library(caiman)
context("test source geometryFunctions.r")

test_that("error messages of calcR() work.", {
  expect_error(calcR(), "missing")
  expect_error(calcR(asAngle(pi/4), y="some string"), "character")
  expect_error(calcR("300"), "unable to")
  expect_error(calcR(asAngle(99, degrees = TRUE), y = lensPolyCoef()), "less than 90 degrees")
  expect_error(calcR(asAngle(pi / 2 + pi, degrees = FALSE), y = lensPolyCoef()), "less than 90 degrees")
})

test_that("error messages of makeRimage work.", {
  expect_error(makeRimage(555), "must be an even integer")
  expect_error(new("RelativeRadiusImage", diameter = as.integer(555)), "must be an even integer")
})


test_that("makeRimage work.", {
  a <- values(makeRimage(596))
  expect_equal(round(max(a, na.rm = TRUE)), 1)
  expect_gt(min(a, na.rm = TRUE), 0)
})

test_that("calcR() make good calcs.", {
  expect_equal(calcR(asAngle(90), y=lensPolyCoef()), 1)
  expect_equal(calcR(asAngle(0), y=lensPolyCoef()), 0)
})

test_that("makeZimage work.", {
  a <- values(makeZimage(596, lensPolyCoef()))
  expect_equal(max(a, na.rm = TRUE), 90)
  expect_gt(min(a, na.rm = TRUE), 0.3)
  expect_lt(min(a, na.rm = TRUE), 0.4)
})

test_that("makeAimage work.", {
  z <- makeZimage(596, lensPolyCoef())
  a <- values(makeAimage(z))
  max(a, na.rm = TRUE)
  expect_gt(max(a, na.rm = TRUE), 359) # getMax()
  expect_lt(max(a, na.rm = TRUE), 360)
})
