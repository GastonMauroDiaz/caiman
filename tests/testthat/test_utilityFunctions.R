library(caiman)
context("test utilityFunctions.r")

x <- loadPhoto()

test_that("calcExposure works.", {
  expect_equal(round(calcExposure(8,1/125)), 13)
  expect_equal(round(calcExposure(loadPhoto()), 2), 9.88)
})
