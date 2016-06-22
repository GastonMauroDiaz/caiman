library(caiman)
context("test prototypes")

test_that("prototypes pass validObject()", {
  expect_true(validObject(new("LensPolyCoef")))
  expect_true(validObject(new("Angle")))
  expect_true(validObject(new("FishEye")))
  expect_true(validObject(new("CanopyPhoto")))
})
