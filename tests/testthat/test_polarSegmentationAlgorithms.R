library(caiman)
context("test polarSegmentationAlgorithms.r")

x <- loadPhoto()
z <- makeZimage(ncol(x), lensPolyCoef())
r <- makeRings(z, angleWidth = asAngle(30))

test_that("makeRings works.", {
  expect_is(r, "PolarSegmentation")
  expect_equal(levels(r)[[1]][,1], c(1, 2, 3))
})

a <- makeAimage(z)
r <- makePolarSectors(a, angleWidth = asAngle(30))

test_that("makePolarSectors works.", {
  expect_is(r, "PolarSegmentation")
  expect_equal(levels(r)[[1]][,1], 1:12)
})

a <- makeAimage(z)
r1 <- makePolarGrid(z, a, angleWidth = asAngle(30), sequential = TRUE)
r2 <- makePolarGrid(z, a, angleWidth = asAngle(30), sequential = FALSE)

test_that("makePolarGrid works.", {
  expect_is(r, "PolarSegmentation")
  expect_equal(levels(r1)[[1]][, 1], 1:36)
  expect_equal(getMax(r2), 12003)
})
