library(caiman)
context("test objectBasedProcessingFunctions.r")

x <- loadPhoto()
z <- makeZimage(ncol(x), lensPolyCoef())
seg <- makeRings(z, angleWidth = asAngle(30))
fi <- getFeatureImage(raster::subset(x, 3), seg, mean)
f <- extractFeatures(raster::subset(x, 3), seg, mean)
attributes(f) <- NULL

levels(as.factor(fi))[[1]][,1]

test_that("getFeatureImage works.", {
  expect_is(fi, "RasterLayer")
  expect_equal(levels(as.factor(fi))[[1]][,1], sort(f))
})
