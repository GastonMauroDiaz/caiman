library(caiman)
context("test perPixelProcessingFunctions.r")

x <- loadPhoto()
x <- presetThr(raster::subset(x, 3), 100)

test_that("presetThr works.", {
  expect_is(x, "BinImage")
  expect_equal(getMax(x), 1)
  expect_equal(getMin(x), 0)
})

x <- loadPhoto()
x <- autoThr(x)

test_that("autoThr works.", {
  expect_is(x, "BinImage")
  expect_equal(getMax(x), 1)
  expect_equal(getMin(x), 0)
})


path <- sub("inst", "" , system.file(package = "caiman"))
x <- brick(paste0(path, "tests/file.tif"))
x <- normalize(x, 0, getMax(x)[1])

test_that("normalize works whit Raster.", {
  expect_is(x, "RasterBrick")
  expect_equal(getMax(x), c(1, 1, 1))
  expect_equal(getMin(x), c(0, 0, 0))
})

x <- raster(paste0(path, "/tests/file.tif"))
x <- normalize(x, 0, getMax(x))

test_that("normalize works.", {
  expect_is(x, "RasterLayer")
  expect_equal(getMax(x), 1)
  expect_equal(getMin(x), 0)
})

x <- loadPhoto()
x <- normalize(x, 0, 255)

test_that("normalize works whit CanopyPhoto.", {
  expect_is(x, "CanopyPhoto")
  expect_equal(getMax(x), c(1, 1, 1))
  expect_equal(getMin(x), c(0, 0, 0))
})

lab <- sRGB2LAB(x)
round(getMax(lab),2)
test_that("sRGB2LAB works whit CanopyPhoto.", {
  expect_is(lab, "CanopyPhoto")
  expect_equal(lab@data@names, c("L", "A", "B"))
  expect_equal(round(getMax(lab), 2), c(100.00, 41.62, 54.25))
})

ma <- rbind(c(0.5, 0.2, 0.1), c(0.1, 0.2, 0.5))
lab <- sRGB2LAB(ma)

test_that("sRGB2LAB works whit numeric and matrix.", {
  expect_is(sRGB2LAB(c(0.5,0.5,0.5)), "matrix")
  expect_equal(as.numeric(round(sRGB2LAB(c(0.5,0.5,0.5)), 2)), c(53.39, 0, 0))
  expect_is(lab, "matrix")
})

# x <- loadPhoto()
# x <- normalize(x, 0, 255)
a <- membership2color(x, targetColor = sRGB(1,0,0))
b <- membership2color(sRGB(0.5,0.5, 0.5), targetColor = sRGB(1,0,0))
attributes(b) <- NULL
test_that("membership2color works.", {
  expect_is(a, "RasterLayer")
  expect_equal(round(getMax(a), 2), 0.83)
  expect_is(b, "numeric")
  expect_equal(round(b, 2), 0.61)
})

