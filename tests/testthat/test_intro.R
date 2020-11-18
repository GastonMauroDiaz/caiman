library(caiman)
context("test intro")

path <- system.file("external", package="caiman")
my_file <- paste0(path, '/19.JPG')

if (file.exists(my_file)) {
  download.file("https://osf.io/zh5md/download", my_file, method = "auto", mode = "wb")
}
setwd(path)

test_that("reading a photo creates an object that represents a color image.", {
  expect_is(cp <- loadPhoto('19.JPG'), "CanopyPhoto")
  expect_equal(nlayers(cp), 3)
})

cp <- loadPhoto('19.JPG', upperLeft = c(534,237), width = 1530, height = 1476)
test_that("a ROI from a photo can be read correctly", {
  expect_is(cp, "CanopyPhoto")
  expect_equal(ncell(cp), 1530 * 1476)
  expect_equal(as.numeric(cp[1]), c(204,194,145))
})

test_that("ISODATA is the default algorithm and it works properly", {
  expect_equal(round(autoThr(cp$Blue[])), 135)
})


test_that("a good relative radius raster can be created") {
  expect_error(makeRimage(555), "must be an even integer")
  r <- makeRimage(750)
  expect_equal()
}
