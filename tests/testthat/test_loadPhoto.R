library(caiman)
context("test CanopyPhoto object")

path <- system.file("external", package="caiman")


test_that("loadPhoto() return an object of CanopyPhoto class.", {
  expect_is(loadPhoto(paste0(path, "/UnFavAutoE3.jpg")), "CanopyPhoto")
  expect_is(loadPhoto(paste0(path, "/UnFavAutoE3.jpg"), upperLeft = c(48, 18),
                                  width = 159, height = 159), "CanopyPhoto")
})

test_that("error messages of loadPhoto() work.", {
  expect_error(loadPhoto(""), "Provide")
  expect_error(
    loadPhoto(paste0(path, "/seg.tif"))
    , "layer")
  expect_error(loadPhoto(paste0(path, "/tests/file2.tif")), "not")
})

x <- loadPhoto()

test_that("Test loadPhotoFunctions.", {
  expect_is(equipment(x), "character")
  expect_is(fisheye(x), "FishEye")
  expect_is(newFishEye(TRUE, TRUE, TRUE), "FishEye")
  expect_is(datetime(x), "character")
  expect_is(geocode(x), "SpatialPoints")
  expect_is(bearing(x), "Angle")
  expect_is(elevation(x), "Angle")
})

to <- loadPhoto(paste0(path, "/UnFavAutoE3.jpg"))
from <- loadPhoto()
to <- cloneSlots(from, to)
test_that("loadPhoto() return an object of CanopyPhoto class.", {
  expect_equal(equipment(from), equipment(to))
  expect_equal(fisheye(from), fisheye(to))
  expect_equal(datetime(from), datetime(to))
  expect_equal(geocode(from), geocode(to))
  expect_equal(bearing(from), bearing(to))
  expect_equal(elevation(from), elevation(to))
})
