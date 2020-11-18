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
  expect_is(geoLocation(x), "SpatialPoints")
  expect_is(bearing(x), "Angle")
  expect_is(elevation(x), "Angle")
  expect_is(slope(x), "Angle")
  expect_is(fNumber(x), "numeric")
  expect_is(exposureTime(x), "numeric")
  expect_is(isoSpeed(x), "numeric")
})

to <- loadPhoto(paste0(path, "/UnFavAutoE3.jpg"))
from <- loadPhoto()
to <- cloneSlots(from, to)

mySlots <- slotNames(to)
index <-
  mySlots == "legend" |
  mySlots == "rotated" |
  mySlots == "rotation" |
  mySlots == "data" |
  mySlots == "file" |
  mySlots == "extent" |
  mySlots == "ncols" |
  mySlots == "nrows" |
  mySlots == "crs" | mySlots == "history" | mySlots == "z"
mySlots <- mySlots[!index]

test_that("loadPhoto() return an object of CanopyPhoto class.", {
  expect_equal(Map(function(x) slot(from, x), mySlots),
               Map(function(x) slot(to, x), mySlots))
})
