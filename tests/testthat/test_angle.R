# library(caiman)
# context("test Angle")
#
# test_that("error messages of asAngle() work.", {
#   expect_error(asAngle(370, degrees = TRUE), "degrees")
#   expect_error(asAngle(-10, degrees = TRUE), "degrees")
#   expect_error(asAngle(360, degrees = TRUE), "degrees")
#   expect_error(asAngle(360, degrees = FALSE), "degrees")
#   expect_error(asAngle(355, degrees = FALSE), "degrees")
#   expect_error(asAngle(-10, degrees = FALSE), "degrees")
#   expect_error(asAngle(pi * 2, degrees = FALSE), "degrees")
#   expect_error(asAngle(pi * 3, degrees = FALSE), "degrees")
#   expect_error(asAngle(-pi, degrees = FALSE), "degrees")
# })
