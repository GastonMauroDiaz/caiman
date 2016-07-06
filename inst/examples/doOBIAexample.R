x <- loadPhoto()
x <- normalize(x, 0, 255)
z <- makeZimage(ncol(x), lensPolyCoef(c(0.6427, 0.0346, -0.024491)))

# Because next line takes too long for an example...
\dontrun{
  seg <- doPolarQtree(x, z, scaleParameter = 0.1)
}
# ... you can open the result whit the next lines.
seg <- raster(system.file("external/seg.tif", package="caiman"))
seg <- as(seg, "PolarSegmentation")

g1 <- makePolarGrid(z)
zlim <- asAngle(c(20, 80))
\donttest{
out <- doOBIA(x, z, seg, g1, zlim = zlim, sampleSize = 20, calibration = FALSE)
plot(out)
out <- doOBIA(x, z, seg, g1, zlim = zlim, sampleSize = 20, calibration = TRUE)
plot(out)

zlim <- asAngle(c(30, 60))

out <- doOBIA(x, z, seg, g1, zlim = zlim, sampleSize = 20, calibration = TRUE)
plot(out)
}


# Fullframe

## Making an artificially fullframe hemiphoto by cropping a non-fullframe
## hemiphoto (just for making an example).
path <- system.file("external/UnFavAutoE3.jpg", package="caiman")
x <- loadPhoto(path, upperLeft = c(33, 120), width = 414, height = 240)
### declaring it as a fullframe
fisheye(x) <- newFishEye(TRUE, TRUE, TRUE)

x <- normalize(x, 0, 255)

\dontrun{
  seg <- doPolarQtree(x, z, scaleParameter = 0.1)
}
### It is not exactly the same, but you can use the previous seg.
\donttest{
out <- doOBIA(x, z, seg, sampleSize = 20, calibration = TRUE)
plot(out)
out <- doOBIA(x, z, seg, sampleSize = 20, calibration = FALSE)
plot(out)
}
