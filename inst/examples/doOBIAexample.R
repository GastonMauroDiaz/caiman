x <- loadPhoto()
x <- normalize(x, 0, 255)
z <- makeZimage(ncol(x), lensPolyCoef(c(0.6427, 0.0346, -0.024491)))

# Because next line takes too long for an example...
\dontrun{
  seg <- doPolarQtree(x, z, scaleParameter = 0.1)
}
# ... you can open the result with the following lines.
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

path <- system.file("external/DSC_2881.jpg", package="caiman")
x <- loadPhoto(path)
### declaring it as a fullframe
fisheye(x) <- newFishEye(TRUE, TRUE, TRUE)

x <- normalize(x, 0, 255)

lens <- lensPolyCoef(c(1.13, 0.00798, -0.138))
diameter <- calcDiameter(x, lens)
r <- makeRimage(diameter, lens)
z <- makeZimage(r, lens)

# Because next line takes too long for an example...
\dontrun{
  seg <- doPolarQtree(x, z, scaleParameter = 0.5)
}
# ... you can open the result with the following lines.
seg <- raster(system.file("external/segFF.tif", package="caiman"))
seg <- as(seg, "PolarSegmentation")

\donttest{
out <- doOBIA(x, z, seg, sampleSize = 20, calibration = TRUE)
plot(out)
out <- doOBIA(x, z, seg, sampleSize = 20, calibration = FALSE)
plot(out)
}
