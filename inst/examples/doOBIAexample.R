x <- loadPhoto()
x <- normalize(x, 0, 255)
z <- makeZimage(ncol(x), lensPolyCoef(c(0.6427, 0.0346, -0.024491)))
m <- doMask(z)
bin <- autoThr(enhanceHP(x, m, sharpen = FALSE))


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
out <- doOBIA(x, bin, z, seg, g1, zlim = zlim, sampleSize = 20, calibration = FALSE)
plot(out)
out <- doOBIA(x, bin, z, seg, g1, zlim = zlim, sampleSize = 20, calibration = TRUE)
plot(out)

zlim <- asAngle(c(30, 60))

out <- doOBIA(x, bin, z, seg, g1, zlim = zlim, sampleSize = 20, calibration = TRUE)
plot(out)
}


# Fullframe

path <- system.file("external/DSC_2881.jpg", package="caiman")
x <- loadPhoto(path)
### declaring it as a fullframe
fisheye(x) <- newFishEye(TRUE, TRUE, TRUE)

x <- normalize(x, 0, 255)

lens <- lensPolyCoef(c(0.71553, 0.01146, -0.03928))
angle <- asAngle(c(53))
pix <- c(224)
diameter <- calcDiameter(lens, pix, angle)
z <- makeZimage(diameter, lens)

m <- doMask(x, z)
x <- expandFullframe(x, z)

bin <- autoThr(enhanceHP(x, m, sharpen = FALSE))

\donttest{
seg <- doPolarQtree(x, z, scaleParameter = 0.2, mnSize = 1000)
out <- doOBIA(x, bin, z, seg, sampleSize = 20, calibration = TRUE)
plot(out)
out <- doOBIA(x, z, seg, sampleSize = 20, calibration = FALSE)
plot(out)
}
