x <- loadPhoto()
z <- makeZimage(ncol(x), lensPolyCoef(c(0.9192, -0.1792, -0.000443)))
m <- doMask(z, zlim = asAngle(c(30, 60)))
plot(m)

m <- doMask(z, zlim = asAngle(c(30, 60)), alim = asAngle(c(0, 90)))
plot(m)

plot(x[m, drop = FALSE])

# for fullframe hemispherical photos
path <- system.file("external/DSC_2881.jpg", package="caiman")
x <- loadPhoto(path)
### declaring it as a fullframe
fisheye(x) <- newFishEye(TRUE, TRUE, TRUE)

m <- doMask(x, z)
plot(m)

m <- doMask(z, previousMask = m, zlim = asAngle(c(30, 60)),
                                                    alim = asAngle(c(0, 90)))
plot(m)
