x <- loadPhoto()
x <- normalize(x, 0, 255)
xe <- enhanceHemiPhoto(x)
plot(xe)

z <- makeZimage(ncol(x), lensPolyCoef(c(0.6427, 0.0346, -0.024491)))

m <- doMask(z, zlim = asAngle(c(30,60)), alim = asAngle(c(0, 45)))
# mask affects the result but does not crop it (see Details).
plot(enhanceHemiPhoto(x, mask = m))

# Making an artificiall fullframe hemiphoto by cropping a non-fullframe
# hemiphoto (just as an example)
path <- system.file("external/UnFavAutoE3.jpg", package="caiman")
x <- loadPhoto(path, upperLeft = c(33, 120), width = 414, height = 240)
## declaring it as a fullframe
fisheye(x) <- newFishEye(TRUE, TRUE, TRUE)

x <- normalize(x, 0, 255)
xe <- enhanceHemiPhoto(x)
plot(xe)

# Using a mask for fullframe photos is tricky (see Details).
m <- doMask(x)
m <- doMask(z, previousMask = m, zlim = asAngle(c(30, 40)),
                                                      alim = asAngle(c(20,130)))
plot(m)
plot(enhanceHemiPhoto(x, mask = m) - enhanceHemiPhoto(x))
