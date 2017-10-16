# for circular hemispherical photos
x <- loadPhoto()
x <- normalize(x, 0, 255)
xe <- enhanceHemiPhoto(x)
plot(xe)

z <- makeZimage(ncol(x), lensPolyCoef(c(0.6427, 0.0346, -0.024491)))

m <- doMask(z, zlim = asAngle(c(30,60)), alim = asAngle(c(0, 45)))
# mask affects the result but does not crop it (see Details).
plot(enhanceHemiPhoto(x, mask = m))


# for fullframe hemispherical photos

path <- system.file("external/DSC_2881.jpg", package="caiman")
x <- loadPhoto(path)
## declaring it as a fullframe
fisheye(x) <- newFishEye(TRUE, TRUE, TRUE)

x <- normalize(x, 0, 255)

lens <- lensPolyCoef(c(1.13, 0.00798, -0.138))
diameter <- calcDiameter(x, lens)
r <- makeRimage(diameter, lens)
z <- makeZimage(r, lens)

xe <- enhanceHemiPhoto(x, z = z)
plot(xe)

## Using a mask for fullframe photos is tricky (see Details).
m <- doMask(x, z)
m <- doMask(z, previousMask = m, zlim = asAngle(c(30, 40)),
                                                      alim = asAngle(c(20,130)))
plot(m)
plot(enhanceHemiPhoto(x, z = z, mask = m) - enhanceHemiPhoto(x, z = z))
