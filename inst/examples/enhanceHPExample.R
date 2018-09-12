# for circular hemispherical photos
x <- loadPhoto()
x <- normalize(x, 0, 255)
xe <- enhanceHP(x)
plot(xe)

z <- makeZimage(ncol(x), lensPolyCoef(c(0.6427, 0.0346, -0.024491)))

m <- doMask(z, zlim = asAngle(c(30,60)), alim = asAngle(c(0, 45)))
# mask affects the result but does not crop it (see Details).
plot(enhanceHP(x, mask = m))
