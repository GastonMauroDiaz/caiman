x <- loadPhoto()
z <- makeZimage(ncol(x), lensPolyCoef(c(0.9192, -0.1792, -0.000443)))
m <- doMask(z, zlim = asAngle(c(30, 60)))
plot(m)

m <- doMask(z, zlim = asAngle(c(30, 60)), alim = asAngle(c(0, 90)))
plot(m)

plot(x[m, drop = FALSE])
