z <- makeZimage(480, lensPolyCoef())

r <- makeRings(z, angleWidth = asAngle(30))
r
plot(r)
r <- makeRings(z, angleWidth = asAngle(30), angleMean = TRUE)
r
plot(r)

r <- makeRings(z, angleWidth = asAngle(10))
plot(r)
