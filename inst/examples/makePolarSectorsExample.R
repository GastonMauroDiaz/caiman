z <- makeZimage(480, lensPolyCoef())
a <- makeAimage(z)

s <- makePolarSectors(a, angleWidth = asAngle(10))
s
plot(s)
s <- makePolarSectors(a, angleWidth = asAngle(10), angleMean = TRUE)
s
plot(s)

s <- makePolarSectors(a, angleWidth = asAngle(15))
s
plot(s)
