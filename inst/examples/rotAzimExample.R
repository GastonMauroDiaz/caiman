z <- makeZimage(480, lensPolyCoef())
a <- makeAimage(z)
plot(a)
a
a <- rotAzim(a, asAngle(35))
plot(a)
a
