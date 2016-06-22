z <- makeZimage(480, lensPolyCoef())
a <- makeAimage(z)
g <- makePolarGrid(z, a, angleWidth = asAngle(10))
g
head(levels(g)[[1]])
plot(g)
