x <- loadPhoto()
x <- normalize(x, 0, 255)
z <- makeZimage(ncol(x), lensPolyCoef())
m <- doMask(z)

outOfDR(x, mask = m)
plot(outOfDR(x, mask = m,  returnImages = TRUE))

outOfDR(x, "Blue", mask = m)
plot(outOfDR(x, "Blue", mask = m,  returnImages = TRUE))
