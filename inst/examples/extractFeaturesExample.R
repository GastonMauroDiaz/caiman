x <- loadPhoto()
z <- makeZimage(ncol(x), lensPolyCoef(c(0.9192, -0.1792, -0.000443)))
r <- makeRings(z, angleWidth = asAngle(30))
fe <- extractFeatures(raster::subset(x, 3), r, sd)
class(fe)
str(fe)
fe
names(fe)
unname(fe)
