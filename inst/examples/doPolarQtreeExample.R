x <- loadPhoto()
z <- makeZimage(ncol(x), lensPolyCoef(c(0.6427, 0.0346, -0.024491)))
a <- makeAimage(z)
x <- normalize(x, 0, 255)
\donttest{
seg <- doPolarQtree(x, z, a, scaleParameter = 1)
seg
plot(seg)
}
# Next lines open a result obtained with scaleParameter = 0.1, which takes
# too long for an example.
seg <- raster(system.file("external/seg.tif", package="caiman"))
seg <- as(seg, "PolarSegmentation")
plot(seg)
