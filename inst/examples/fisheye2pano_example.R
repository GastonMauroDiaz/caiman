require(caiman)
hcp <- loadPhoto()
z <- makeZimage(ncol(hcp), lensPolyCoef(c(0.6427, 0.0346, -0.024491)))
a <- makeAimage(z)
m <- doMask(z, zlim = asAngle(c(10,80)))

plotRGB(fisheye2pano(hcp, z, a, m))



