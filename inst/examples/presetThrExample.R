x <- loadPhoto()
x <- raster::subset(x, 3)
bin <- presetThr(x, 125)
bin
plot(bin)
