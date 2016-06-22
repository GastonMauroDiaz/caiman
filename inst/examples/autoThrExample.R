x <- loadPhoto()
bin <- autoThr(x, 3)
bin
plot(bin)

path <- system.file("external/Black_caiman_Macrofotografie_2.jpg",
                                                              package="caiman")
bin <- autoThr(raster(path))
plot(bin)
