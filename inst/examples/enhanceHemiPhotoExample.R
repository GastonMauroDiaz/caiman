x <- loadPhoto()
x <- normalize(x, 0, 255)
x <- enhanceHemiPhoto(x)
plot(x)
