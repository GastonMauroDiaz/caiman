sRGB2LAB(c(0.5,0.7,0.5))
x <- sRGB2LAB(matrix(c(normalize(1:90, 1, 90)), ncol = 3))
str(x)
head(x)

x <- loadPhoto()
x <- normalize(x, 0, 255)
x <- sRGB2LAB(x)
x
plot(x)

x <- system.file("external/Black_caiman_Macrofotografie_2.jpg", package="caiman")
x <- loadPhoto(x)
plotRGB(x)
x <- normalize(x, 0, 255)
x <- sRGB2LAB(x)
plot(x)
