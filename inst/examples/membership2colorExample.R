x <- loadPhoto()
x <- normalize(x, 0, 255)
targetColor <- colorspace::sRGB(0.3, 1, 0.2)
plot(targetColor)
x <- membership2color(x, targetColor)
plot(x, col = grey((1:255) / 255))

x <- system.file("external/Black_caiman_Macrofotografie_2.jpg", package="caiman")
x <- loadPhoto(x)
plot(x)
x <- normalize(x, 0, 255)
y <- membership2color(x, targetColor)
plot(y, col = grey((1:255) / 255))
index <- cellFromCol(x, 200:350)
x[index] <- y[index]
plotRGB(x * 255)

x <- membership2color(sRGB(0.5,0.5, 0.5), targetColor = targetColor)
x
attr(x, "sigma")
attr(x, "targetColor")
# if attributes get in your way...
attributes(x) <- NULL
x
