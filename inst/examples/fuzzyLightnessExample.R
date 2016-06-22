x <- raster(ncol = 100, nrow = 100)
extent(x) <- c(0, 100, 0, 100)
values(x) <- rep(1:ncol(x), nrow(x))
m <- t(x / getMax(x))
plot(x)
plot(m)

r <- x

values(r) <- fuzzyLightness(values(x), values(m), 45, 10)
plot(r, col = grey(1:100 / 100), ylab = "m * 100", xlab = "Lightness")

values(r) <- fuzzyLightness(values(x), values(m), 45, 50)
plot(r, col = grey(1:100 / 100), add = TRUE)

values(r) <- fuzzyLightness(values(x), values(m), 80, 20)
plot(r, col = grey(1:100 / 100), add = TRUE)

values(r) <- fuzzyLightness(values(x), values(m), 80, 1)
plot(r, col = grey(1:100 / 100), add = TRUE)
