x <- 0:100
summary(normalize(x, 0, 100))
summary(normalize(x, 25, 75))
summary(normalize(x, 50, 75))

x <- normalize(loadPhoto(), 0, 255)
getMin(x)
getMax(x)
x <- normalize(loadPhoto(), 100, 200)
getMin(x)
getMax(x)
