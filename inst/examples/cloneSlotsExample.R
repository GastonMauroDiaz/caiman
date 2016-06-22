x <- loadPhoto()
equipment(x) <- "some string"

y <- loadPhoto()

x <- cloneSlots(x, y)
equipment(x)
