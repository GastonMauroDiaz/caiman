x <- loadPhoto() # or use a path to a file on your disk
x
plot(x)

path <- system.file("external/UnFavAutoE3.jpg", package="caiman")
# You can also set all or any slots
x <- loadPhoto(path, equipment = "some string", fisheye = newFishEye(),
                  datetime = "2000/01/01 12:00:00",
                    geocode = SpatialPoints(cbind(-43, -71)),
                      bearing = asAngle(0), elevation = asAngle(90))
x
plot(x)

path <- system.file("external/Black_caiman_Macrofotografie_2.jpg",
                                                              package="caiman")
x <- loadPhoto(path)
x
plot(x)
x <- loadPhoto(path, upperLeft = c(49, 29), width = 140, height = 81)
x
plot(x)

# this is what upperLeft means in hemispherical photographs:
x <- extend(makeRimage(200), extent(-50, 250, -20, 220))
extent(x) <- extent(0, ncol(x), 0 , nrow(x))
e <- extent(50, 250, 20, 220)
plot(is.na(x), legend = FALSE, axes = FALSE, box = FALSE,
  xlab = "240 px", ylab = "300 px")
plot(e, add = TRUE)
text(sp::SpatialPoints(cbind(50, 220)), "upperLeft = c(20, 50)", col = "white")
