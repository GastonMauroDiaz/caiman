## Making an artificially fullframe hemiphoto by cropping a non-fullframe
## hemiphoto (just for making an example)
path <- system.file("external/UnFavAutoE3.jpg", package="caiman")
x <- loadPhoto(path, upperLeft = c(33, 120), width = 414, height = 240)
### declaring it as a fullframe
fisheye(x) <- newFishEye(TRUE, TRUE, TRUE)

plot(expandFullframe(x))

x <- expandFullframe(x, FALSE)
plot(x)

fisheye(x)
