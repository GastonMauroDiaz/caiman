## Making an artificially fullframe hemiphoto by cropping a non-fullframe
## hemiphoto (just as an example)
path <- system.file("external/UnFavAutoE3.jpg", package="caiman")
x <- loadPhoto(path)
### declaring it as a fullframe
fisheye(x) <- newFishEye(TRUE, TRUE, TRUE)

y <- lensPolyCoef(c(0.6427, 0.0346, -0.024491))
diameter <- calcDiameter(x, y)
r <- makeRimage(diameter, y)

x <- expandFullframe(x, y)
plot(x)

fisheye(x)
