# Nikon D50 Fisheye Nikkor 10.5 mm lens
lens <- lensPolyCoef(c(0.71553, 0.01146, -0.03928))

angle <- asAngle(c(53))
pix <- c(1202)

calcDiameter(lens, pix, angle)
