x <- loadPhoto()
geoLocation(x)
xy <- matrix(c(-57.95, -34.93), ncol = 2)
newPt <- SpatialPoints(xy, proj4string = CRS("+init=epsg:4326"))
geoLocation(x) <- newPt
geoLocation(x)

\dontrun{
  xy <- matrix(c(-57.95, -34.93, -57, -34), ncol = 2)
  newPt <- SpatialPoints(xy, proj4string = CRS("+init=epsg:4326"))
  geoLocation(x) <- newPt
}
