x <- loadPhoto()
geocode(x)
xy <- matrix(c(-57.95, -34.93), ncol = 2)
newPt <- SpatialPoints(xy, proj4string = CRS("+init=epsg:4326"))
geocode(x) <- newPt
geocode(x)

\dontrun{
  xy <- matrix(c(-57.95, -34.93, -57, -34), ncol = 2)
  newPt <- SpatialPoints(xy, proj4string = CRS("+init=epsg:4326"))
  geocode(x) <- newPt
}
