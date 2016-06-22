x <- loadPhoto()
bearing(x)
bearing(x) <- asAngle(10)
bearing(x)
\dontrun{
  bearing(x) <- asAngle(c(10,20))
}
