x <- loadPhoto()
elevation(x)
elevation(x) <- asAngle(55)
elevation(x)

\dontrun{
  elevation(x) <- asAngle(c(10,20))
}
