x <- loadPhoto()
datetime(x)
datetime(x) <- "1980-11-20 14:00:00"
datetime(x)

\dontrun{
  datetime(x) <- "an error"
}
