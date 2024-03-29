% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_base_subgroup_read_cp.R
\name{loadPhoto}
\alias{loadPhoto}
\alias{loadPhoto,character-method}
\alias{loadPhoto,missing-method}
\title{Load a photograph from a file.}
\usage{
loadPhoto(
  x,
  upperLeft = NULL,
  width = NULL,
  height = NULL,
  equipment = NULL,
  fisheye = NULL,
  datetime = NULL,
  geoLocation = NULL,
  bearing = NULL,
  elevation = NULL,
  slope = NULL,
  exposureTime = NULL,
  fNumber = NULL,
  isoSpeed = NULL
)

\S4method{loadPhoto}{character}(
  x,
  upperLeft = NULL,
  width = NULL,
  height = NULL,
  equipment = NULL,
  fisheye = NULL,
  datetime = NULL,
  geoLocation = NULL,
  bearing = NULL,
  elevation = NULL,
  slope = NULL,
  exposureTime = NULL,
  fNumber = NULL,
  isoSpeed = NULL
)

\S4method{loadPhoto}{missing}(x)
}
\arguments{
\item{x}{character or missing.}

\item{upperLeft}{numeric of length \code{2}. Default is \code{NULL}, see
Details.}

\item{width}{integer. Default is \code{NULL}, see Details.}

\item{height}{integer. Default is \code{NULL}, see Details.}

\item{equipment}{one-length character. Default is \code{NULL}, see
\code{\link{equipment}}.}

\item{fisheye}{\code{\linkS4class{FishEye}}. See \code{\link{fisheye}}.}

\item{datetime}{one-length character. Default is \code{NULL}. The accepted formats
are: \emph{yyyy/mm/dd hh:mm:ss} or \emph{yyyy-mm-dd hh:mm:ss}, see
\code{\link{datetime}}.}

\item{geoLocation}{\code{\link[sp]{SpatialPoints}} with only \code{1} point. Default
is \code{NULL}, see \code{\link{geoLocation}}.}

\item{bearing}{\code{\linkS4class{Angle}} with only \code{1} value. Default is
\code{NULL}, see \code{\link{bearing}}.}

\item{elevation}{\code{\linkS4class{Angle}} with only \code{1} value. Default
is \code{NULL}, see \code{\link{elevation}}.}

\item{slope}{\code{\linkS4class{Angle}} with only \code{1} value. Default
is \code{NULL}, see \code{\link{slope}}.}

\item{exposureTime}{numeric. Default is \code{NULL}, see Details.}

\item{fNumber}{numeric. Default is \code{NULL}, see Details.}

\item{isoSpeed}{numeric. Default is \code{NULL}, see Details.}
}
\value{
\code{\linkS4class{CanopyPhoto}}.
}
\description{
Helper function to generate a new
  \code{\linkS4class{CanopyPhoto}}. Only support JPEG and TIFF format.
}
\details{
To make a photograph ready to process with \code{caiman} package,
  use \code{\link{loadPhoto}} with a path to the file as argument, this
  creates a new \code{\linkS4class{CanopyPhoto}}. To load only a region of
  the file use \code{upperLeft}, \code{width} and \code{height}, both in
  pixels (these must be equals if you are loading a hemispherical photograph).
  To get the upperLeft corner of your region of interest, I recommend use
  \href{https://imagej.nih.gov/ij/}{ImageJ} or
  \href{http://www.irfanview.com/}{IrfanView}.

  To get an example of a \code{\linkS4class{CanopyPhoto}}, run
  \code{loadPhoto()}. This data-example is an hemispherical photograph taken
  in a \emph{Nothofagus pumilio} forest, in Argentina. To know what equipment
  was used to take it, use \code{\link{equipment}}. To know when it was
  taken, use \code{\link{datetime}}. To know where it was taken, use
  \code{\link{geoLocation}}. To know how the camera was physically oriented use
  \code{\link{bearing}}, \code{\link{slope}} and \code{\link{fisheye}}.
  To know the exposure use \code{\link{exposureTime}},
  \code{\link{fNumber}} and \code{\link{isoSpeed}}. The metadata of a new
  CanopyPhoto could be provided as arguments for \code{loadPhoto}, but also
  can be set for an existing object using replacement methods (see examples).
}
\section{Methods (by class)}{
\itemize{
\item \code{character}: You need to provide the path to a file. For a file stored in the
working directory, just provide filename. Always include file extension.

\item \code{missing}: Return a data-example (see details).
}}

\examples{
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
}
\seealso{
\code{\link{cloneSlots}}, \code{\link{doMask}}, \code{\link{calcExposure}}.
}
