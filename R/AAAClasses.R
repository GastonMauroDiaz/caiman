#devtools::use_package("assertthat")
devtools::use_package("pracma")
devtools::use_package("class")
devtools::use_package("EBImage")
devtools::use_package("rgdal")
devtools::use_package("sp")
#devtools::use_package("HistogramTools", type = "Depends")
devtools::use_package("raster", type = "Depends")
devtools::use_package("methods")
devtools::use_package("colorspace")
devtools::use_package("exif")
devtools::use_package("imager")
#devtools::use_package("testthat", type = "Suggests")

#' @import raster
#' @importFrom colorspace sRGB
#' @importFrom methods as is new validObject
NULL

setClassUnion("RasterAll", c("RasterStackBrick", "RasterLayer"))

##### LensPolyCoef #####
#' @title An S4 class to represent coefficients that models lens distortion.
#'
#' @description An S4 class to represent coefficients that models lens
#'   distortion of hemispherical photographs. Use the helper function
#'   \code{\link{lensPolyCoef}} to generate new objects of this class.
#'
#' @slot coef numeric.
#'
#' @examples showClass("LensPolyCoef")
#'
#' @seealso \code{\link{lensPolyCoef}}
#'
setClass(Class = "LensPolyCoef",
  slots = c(coef = "numeric"),
  validity = function(object) {

    tolerance = 0.00001
    foo <- all.equal(calcR(asAngle(0), object),
                       0, tolerance = tolerance)

    if (is.logical(foo)) { #the return of all.equal is tricky
      return(TRUE)

    } else {
      stop("Polynomial function is out of range")
    }

  },
  prototype = list(coef = 2 / pi)
)

##### RelativeRadiusImage ####
#' @title An S4 class to represent Relative Radius as an image.
#'
#' @description An S4 class to represent Relative Radius as an image. Use the
#'   helper function \code{\link{makeRimage}} to generate a new
#'   \code{\linkS4class{RelativeRadiusImage}}.
#'
#' @slot diameter integer.
#' @slot ... Inherited from \code{\linkS4class{RasterLayer}}.
#'
#' @details \code{raster} package offer a wide range of functionalities to
#'   manipulate raster files efficiently. It uses georeferenced raster because
#'   was designed with a field-based conception of the geographical reality
#'   (Galton, 2011). To manipulate such data, \code{raster} package defines "S4
#'   classes" grouped in the \code{\linkS4class{Raster}} family that has a complex hierarchical
#'   structure of inheritance. These classes have slots that help to precisely
#'   locate each cell over the earth because they were designed to represent
#'   georeferenced raster. \code{RelativeRadiusImage} was built on top of \code{RasterLayer}
#'   to take advantage of \code{raster} package functionality. However,
#'   \code{RelativeRadiusImage} requires raster implementation but not georeference.
#'   That is why some inherited slots are meaningless for these class.
#'
#' @references Galton, A., 2001. A Formal Theory of Objects and Fields, in:
#'   COSIT. pp. 458-473.
#'
#' @seealso \code{\link{makeRimage}}
#'
#' @examples showClass("RelativeRadiusImage")
#'
setClass(Class = "RelativeRadiusImage",
  slots = c(diameter = "integer"),
  validity = function (object) {
      error <- FALSE
      x <- object@diameter
      if ((x / 2) != round(x / 2) | round(x) != x)
      {
        stop("diameter must be an even integer")
      } else {
        return(TRUE)
      }
  },
  contains = "RasterLayer"
)

##### ZenithImage ####
#' @title An S4 class to represent zenith angles as an image.
#'
#' @description An S4 class to represent zenith angles as an image. Use the
#'   helper function \code{\link{makeZimage}} to generate new objects of this
#'   class.
#'
#' @slot lens \code{\linkS4class{LensPolyCoef}}.
#' @slot ... Inherited from \code{\linkS4class{RelativeRadiusImage}}.
#'
#' @seealso \code{\link{makeZimage}}
#'
#' @examples showClass("ZenithImage")
#'
setClass(Class = "ZenithImage",
  slots = c(lens = "LensPolyCoef"),
  validity = function (object) {
  },
  contains = "RelativeRadiusImage"
)

##### AzimuthImage ####
#' @title An S4 class to represent azimuth angles as an image.
#'
#' @description An S4 class to represent azimuth angles as an image. Use the
#'   helper function \code{\link{makeAimage}} to generate new objects of this
#'   class.
#'
#' @slot ... Inherited from \code{\linkS4class{ZenithImage}}.
#'
#' @seealso \code{\link{makeAimage}}
#'
#' @examples showClass("AzimuthImage")
#'
setClass ("AzimuthImage",
  validity = function (object) {
    v <- max(object, na.rm = TRUE)
    c1 <- v <= 360
    c2 <- v >= 359
    if (c1 & c2) stop("Incorrect azimuth angle values.")
  },
  contains = c("RelativeRadiusImage"))

##### Angle ####
#' @title An S4 class to represent angle values.
#'
#' @description An S4 class to represent angle values. It has a slot that sets
#'   whether it is in degrees or radians, so the methods and functions can adapt
#'   to it instead of users. Only allows positive values. Use the helper
#'   function \code{\link{asAngle}} to generate new objects of this class.
#'
#' @slot values numeric.
#' @slot degrees logical.
#'
#' @seealso \code{\link{asAngle}}
#'
#' @examples showClass("Angle")
#'
setClass(Class = "Angle",
  slots = c(
    values = "numeric",
    degrees = "logical"
  ),
  prototype = list(values = seq(0, 90), degrees = TRUE),
  validity = function(object) {

    error <- TRUE
    if (!object@degrees & min(object@values) >= 0 & max(object@values) < 2 * pi)
      error <- FALSE
    if (object@degrees & min(object@values) >= 0 & max(object@values) < 360)
      error <- FALSE

    if (error)
    {
      stop(
          "\nAngle in degrees must be equal or greater than 0 and less than 360.\nAngle in radians must be equal or greater than 0 and less than 2pi.")
    } else {
      return(TRUE)
    }
  }
)

##### FishEye ####
#' @title An S4 class to represent the metadata related with fisheye photographs.
#'
#' @description An S4 class to represent the metadata related with fisheye photographs.
#'
#' @slot is logical.
#' @slot up logical.
#' @slot leveled logical.
#' @slot fullframe logical.
#'
#' @seealso \code{\link{newFishEye}}
#'
#' @examples showClass("FishEye")
#'
setClass(Class = "FishEye",
  slots = c(
    is = "logical",
    up = "logical",
    leveled = "logical",
    fullframe = "logical"
  ),
  validity = function(object) {
    c1 <- length(object@is) == 1
    c2 <- length(object@up) == 1
    c3 <- length(object@leveled) == 1
    c4 <- length(object@fullframe) == 1
    if (c1 & c2 & c3 & c4)
    {
      return(TRUE)
    } else {
      stop("is, up, leveled and fullframe must have length one.")
    }
  },
  prototype = list(is = FALSE, up = FALSE, leveled = FALSE, fullframe = FALSE)
)

##### CanopyPhoto ####
.datetimeCharacterValidation <- function (x) {
  if(length(x) != 1) {stop("The datetime slot must have length one")}

  datetime <- x

  error <- FALSE

  if (nchar(x) != 19) error <- TRUE
  x <- unlist(strsplit(x, " "))
  if (!min(nchar(x) == c(10, 8))) error <- TRUE
  .date <- x[1]
  .time <- x[2]

  dateSeparator <- c("/", "-")
  for (i in seq(1, length(dateSeparator))) {
    .date <- unlist(strsplit(.date, dateSeparator[i]))
  }
  if (length(.date) != 3) error <- TRUE

  timeSeparator <- c(":")
  for (i in seq(1, length(timeSeparator))) {
    .time <- unlist(strsplit(.time, timeSeparator[i]))
  }
  if (length(.time) != 3) error <- TRUE

  if(!error) {
    a <- paste(
                paste(.date[1], .date[2], .date[3], sep = "-" ),
                paste(.time[1], .time[2], .time[3], sep = ":" )
              )
    b <- as.character(as.POSIXlt(datetime))
    if (a != b)
      stop(paste("Something is wrong, as.POSIXlt returns", b, "but you input",a ,"."))
  }

  if (error) {
    stop("The only valid formats for the datetime slot are: yyyy/mm/dd hh:mm:ss or yyyy-mm-dd hh:mm:ss")
  } else {
    return(TRUE)
  }
}

.elevationValidation <- function(x) {
  if (!x@degrees) x <- switchUnit(x)
  if (x@values > 90) stop("An elevation greater than 90 degrees is not possible.")
  return(TRUE)
}

#' @title An S4 class to store vegetal canopy photographs.
#'
#' @description An S4 class to store vegetal canopy photographs. Use the
#'   helper function \code{\link{loadPhoto}} to build new objects of this class
#'   from a file.
#'
#' @slot equipment character.
#' @slot fisheye \code{\linkS4class{FishEye}}.
#' @slot datetime character.
#' @slot geocode \code{\link[sp]{SpatialPoints}}.
#' @slot bearing \code{\linkS4class{Angle}}.
#' @slot elevation \code{\linkS4class{Angle}}.
#' @slot ssDenominator numeric.
#' @slot aperture numeric.
#' @slot isoSpeed numeric.
#' @slot ... Inherited from \code{\linkS4class{RasterBrick}}.
#'
#' @details \code{CanopyPhoto} was built on top of \code{RasterBrick} to take
#'   advantage of the \code{raster package} functionalities. However,
#'   \code{CanopyPhoto} requires raster implementation but not georeference and
#'   this is why some inherited slots are meaningless for this class.
#'
#' @seealso \code{\link{loadPhoto}}
#'
#' @examples showClass("CanopyPhoto")
#'
setClass(Class = "CanopyPhoto",
  slots = c(
    equipment = "character",
    fisheye = "FishEye",
    datetime = "character",
    geocode = "SpatialPoints",
    bearing = "Angle",
    elevation = "Angle",
    ssDenominator = "numeric",
    aperture = "numeric",
    isoSpeed = "numeric"
    ),
  validity = function(object) {

    .elevationValidation(object@elevation)
#    .datetimeCharacterValidation(object@datetime)
    c1 <- length(object@geocode) == 1
    c2 <- length(object@bearing@values) == 1
    c3 <- length(object@elevation@values) == 1
    c4 <- length(object@ssDenominator) == 1
    c5 <- length(object@aperture) == 1
    c6 <- length(object@isoSpeed) == 1
    if (object@fisheye@is) {
      if (!object@fisheye@fullframe) stopifnot(nrow(object) == ncol(object))
      if (!object@fisheye@fullframe & round(ncol(object) / 2) != ncol(object) / 2)
        stop("The diameter of the fisheye picture must be even.")
    }
    if (c1 & c2 & c3 & c4 & c5 & c6) {
      return(TRUE)
    } else {
      stop("At lest one of this slots have length greater than one: geocode, bearing, elevation, ssDenominator, aperture or isoSpeed")
    }
  },
  prototype = list(
    fisheye = new("FishEye"),
    datetime = "1980/11/20 14:00:00",
    geocode =
      SpatialPoints(coords = matrix(c(-57.95, -34.93333), ncol = 2),
        proj4string = CRS("+init=epsg:4326")),
    bearing = new("Angle", values = 0, degrees = TRUE),
    elevation = new("Angle", values = 0, degrees = TRUE),
    ssDenominator = 0,
    aperture = 0,
    isoSpeed = 0
  ),
  contains = "RasterBrick"
)

##### BinImage ####
#' @title An S4 class to store binarized images.
#'
#' @description An S4 class to store binarized images.
#'
#' @slot threshold One-length numeric.
#' @slot originalData One-length character.
#' @slot processedLayer One-length integer	.
#' @slot ... Inherited from \code{\linkS4class{RasterLayer}}.
#'
#' @seealso \code{\link{autoThr}}, \code{\link{presetThr}},
#'   \code{\link{doMask}}, \code{\link{doOBIA}}
#'
#' @examples showClass("BinImage")
#'
setClass(Class = "BinImage",
  slots = c(
    threshold = "numeric",
    originalData = "character",
    processedLayer = "numeric"
  ),
  validity = function(object) {
   getMin(object) == 0
   getMax(object) == 1

   all(levels(as.factor(object))[[1]][,1] == c(FALSE, TRUE))
  },
  contains = "RasterLayer"
)

##### PolarSegmentation ####
#' @title An S4 class to store polar segmentations.
#'
#' @description An S4 class to store polar segmentations.
#'
#' @slot  angleWidth \code{\linkS4class{Angle}}
#' @slot  scaleParameter numeric
#' @slot  ... Inherited from \code{\linkS4class{ZenithImage}}.
#'
#' @seealso \code{\link{makeRings}}, \code{\link{makePolarSectors}},
#'   \code{\link{makePolarGrid}}
#'
#' @examples showClass("PolarSegmentation")
#'
setClass(Class = "PolarSegmentation",
  slots = c(angleWidth  = "Angle", scaleParameter = "numeric"),
  contains = "ZenithImage"
)
