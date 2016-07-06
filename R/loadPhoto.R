#' @title Load a photograph from a file.
#'
#' @description Helper function to generate a new \code{\linkS4class{CanopyPhoto}} from a file on disk. Only support JPEG and TIFF format.
#'
#' @param x character or missing. You need to provide the path to the file. For a file stored in the working directory, just provide filename. Always include file extension.
#' @param upperLeft numeric of length two. The default is NULL, see Details.
#' @param width integer. The default is NULL, see Details.
#' @param height integer. The default is NULL, see Details.
#' @param equipment one-length character. The default is NULL, see \code{\link{equipment}}.
#' @param fisheye \code{\linkS4class{FishEye}}. See \code{\link{fisheye}}.
#' @param datetime one-length character. The default is NULL. The accepted formats are: \emph{yyyy/mm/dd hh:mm:ss} or \emph{yyyy-mm-dd hh:mm:ss}, see \code{\link{datetime}}.
#' @param geocode \code{\link[sp]{SpatialPoints}} with only one point. The default is NULL, see \code{\link{geocode}}.
#' @param bearing \code{\linkS4class{Angle}} with only one value. The default is NULL, see \code{\link{bearing}}.
#' @param elevation \code{\linkS4class{Angle}} with only one value. The default is NULL, see \code{\link{elevation}}.
#'
#' @details To make a photograph ready to process with \code{caiman package}, use \code{loadPhoto} with a path to the file as argument. This create a new \code{\linkS4class{CanopyPhoto}}. To load only a region of the file use \code{upperLeft}, \code{width} and \code{height}, both in pixels. This must be equal if you are loading a hemispherical photograph. To get the upperLeft corner of your region of interest, I recommend use \href{https://imagej.nih.gov/ij/}{ImageJ} or \href{http://www.irfanview.com/}{IrfanView}.
#'
#' To get an example of \code{\linkS4class{CanopyPhoto}}, run \code{loadPhoto()}. This data-example is a hemispherical photograph taken in a \emph{Nothofagus pumilio} forest, in Argentina. To know what equipment was used to take it, use \code{\link{equipment}}. To know when it was taken, use \code{\link{datetime}}. To know where it was taken, use \code{\link{geocode}}. To know how the camera was physically oriented use \code{\link{bearing}}, \code{\link{elevation}} and \code{\link{fisheye}}. This metadata helps to represent the object and automatically preset methods.
#'
#' The metadata of a new \code{\linkS4class{CanopyPhoto}} could be provided as arguments for \code{loadPhoto}, but also can be set for an existing object using replacement methods (see examples).
#'
#' @return \code{\linkS4class{CanopyPhoto}}.
#'
#' @seealso \code{\link{cloneSlots}}, \code{\link{doMask}}.
#'
#' @example /inst/examples/loadPhotoExample.R
#'
setGeneric("loadPhoto", function(x, upperLeft = NULL, width = NULL,
    height = NULL, equipment = NULL,  fisheye = NULL, datetime = NULL,
      geocode = NULL, bearing = NULL,  elevation = NULL)
        standardGeneric("loadPhoto"))
#' @export loadPhoto

#' @describeIn loadPhoto You need to provide the path to the file. For a file
#'   stored in the working directory, just provide filename. Always include file
#'   extension.
setMethod("loadPhoto",
  signature(x = "character"),
  function (x, upperLeft, width, height, equipment, fisheye, datetime,
    geocode, bearing, elevation)
  {
    # Code from raster-package rasterFromFile.R, Version 1.0.
    # START
    x <- trim(x)
    if (x=='' | x=='.') { # etc?
      stop('Provide a valid filename.')
    }
    if (!file.exists(x)) stop("File does not exists.")

    # fix for opendap https://r-forge.r-project.org/forum/message.php?msg_id=5015
    start <- tolower(substr(x, 1, 4))
    if (! start %in% c('http', 'ftp')) {
      y <- NULL
      try( y <- normalizePath(x, mustWork=TRUE), silent=TRUE)
      if (!is.null(y)) {
        x <- y
      }
    }

    fileext <- toupper(extension(x))
    # END

    if (fileext %in% c(".JPG", ".JPEG", ".TIF", ".TIFF", ".GRD")) {
      r <- brick(x)
      r <- as(r, "CanopyPhoto")
      if (nlayers(r) != 3) stop("The photograph must have tree layer.")
      names(r) <- c("Red", "Green", "Blue")
    }
    if (exists("r", inherits = FALSE)) {
      if (!is.null(equipment)) equipment(r) <- equipment
#       if (all(!is.null(up), !is.null(leveled), !is.null(fullframe)))
#         r <- fisheye(r, up, leveled, fullframe)
      if (!is.null(fisheye)) fisheye(r) <- fisheye
      if (!is.null(datetime)) datetime(r) <- datetime
      if (!is.null(geocode)) geocode(r) <- geocode
      if (!is.null(bearing)) bearing(r) <- bearing
      if (!is.null(elevation)) elevation(r) <- elevation

      if (all(!is.null(upperLeft), !is.null(height), !is.null(width))) {

        if (length(upperLeft) != 2)
          stop("upperLeft should be a numeric vector of length two")
        if (any(upperLeft == c(0, 0)))
          stop("upperLeft shoul be c(1, 1) instead of c(0,0).")
        if (all(length(height) != 1, as.integer(height) == height))
          stop("height should be one-lenght integer")
        if (all(length(width) != 1, as.integer(width) == width))
          stop("width should be one-lenght integer")

        xmn <- xFromCol(r, upperLeft[1])
        xmx <- xFromCol(r, upperLeft[1] + width)
        ymx <- yFromRow(r, upperLeft[2])
        ymn <- yFromRow(r, upperLeft[2] + height)

        if(any(is.na(xmn), is.na(xmx), is.na(ymn), is.na(ymx))) {
          stop("Your selection goes out of the picture, please review upperLeft, heigth and width.")
        }

        e <- extent(xmn, xmx, ymn, ymx)
        r <- crop(r, e)
        extent(r) <- extent(0, ncol(r), 0, nrow(r))
        r <- as(r, "CanopyPhoto")
      }

      validObject(r)
      return(r)
    } else {
      stop("Only support JPEG, TIFF or raster-package native image format.")
    }
  }
)

#' @describeIn loadPhoto Return a data-example (see details).
setMethod("loadPhoto",
  signature(x = "missing"),
  function (x)
  {
    path <- system.file("external/UnFavAutoE3.jpg", package = "caiman")

    x <- loadPhoto(path)
    rtitle(x) <- "UnFavAutoE3"
    equipment(x) <- "Nikon 5700 FC-E9 "
    datetime(x) <- "2009-12-07 07:39:18"
    bearing(x) <- asAngle(0)
    elevation(x) <- asAngle(90)
    geocode(x) <-
      sp::SpatialPoints(coords = matrix(c(-71.4638466,-43.8223210), ncol = 2), proj4string = sp::CRS("+init=epsg:4326"))
    fisheye(x) <- newFishEye(TRUE, TRUE, FALSE)

    return(x)
  }
)
