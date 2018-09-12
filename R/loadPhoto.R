.extractDatetimeFromExif <- function(x) {

  x <- unlist(strsplit(x, " "))
  .date <- x[1]
  .time <- x[2]

  dateSeparator <- c("/", "-", ":")
  for (i in seq(1, length(dateSeparator))) {
    .date <- unlist(strsplit(.date, dateSeparator[i]))
  }

  timeSeparator <- c(":")
  for (i in seq(1, length(timeSeparator))) {
    .time <- unlist(strsplit(.time, timeSeparator[i]))
  }

  x <- paste(
    paste(.date[1], .date[2], .date[3], sep = "-" ),
    paste(.time[1], .time[2], .time[3], sep = ":" )
  )
  x
}

.extractgeoLocationFromExif <- function(GPSLongitude, GPSLatitude) {
  sp::SpatialPoints(coords = matrix(c(GPSLongitude, GPSLatitude), ncol = 2),
                          proj4string = sp::CRS("+init=epsg:4326"))
}


#' @title Load a photograph from a file.
#'
#' @description Helper function to generate a new
#'   \code{\linkS4class{CanopyPhoto}}. Only support JPEG and TIFF format.
#'
#' @param x character or missing.
#' @param upperLeft numeric of length \code{2}. Default is \code{NULL}, see
#'   Details.
#' @param width integer. Default is \code{NULL}, see Details.
#' @param height integer. Default is \code{NULL}, see Details.
#' @param equipment one-length character. Default is \code{NULL}, see
#'   \code{\link{equipment}}.
#' @param fisheye \code{\linkS4class{FishEye}}. See \code{\link{fisheye}}.
#' @param datetime one-length character. Default is \code{NULL}. The accepted formats
#'   are: \emph{yyyy/mm/dd hh:mm:ss} or \emph{yyyy-mm-dd hh:mm:ss}, see
#'   \code{\link{datetime}}.
#' @param geoLocation \code{\link[sp]{SpatialPoints}} with only \code{1} point. Default
#'   is \code{NULL}, see \code{\link{geoLocation}}.
#' @param bearing \code{\linkS4class{Angle}} with only \code{1} value. Default is
#'   \code{NULL}, see \code{\link{bearing}}.
#' @param elevation \code{\linkS4class{Angle}} with only \code{1} value. Default
#'   is \code{NULL}, see \code{\link{elevation}}.
#' @param slope \code{\linkS4class{Angle}} with only \code{1} value. Default
#'   is \code{NULL}, see \code{\link{slope}}.
#' @param exposureTime numeric. Default is \code{NULL}, see Details.
#' @param fNumber numeric. Default is \code{NULL}, see Details.
#' @param isoSpeed numeric. Default is \code{NULL}, see Details.
#'
#' @details To make a photograph ready to process with \code{caiman} package,
#'   use \code{\link{loadPhoto}} with a path to the file as argument, this
#'   creates a new \code{\linkS4class{CanopyPhoto}}. To load only a region of
#'   the file use \code{upperLeft}, \code{width} and \code{height}, both in
#'   pixels (these must be equals if you are loading a hemispherical photograph).
#'   To get the upperLeft corner of your region of interest, I recommend use
#'   \href{https://imagej.nih.gov/ij/}{ImageJ} or
#'   \href{http://www.irfanview.com/}{IrfanView}.
#'
#'   To get an example of a \code{\linkS4class{CanopyPhoto}}, run
#'   \code{loadPhoto()}. This data-example is an hemispherical photograph taken
#'   in a \emph{Nothofagus pumilio} forest, in Argentina. To know what equipment
#'   was used to take it, use \code{\link{equipment}}. To know when it was
#'   taken, use \code{\link{datetime}}. To know where it was taken, use
#'   \code{\link{geoLocation}}. To know how the camera was physically oriented use
#'   \code{\link{bearing}}, \code{\link{slope}} and \code{\link{fisheye}}.
#'   To know the exposure use \code{\link{exposureTime}},
#'   \code{\link{fNumber}} and \code{\link{isoSpeed}}. The metadata of a new
#'   CanopyPhoto could be provided as arguments for \code{loadPhoto}, but also
#'   can be set for an existing object using replacement methods (see examples).
#'
#' @return \code{\linkS4class{CanopyPhoto}}.
#'
#' @seealso \code{\link{cloneSlots}}, \code{\link{doMask}}, \code{\link{calcExposure}}.
#'
#' @example /inst/examples/loadPhotoExample.R
#'
setGeneric("loadPhoto", function(x, upperLeft = NULL, width = NULL,
    height = NULL, equipment = NULL,  fisheye = NULL, datetime = NULL,
      geoLocation = NULL, bearing = NULL,  elevation = NULL, slope = NULL,
        exposureTime = NULL, fNumber = NULL, isoSpeed = NULL)
          standardGeneric("loadPhoto"))
#' @export loadPhoto

#' @describeIn loadPhoto You need to provide the path to a file. For a file stored in the
#'   working directory, just provide filename. Always include file extension.
setMethod("loadPhoto",
  signature(x = "character"),
  function (x, upperLeft, width, height, equipment, fisheye, datetime,
    geoLocation, bearing, elevation, slope, exposureTime, fNumber, isoSpeed)
  {

    # Code from raster-package rasterFromFile.R, Version 1.0.
    ## START
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
    ## END

    if (fileext %in% c(".JPG", ".JPEG", ".TIF", ".TIFF", ".GRD")) {
      r <- brick(x)
      r <- as(r, "CanopyPhoto")
      if (nlayers(r) != 3) stop("The photograph must have tree layer.")
    }

    if (all(!is.null(upperLeft), !is.null(height), !is.null(width))) {

        if (length(upperLeft) != 2)
          stop("upperLeft should be a numeric vector of length two")
        if (any(upperLeft == c(0, 0)))
          stop("upperLeft shoul be c(1, 1) instead of c(0,0).")
        if (all(length(height) != 1, as.integer(height) == height))
          stop("height should be one-lenght integer")
        if (all(length(width) != 1, as.integer(width) == width))
          stop("width should be one-lenght integer")

        baz <- 0
        xmn <- xFromCol(r, upperLeft[1] - baz)
        xmx <- xFromCol(r, upperLeft[1] + width - baz)
        ymx <- yFromRow(r, upperLeft[2] - baz)
        ymn <- yFromRow(r, upperLeft[2] + height - baz)

        if(any(is.na(xmn), is.na(xmx), is.na(ymn), is.na(ymx))) {
          stop("Your selection goes out of the picture, please review upperLeft, height, and width.")
        }

        e <- extent(xmn, xmx, ymn, ymx)
        r <- crop(r, e) # crop seems unprecise
        extent(r) <- extent(0, ncol(r), 0, nrow(r))
        r <- as(r, "CanopyPhoto")
    }

    if (exists("r", inherits = FALSE)) {
      names(r) <- c("Red", "Green", "Blue")

      tmp1 <- strsplit(x, "/")
      tmp2 <- strsplit(x, "\\\\")
      tmp <- which.max(c(length(tmp1[[1]]), length(tmp2[[1]])))
      tmp <- get(paste0("tmp", tmp))[[1]]

      rtitle(r) <- tmp[length(tmp)]

      foo <- function(x) {
        if (any(names(exif) == x)) {
          names(exif)
          unname(unlist(exif[, names(exif) == x]))
        }
      }

      if (length(grep(".jpg", rtitle(r), ignore.case = TRUE)) > 0) {
        if (!requireNamespace("exifr", quietly = TRUE)) {
          warning("EXIF data cannot be extracted. Package \"exifr\" needed for automatic extraction of Exif data.")
        } else {
          exif <- try(exifr::read_exif(x), silent = TRUE)
          if (!is.null(equipment)) equipment(r) <- equipment
          else try(equipment(r) <- paste(foo("Make"), foo("Model")), silent = TRUE)
          if (!is.null(fisheye)) fisheye(r) <- fisheye
          if (!is.null(datetime)) datetime(r) <- datetime
          else try(datetime(r) <-
                     .extractDatetimeFromExif(foo("DateTimeOriginal")), silent = TRUE)
          if (!is.null(geoLocation)) geoLocation(r) <- geoLocation
          else try(geoLocation(r) <-
                     .extractgeoLocationFromExif(foo("GPSLongitude"),
                                                 foo("GPSLatitude")), silent = TRUE)
          if (!is.null(bearing)) bearing(r) <- bearing
          if (!is.null(elevation)) elevation(r) <- elevation
          if (!is.null(slope)) slope(r) <- slope
          if (!is.null(exposureTime)) exposureTime(r) <- exposureTime
          else try(exposureTime(r) <- foo("ExposureTime"), silent = TRUE)
          if (!is.null(fNumber)) fNumber(r) <- fNumber
          else try(fNumber(r) <- foo("FNumber"), silent = TRUE)
          if (!is.null(isoSpeed)) isoSpeed(r) <- isoSpeed
          else try(isoSpeed(r) <- foo("ISO"), silent = TRUE)
        }

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
    slope(x) <- asAngle(0)
    geoLocation(x) <-
      sp::SpatialPoints(coords = matrix(c(-71.4638466,-43.8223210), ncol = 2),
                          proj4string = sp::CRS("+init=epsg:4326"))
    fisheye(x) <- newFishEye(TRUE, TRUE, FALSE)
    fNumber(x) <- 3.5
    exposureTime(x) <- 1/77
    isoSpeed(x) <- 100

    return(x)
  }
)
