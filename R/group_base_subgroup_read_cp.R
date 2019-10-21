#### loadPhoto ####

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


#### bearing #####
#' @title Set or get the bearing slot.
#'
#' @aliases bearing<-
#'
#' @description Set or get the \code{\linkS4class{Angle}} that represents the
#'   bearing of a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\linkS4class{Angle}} that represents the bearing of
#'   \code{x}.
#'
#' @details Bearing is a synonym for azimuth commonly used in geotagged
#'   photographs. Like azimuth, it is the angle between the North direction and
#'   the line of sight projected to the leveled plane. Since it is used for
#'   non-hemispherical-photographs, the line of sight refers to the centerline
#'   of the cone of vision (see details of \code{\link{lensPolyCoef}}). But if
#'   the photographs have been taken looking directly to the zenith (90 degrees
#'   of elevation), bearing does not make sense. In that case, bearing refers to
#'   the orientation of the top of the frame, which means taking photographs
#'   with the top of the camera facing the bearing. The same is valid for
#'   downward looking leveled hemispherical photographs.
#'
#' @return \code{\linkS4class{Angle}}.
#'
#' @seealso \code{\link{makeAimage}}
#'
#' @example /inst/examples/bearingExample.R
#'
setGeneric("bearing", function(x) standardGeneric("bearing"))
#' @export bearing

#' @rdname bearing
setMethod("bearing",
  signature(x = "CanopyPhoto"),
  function (x) {
    return(x@bearing)
  }
)

setGeneric("bearing<-", function(x, value) standardGeneric("bearing<-"),
  useAsDefault = FALSE)

#' @rdname bearing
setMethod("bearing<-",
  signature(x = "CanopyPhoto", value = "Angle"),
  function (x, value) {
    x@bearing <- value
    validObject(x)
    return(x)
  }
)
#' @export bearing<-

#### elevation ######
#' @title Set or get the elevation slot.
#'
#' @aliases elevation<-
#'
#' @description Set or get the \code{\linkS4class{Angle}} that represents the
#'   elevation of a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\linkS4class{Angle}} that represents the elevation of
#'   \code{x}.
#'
#' @details The elevation is the complement of the line-of-sight zenith angle.
#'   For zenith angle definition, see the details of \code{\link{lensPolyCoef}}.
#'
#' @return \code{\linkS4class{Angle}}.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/elevationExample.R
#'
setGeneric("elevation", function(x) standardGeneric("elevation"))
#' @export elevation

#' @rdname elevation
setMethod("elevation",
  signature(x = "CanopyPhoto"),
  function (x) {
    return(x@elevation)
  }
)

setGeneric("elevation<-", function(x, value) standardGeneric("elevation<-"),
  useAsDefault = FALSE)

#' @rdname elevation
setMethod("elevation<-",
  signature(x = "CanopyPhoto", value = "Angle"),
  function (x, value) {
    x@elevation <- value
    validObject(x)
    return(x)
  }
)
#' @export elevation<-

#### equipment #####
#' @title Set or get the equipment slot.
#'
#' @aliases equipment<-
#'
#' @description Set or get the length-one character string that describe the
#'   equipment used to take the photograph represented by a
#'   \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value character that describe the equipment used to take \code{x}.
#'
#' @return \code{character}.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/equipmentExample.R
#'
setGeneric("equipment", function(x) standardGeneric("equipment"))
#' @export equipment

#' @rdname equipment
setMethod("equipment",
  signature(x = "CanopyPhoto"),
  function (x) {
    return(x@equipment)
  }
)

setGeneric("equipment<-",
           function(x, value) standardGeneric("equipment<-"),
           useAsDefault = FALSE)

#' @rdname equipment
setMethod("equipment<-",
  signature(x = "CanopyPhoto", value = "character"),
  function (x, value) {
    x@equipment <- value
    validObject(x)
    return(x)
  }
)
#' @export equipment<-

#### datetime #####
#' @title Set or get the datetime slot.
#'
#' @aliases datetime<-
#'
#' @description Set or get the character string with date and time when the
#'   photograph was taken.
#'
#' @param x \code{\linkS4class{CanopyPhoto}} or character.
#' @param value Character. The only valid formats for the datetime slot are:
#'   yyyy/mm/dd hh:mm:ss or yyyy-mm-dd hh:mm:ss
#'
#' @return character.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/datetimeExample.R
#'
setGeneric("datetime", function(x) standardGeneric("datetime"))
#' @export datetime

#' @rdname datetime
setMethod("datetime",
  signature(x = "CanopyPhoto"),
  function (x) {
    return(x@datetime)
  }
)

#' @describeIn  datetime \code{x} character. The names of the photographs
#'   (extension included) of the canopy models. If your working directory is the
#'   one that contains the photographs, use just the file name, otherwise, use
#'   the full path to the file.
setMethod("datetime",
          signature(x = "character"),
          function(x) {
            evs <- c()
            for (i in 1:length(x)) {
              cp <- loadPhoto(x[i])
              evs[i] <- datetime(cp)
            }
            evs
          }
)

setGeneric("datetime<-", function(x, value) standardGeneric("datetime<-"),
  useAsDefault = FALSE)

#' @rdname datetime
setMethod("datetime<-",
  signature(x = "CanopyPhoto", value = "character"),
  function (x, value) {
    x@datetime <- value
    validObject(x)
    return(x)
  }
)
#' @export datetime<-

#### geoLocation ####
#' @title Set or get the geoLocation slot.
#'
#' @aliases geoLocation<-
#'
#' @description Set or get the \code{\link[sp]{SpatialPoints}} that represent
#'   the shot location for the photograph represented by a
#'   \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\link[sp]{SpatialPoints}} that represents the location of
#'   \code{x}.
#'
#' @return \code{\link[sp]{SpatialPoints}}.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/geoLocationExample.R
#'
setGeneric("geoLocation", function(x) standardGeneric("geoLocation"))
#' @export geoLocation

#' @rdname geoLocation
setMethod("geoLocation",
  signature(x = "CanopyPhoto"),
  function (x) {
    return(x@geoLocation)
  }
)

setGeneric("geoLocation<-", function(x, value) standardGeneric("geoLocation<-"),
  useAsDefault = FALSE)

#' @rdname geoLocation
setMethod("geoLocation<-",
  signature(x = "CanopyPhoto", value = "SpatialPoints"),
  function (x, value) {
    x@geoLocation <- value
    validObject(x)
    return(x)
  }
)
#' @export geoLocation<-

#### newFishEye #####
#' @title Generate a new FishEye.
#'
#' @description Helper function to generate a \code{\linkS4class{FishEye}}
#'   needed to fill the slot \code{fisheye} of a
#'   \code{\linkS4class{CanopyPhoto}}.
#'
#' @param up missing or logical. Set as \code{TRUE} if the photograph was taken
#'   looking up.
#' @param  leveled missing or logical. Set as \code{TRUE} if the camera was leveled
#'   and therefore its optical axis was aligned with a perfect vertical.
#' @param  fullframe missing or logical. Set as \code{TRUE} if all the pixels of
#'   the photograph have data. For circular fisheye, with hundreds of no-data
#'   pixels, set as \code{FALSE}.
#'
#' @details A call to \code{newFishEye()} returns the prototype for
#'   \code{\linkS4class{FishEye}}.
#'
#' @return \code{\linkS4class{FishEye}}.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/newFishEyeExample.R
#'
setGeneric("newFishEye", function(up, leveled, fullframe) standardGeneric("newFishEye"))
#' @export newFishEye

#' @rdname newFishEye
setMethod("newFishEye",
  signature(up = "logical", leveled = "logical", fullframe = "logical"),
  function (up, leveled, fullframe) {
    x <- new("FishEye", is = TRUE, up = up, leveled = leveled,
      fullframe = fullframe)
    validObject(x)
    return(x)
  }
)

#' @rdname newFishEye
setMethod("newFishEye",
  signature(up = "missing", leveled = "missing", fullframe = "missing"),
  function (up, leveled, fullframe) {
    x <- new("FishEye")
    validObject(x)
    return(x)
  }
)

#### fisheye #####
#' @title Set or get the fisheye slot.
#'
#' @aliases fisheye<-
#'
#' @description Set or get the fisheye object that describe the photograph
#'   represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\linkS4class{FishEye}}.
#'
#' @return \code{\linkS4class{FishEye}}.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/fisheyeExample.R
#'
setGeneric("fisheye", function(x) standardGeneric("fisheye"))
#' @export fisheye

#' @rdname fisheye
setMethod("fisheye",
  signature(x = "CanopyPhoto"),
  function (x) {
    return(x@fisheye)
  }
)

setGeneric("fisheye<-", function(x, value) standardGeneric("fisheye<-"),
  useAsDefault = FALSE)

#' @rdname fisheye
setMethod("fisheye<-",
  signature(x = "CanopyPhoto", value = "FishEye"),
  function (x, value) {

    x@fisheye <- value

    validObject(x)
    return(x)
  }
)
#' @export fisheye<-


#### cloneSlots ####
#' @title Copy some slots between CanopyPhoto objects.
#'
#' @description Copy the slots equipment, fisheye, datetime, geoLocation,
#'   bearing, elevation, fNumber, exposureTime, isoSpeed and names
#'   from a \code{\linkS4class{CanopyPhoto}} to another.
#'
#' @param from \code{\linkS4class{CanopyPhoto}}.
#' @param to \code{\linkS4class{CanopyPhoto}}.
#'
#' @return \code{\linkS4class{CanopyPhoto}}.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/cloneSlotsExample.R
#'
setGeneric("cloneSlots", function(from, to) standardGeneric("cloneSlots"))
#' @export cloneSlots

#' @rdname cloneSlots
setMethod("cloneSlots",
  signature(from = "CanopyPhoto", to = "CanopyPhoto"),
  function (from, to) {
    equipment(to) <- equipment(from)
    fisheye(to) <- fisheye(from)
    datetime(to) <- datetime(from)
    geoLocation(to) <- geoLocation(from)
    bearing(to) <- bearing(from)
    elevation(to) <- elevation(from)
    slope(to) <- slope(from)
    fNumber(to) <- fNumber(from)
    exposureTime(to) <- exposureTime(from)
    isoSpeed(to) <- isoSpeed(from)
    names(to) <- names(from)
    rtitle(to) <- rtitle(from)
    return(to)
  }
)

#### exposureTime ####
#' @title Set or get the exposureTime slot.
#'
#' @aliases exposureTime<-
#'
#' @description Set or get the denominator of the shutter speed used to take the photograph represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value numeric. Denominator of the shutter speed used to take \code{x}.
#'
#' @return numeric.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/exposureTimeExample.R
#'
setGeneric("exposureTime", function(x) standardGeneric("exposureTime"))
#' @export exposureTime

#' @rdname exposureTime
setMethod("exposureTime",
          signature(x = "CanopyPhoto"),
          function (x) {
            return(as.numeric(x@exposureTime))
          }
)

setGeneric("exposureTime<-", function(x, value) standardGeneric("exposureTime<-"),
           useAsDefault = FALSE)

#' @rdname exposureTime
setMethod("exposureTime<-",
          signature(x = "CanopyPhoto", value = "numeric"),
          function (x, value) {
            x@exposureTime <- value
            validObject(x)
            return(x)
          }
)
#' @export exposureTime<-

#### fNumber ####
#' @title Set or get the fNumber slot.
#'
#' @aliases fNumber<-
#'
#' @description Set or get the the fNumber used to take the photograph represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value Numeric. fNumber used to take \code{x}.
#'
#' @return numeric.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/fNumberExample.R
#'
setGeneric("fNumber", function(x) standardGeneric("fNumber"))
#' @export fNumber

#' @rdname fNumber
setMethod("fNumber",
          signature(x = "CanopyPhoto"),
          function (x) {
            return(x@fNumber)
          }
)

setGeneric("fNumber<-", function(x, value) standardGeneric("fNumber<-"),
           useAsDefault = FALSE)

#' @rdname fNumber
setMethod("fNumber<-",
          signature(x = "CanopyPhoto", value = "numeric"),
          function (x, value) {
            x@fNumber <- value
            validObject(x)
            return(x)
          }
)
#' @export fNumber<-

#### isoSpeed ####
#' @title Set or get the isoSpeed slot.
#'
#' @aliases isoSpeed<-
#'
#' @description Set or get the ISO speed used to take the photograph represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value numeric. ISO Speed used to take \code{x}.
#'
#' @return numeric.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/isoSpeedExample.R
#'
setGeneric("isoSpeed", function(x) standardGeneric("isoSpeed"))
#' @export isoSpeed

#' @rdname isoSpeed
setMethod("isoSpeed",
          signature(x = "CanopyPhoto"),
          function (x) {
            return(x@isoSpeed)
          }
)

setGeneric("isoSpeed<-", function(x, value) standardGeneric("isoSpeed<-"),
           useAsDefault = FALSE)

#' @rdname isoSpeed
setMethod("isoSpeed<-",
          signature(x = "CanopyPhoto", value = "numeric"),
          function (x, value) {
            x@isoSpeed <- value
            validObject(x)
            return(x)
          }
)
#' @export isoSpeed<-



#### getHour ####
#' @title Get hour of a day from datetime character
#'
#' @description Get hour of a day from datetime character.
#'
#' @param x character.
#'
#' @return numeric.
#'
#' @seealso \code{\link{datetime}}.
#'
#' @example /inst/examples/getHourExample.R
#'
setGeneric("getHour", function(x) standardGeneric("getHour"))
#' @export getHour

#' @rdname getHour
setMethod("getHour",
          signature(x = "character"),
          function (x) {
            datetime <- substr(x, 12, 19)
            hour <- strsplit(as.character(datetime), "\\:")
            hour <- unlist(hour)
            hour  <- as.numeric(hour)
            foo <- t(matrix(hour, nrow = 3))
            hour <- foo[,1] + foo[,2]/60 + foo[,3] / (60*60)
            hour
          }
)


#### slope ######
#' @title Set or get the slope slot.
#'
#' @aliases slope<-
#'
#' @description Set or get the the slope of the photosite in which the
#'   photograph represented by a \code{\linkS4class{CanopyPhoto}} was taken.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\linkS4class{Angle}} that represents the slope of
#'   \code{x}.
#'
#' @details The slope is the complement of the line-of-sight zenith angle.
#'   For zenith angle definition, see the details of \code{\link{lensPolyCoef}}.
#'
#' @return \code{\linkS4class{Angle}}.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/slopeExample.R
#'
setGeneric("slope", function(x) standardGeneric("slope"))
#' @export slope

#' @rdname slope
setMethod("slope",
          signature(x = "CanopyPhoto"),
          function (x) {
            return(x@slope)
          }
)

setGeneric("slope<-", function(x, value) standardGeneric("slope<-"),
           useAsDefault = FALSE)

#' @rdname slope
setMethod("slope<-",
          signature(x = "CanopyPhoto", value = "Angle"),
          function (x, value) {
            x@slope <- value
            validObject(x)
            return(x)
          }
)
#' @export slope<-

