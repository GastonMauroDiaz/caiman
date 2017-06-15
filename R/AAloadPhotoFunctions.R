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

setGeneric("equipment<-", function(x, value) standardGeneric("equipment<-"),
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
#' @param x \code{\linkS4class{CanopyPhoto}}.
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

#### geocode ####
#' @title Set or get the geocode slot.
#'
#' @aliases geocode<-
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
#' @example /inst/examples/geocodeExample.R
#'
setGeneric("geocode", function(x) standardGeneric("geocode"))
#' @export geocode

#' @rdname geocode
setMethod("geocode",
  signature(x = "CanopyPhoto"),
  function (x) {
    return(x@geocode)
  }
)

setGeneric("geocode<-", function(x, value) standardGeneric("geocode<-"),
  useAsDefault = FALSE)

#' @rdname geocode
setMethod("geocode<-",
  signature(x = "CanopyPhoto", value = "SpatialPoints"),
  function (x, value) {
    x@geocode <- value
    validObject(x)
    return(x)
  }
)
#' @export geocode<-

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
#' @description Copy the slots equipment, fisheye, datetime, geocode,
#'   bearing, elevation, aperture, ssDenominator, isoSpeed and names
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
    geocode(to) <- geocode(from)
    bearing(to) <- bearing(from)
    elevation(to) <- elevation(from)
    aperture(to) <- aperture(from)
    ssDenominator(to) <- ssDenominator(from)
    isoSpeed(to) <- isoSpeed(from)
    names(to) <- names(from)
    rtitle(to) <- rtitle(from)
    return(to)
  }
)

#### ssDenominator ####
#' @title Set or get the ssDenominator slot.
#'
#' @aliases ssDenominator<-
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
#' @example /inst/examples/ssDenominatorExample.R
#'
setGeneric("ssDenominator", function(x) standardGeneric("ssDenominator"))
#' @export ssDenominator

#' @rdname ssDenominator
setMethod("ssDenominator",
          signature(x = "CanopyPhoto"),
          function (x) {
            return(x@ssDenominator)
          }
)

setGeneric("ssDenominator<-", function(x, value) standardGeneric("ssDenominator<-"),
           useAsDefault = FALSE)

#' @rdname ssDenominator
setMethod("ssDenominator<-",
          signature(x = "CanopyPhoto", value = "numeric"),
          function (x, value) {
            x@ssDenominator <- value
            validObject(x)
            return(x)
          }
)
#' @export ssDenominator<-

#### aperture ####
#' @title Set or get the aperture slot.
#'
#' @aliases aperture<-
#'
#' @description Set or get the the aperture used to take the photograph represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value Numeric. Aperture used to take \code{x}.
#'
#' @return numeric.
#'
#' @seealso \code{\link{loadPhoto}}.
#'
#' @example /inst/examples/apertureExample.R
#'
setGeneric("aperture", function(x) standardGeneric("aperture"))
#' @export aperture

#' @rdname aperture
setMethod("aperture",
          signature(x = "CanopyPhoto"),
          function (x) {
            return(x@aperture)
          }
)

setGeneric("aperture<-", function(x, value) standardGeneric("aperture<-"),
           useAsDefault = FALSE)

#' @rdname aperture
setMethod("aperture<-",
          signature(x = "CanopyPhoto", value = "numeric"),
          function (x, value) {
            x@aperture <- value
            validObject(x)
            return(x)
          }
)
#' @export aperture<-

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
