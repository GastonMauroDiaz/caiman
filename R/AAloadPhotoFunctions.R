###################
## ** bearing ** ##
###################

#' @title Set or get bearing.
#'
#' @aliases bearing<-
#'
#' @description Set or get the \code{\linkS4class{Angle}} that represents the bearing of a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\linkS4class{Angle}} that represents the bearing of \code{x}.
#'
#' @details Bearing is a synonym for azimuth commonly used in geotagged photographs. Like azimuth, it is the angle between the North direction and the line of sight projected to the leveled plane. Because it is used for non-hemispherical-photographs, the line of sight refers to the centerline of the cone of vision (see details of \code{\link{lensPolyCoef}}). But if the photographs are taken looking directly to the zenith (90 degrees of elevation), bearing no make sense. In that case, bearing refer to the orientation of the top of the frame, which means taking photographs with the top of the camera facing the bearing. The same is valid for downward looking leveled hemispherical photographs.
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

#####################
## ** elevation ** ##
#####################

#' @title Set or get elevation.
#'
#' @aliases elevation<-
#'
#' @description Set or get the \code{\linkS4class{Angle}} that represent the elevation of a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\linkS4class{Angle}} that represents the elevation of \code{x}.
#'
#' @details The elevation is the complement of the zenith angle of the line of sight. For zenith angle definition, see details of \code{\link{lensPolyCoef}}.
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

#####################
## ** equipment ** ##
#####################

#' @title Set or get equipment.
#'
#' @aliases equipment<-
#'
#' @description Set or get the length-one character string that describe the equipment used to take the photograph represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value Character that describe the equipment used to take \code{x}.
#'
#' @return Character.
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

####################
## ** datetime ** ##
####################

#' @title Set or get datetime.
#'
#' @aliases datetime<-
#'
#' @description Set or show the character string with date and time of shot for the photo represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\link{CanopyPhoto-class}}.
#' @param value Character that represents the elevation of \code{x}.
#'
#' @return Character.
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

###################
## ** geocode ** ##
###################

#' @title Set or get geocode.
#'
#' @aliases geocode<-
#'
#' @description Set or get the \code{\link[sp]{SpatialPoints}} that represent the shot location for the photograph represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\link[sp]{SpatialPoints}} that represents the location of \code{x}.
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

######################
## ** newFishEye ** ##
######################

#' @title Generate a new FishEye
#'
#' @description Helper function to generate a \code{\linkS4class{FishEye}} needed to fill the slot \code{filsheye} of \code{\linkS4class{CanopyPhoto}}.
#'
#' @param up missing or logical. Set \code{TRUE} if the photograph was taken looking up.
#' @param  leveled missing or logical. Set \code{TRUE} if the camera was leveled and therefore its optical axis was aligned with a perfect vertical.
#' @param  fullframe missing or logical. Set \code{TRUE} if all the pixels of the photograph have data. For circular fisheye, with hundreds of no-data pixels, set \code{FALSE}.
#'
#' @details A call to \code{newFishEye()} return the prototype for \code{\linkS4class{FishEye}}.
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

###################
## ** fisheye ** ##
###################

#' @title Set or get fisheye.
#'
#' @aliases fisheye<-
#'
#' @description Set or get fisheye attributes for the photograph represented by a \code{\linkS4class{CanopyPhoto}}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param value \code{\linkS4class{FishEye}} that contains the fisheye attributes of \code{x}.
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


######################
## ** cloneSlots ** ##
######################

#' @title Copy slots between CanopyPhoto objects.
#'
#' @description Take the slots \code{equipment, fisheye, datetime, geocode, bearing, elevation} and \code{names} from a CanopyPhoto and copy to another.
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
    names(to) <- names(from)
    return(to)
  }
)

