##################
## ** getMax ** ##
##################

#' @title Get the extreme values.
#'
#' @aliases getMax
#'
#' @description Get the maximum/minimum value of a \code{\linkS4class{Raster}}. If it has multiple layers get the maximums/minimums values of each one.
#'
#' @param x \code{\linkS4class{Raster}}.
#'
#' @return A numeric vector.
#'
#' @example /inst/examples/getMinExample.R
#'
setGeneric("getMin", function(x) standardGeneric("getMin"))
#' @export getMin

#' @rdname getMin
setMethod("getMin",
  signature(x = "RasterAll"),
  function (x) {
    return(x@data@min)
  }
)

#' @rdname getMin
setGeneric("getMax", function(x) standardGeneric("getMax"))
#' @export getMax

#' @rdname getMin
setMethod("getMax",
  signature(x = "RasterAll"),
  function (x) {
    return(x@data@max)
  }
)

###################
## ** asAngle ** ##
###################

#' @title Convert numeric to an Angle.
#'
#' @description Helper function for converting a numeric vector to an \code{\linkS4class{Angle}}. It is similar to coercing. For example, \code{as(45, "Angle")} return the same object than \code{asAngle(45)}.
#'
#' @param x numeric
#' @param degrees logical. The default of TRUE means that \code{x} it is in degrees, if it is FALSE \code{x} should be in radians.
#'
#' @return \code{\linkS4class{Angle}}.
#'
#' @seealso \code{\link{switchUnit}}
#'
#' @example /inst/examples/asAngleExample.R
#'
setGeneric("asAngle", function(x, degrees = TRUE) standardGeneric("asAngle"))
#' @export asAngle

#' @rdname asAngle
setMethod("asAngle",
  signature(x = "numeric"),
  function (x, degrees) {
    return(new("Angle", values = x, degrees = degrees))
  }
)

######################
## ** switchUnit ** ##
######################

#' @title Switch the unit of an Angle.
#'
#' @description Switch consistently the unit of an \code{\linkS4class{Angle}}. If it is in radians convert it to degrees and vice versa.
#'
#' @param x \code{\linkS4class{Angle}}.
#'
#' @seealso \code{\link{asAngle}}.
#'
#' @example /inst/examples/switchUnitExample.R
#'
setGeneric("switchUnit", function(x) standardGeneric("switchUnit"))
#' @export switchUnit

#' @rdname switchUnit
setMethod("switchUnit",
  signature(x = "Angle"),
  function (x) {
    if (!x@degrees) {
      x@values <- x@values * 180 / pi
      x@degrees <- TRUE
    } else {
      x@values <- x@values * pi / 180
      x@degrees <- FALSE
    }
    return(x)
  }
)

##################
## ** rtitle ** ##
##################

#' @title Set or get the title of a Raster*.
#'
#' @aliases rtitle<-
#'
#' @description Set or show the title of a \code{\linkS4class{Raster}}.
#'
#' @param x \code{\linkS4class{Raster}}.
#' @param value Character. The title.
#'
#' @details This method was written after reading this \href{https://stackoverflow.com/questions/9900134/is-it-bad-practice-to-access-s4-objects-slots-directly-using}{stackoverflow answer}.
#'
#' @return Character.
#'
#' @example /inst/examples/rtitleExample.R
#'
setGeneric("rtitle", function(x) standardGeneric("rtitle"))
#' @export rtitle

#' @rdname rtitle
setMethod("rtitle",
  signature(x = "Raster"),
  function (x) {
    return(x@title)
  }
)

setGeneric("rtitle<-", function(x, value) standardGeneric("rtitle<-"),
  useAsDefault = FALSE)

#' @rdname rtitle
setMethod("rtitle<-",
  signature(x = "Raster", value = "character"),
  function (x, value) {
    x@title <- value
    return(x)
  }
)
#' @export rtitle<-

##################
## ** doMask ** ##
##################

#' @title It does a mask for hemispherical photographs.
#'
#' @description Use angular limits to do a mask for hemispherical photographs.
#'
#' @param z \code{\linkS4class{ZenithImage}}.
#' @param a \code{\linkS4class{AzimuthImage}}.
#' @param zlim \code{\linkS4class{Angle}}. Set the zenith angle range with inclusive limits.
#' @param alim \code{\linkS4class{Angle}}. Set the azimuth angle range with inclusive limits.
#'
#' @return \code{\linkS4class{BinImage}}.
#'
#' @details The intervals are closed on the right and on the left.
#'
#' @seealso \code{\link{makeZimage}}, \code{\link{makeAimage}}.
#'
#' @example /inst/examples/doMaskExample.R
#'
setGeneric("doMask",
  function(z, a = makeAimage(z), zlim = NULL, alim = NULL)
    standardGeneric("doMask"))
#' @export doMask

#' @rdname doMask
setMethod("doMask",
  signature(z = "ZenithImage"),
  function (z, a, zlim, alim) {

    noDataArea <- is.na(z)

    if (all(is.null(zlim), is.null(alim))) {
      out <- !is.na(z)
    } else {
      if (all(!is.null(zlim), !is.null(alim))) {

        stopifnot(length(zlim@values) == 2)
        stopifnot(length(alim@values) == 2)
        stopifnot(is(zlim) == "Angle")
        stopifnot(is(alim) == "Angle")

        if (!zlim@degrees) zlim <-  switchUnit(zlim)
        if (!alim@degrees) alim <-  switchUnit(alim)
        stopifnot(all(zlim@values[1] >= 0, zlim@values[2] <= 90))
        stopifnot(all(alim@values[1] >= 0, alim@values[2] <= 360))

        z[is.na(z)] <- 0
        a[is.na(a)] <- 0
        z[z >= zlim@values[1] & z <= zlim@values[2]] <- NA
        a[a >= alim@values[1] & a <= alim@values[2]] <- NA
        out <- is.na(z) + is.na(a)
        out[out == 2] <- NA
        out <- is.na(out)
      } else{
        if (!is.null(zlim)) {

          stopifnot(length(zlim@values) == 2)
          stopifnot(is(zlim) == "Angle")
          if (!zlim@degrees) zlim <- switchUnit(zlim)
          stopifnot(all(zlim@values[1] >= 0, zlim@values[2] <= 90))

          z[is.na(z)] <- 0

          z[z >= zlim@values[1] & z <= zlim@values[2]] <- NA
          out <- is.na(z)

        } else {
          stopifnot(length(alim@values) == 2)
          stopifnot(is(alim) == "Angle")

          if (!alim@degrees) alim <-  switchUnit(alim)
          stopifnot(all(alim@values[1] >= 0, alim@values[2] <= 360))

          a[is.na(a)] <- 0
          a[a >= alim@values[1] & a <= alim@values[2]] <- NA
          out <- is.na(a)

        }
      }
    }
    # fix inclusion of the area outside the circle if zmin is 0
    out[noDataArea] <- 0
    #
    out <- as(out, "BinImage")
    return(out)
  }
)
