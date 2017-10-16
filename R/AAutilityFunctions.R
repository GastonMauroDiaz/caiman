#### getMax ####
#' @title Get the extreme values.
#'
#' @aliases getMax
#'
#' @description Get the maximum/minimum value of a \code{\linkS4class{Raster}}.
#'   If it is multilayer, then get the maximums/minimums values of each layer.
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

#### asAngle ####
#' @title Convert numeric to an Angle.
#'
#' @description Helper function for converting a numeric vector to an
#'   \code{\linkS4class{Angle}}. It is similar to coercing. For example,
#'   \code{as(45, "Angle")} return the same object than \code{asAngle(45)}.
#'
#' @param x numeric
#' @param degrees logical. By default is set as TRUE, meaning x is in degrees.
#'   If FALSE x should be in radians.
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

#### switchUnit #####
#' @title Switch the unit of an Angle.
#'
#' @description Switch consistently the unit of an \code{\linkS4class{Angle}}.
#'   If it is in radians convert it to degrees and vice versa.
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

#### rtitle ####
#' @title Set or get the title of a Raster*.
#'
#' @aliases rtitle<-
#'
#' @description Set or get the title of a \code{\linkS4class{Raster}}.
#'
#' @param x \code{\linkS4class{Raster}}.
#' @param value character. The title.
#'
#' @details This method was written after reading this
#'   \href{https://stackoverflow.com/questions/9900134/is-it-bad-practice-to-access-s4-objects-slots-directly-using}{stackoverflow
#'   answer}.
#'
#' @return character.
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

#### doMask ####
#' @title Do a mask for hemispherical photographs given angle restrictions.
#'
#' @description Do a mask for hemispherical photographs given angle restrictions.
#'
#' @param x \code{\linkS4class{ZenithImage}} or
#'   \code{\linkS4class{CanopyPhoto}}.
#' @param y todo
#' @param previousMask \code{\linkS4class{BinImage}}. Default is \code{NULL}.
#' @param a \code{\linkS4class{AzimuthImage}}.
#' @param zlim \code{\linkS4class{Angle}}. Set the zenith angle range with
#'   inclusive limits.
#' @param alim \code{\linkS4class{Angle}}. Set the azimuth angle range with
#'   inclusive limits.
#'
#' @return \code{\linkS4class{BinImage}}.
#'
#' @details The intervals are closed on the right and on the left.
#'
#' Todo: explain previousMask behavior.
#'
#' @seealso \code{\link{makeZimage}}, \code{\link{makeAimage}}.
#'
#' @references Schneider, D., Schwalbe, E., Maas, H.-G., 2009. Validation of
#'   geometric models for fisheye lenses. ISPRS J. Photogramm. Remote Sens. 64,
#'   259-266.
#'
#' @example /inst/examples/doMaskExample.R
#'
setGeneric("doMask",
  function (x, z, previousMask = NULL, a = makeAimage(x), zlim = NULL, alim = NULL)
    standardGeneric("doMask"))
#' @export doMask

#' @describeIn doMask You can use angular limits to do a mask for hemispherical
#'   photographs.
setMethod("doMask",
  signature(x = "ZenithImage"),
  function (x, previousMask, a, zlim, alim) {

    if (!is.null(previousMask))
      stopifnot(is(previousMask)[1] == "BinImage")

    a
    z <- x
    rm(x)
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

    if (!is.null(previousMask)) out <- out * previousMask

    return(out)
  }
)

#' @describeIn doMask This feature is needed for further processing fullframe
#'   hemispherical photographs.
setMethod("doMask",
  signature(x = "CanopyPhoto"),
  function (x, z) {

    stopifnot(class(z) == "ZenithImage")

    if (all(!fisheye(x)@fullframe, fisheye(x)@is))
      stop("Argument x should be a fullframe hemispherical photo, you need to declare this with the fisheye function.")

    x <- raster::subset(x, 1)
    x[] <- 255

    center <- ncol(z) / 2
    xmn <- center - (ncol(x) / 2)
    xmx <- center + (ncol(x) / 2)
    ymn <- center - (nrow(x) / 2)
    ymx <- center + (nrow(x) / 2)
    e <- extent(xmn, xmx, ymn, ymx)
    extent(x) <- e
    foo <- extend(x, z, value = 0)
    e <- extent(0, ncol(foo), 0, nrow(foo))
    extent(foo) <- e
    x <- foo == 255
    as(x, "BinImage")
  }
)

#' @rdname doMask
setMethod("doMask",
  signature(x = "RelativeRadiusImage"),
  function (x) {
    as(!is.na(x), "BinImage")
  }
)

#### expandFullframe ####
#' @title Expand full frame hemispherical photographs
#'
#' @description Expand a full frame hemispherical photograph to get the
#'   equivalent of a circular hemispherical photograph. Added
#'   pixels will be \code{0}.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param z \code{\linkS4class{ZenithImage}}.

#' @return \code{\linkS4class{CanopyPhoto}}.
#'
#' @seealso \code{\link{doMask}}.
#'
#' @example /inst/examples/doMaskExample.R
#'
setGeneric("expandFullframe",
  function (x, z)
    standardGeneric("expandFullframe"))
#' @export expandFullframe

#' @rdname expandFullframe
setMethod("expandFullframe",
  signature(x = "CanopyPhoto", z = "ZenithImage"),
  function (x, z) {

    if (all(!fisheye(x)@fullframe, fisheye(x)@is))
      stop("Argument x should be a fullframe hemispherical photo, you need to declare this with the fisheye function.")

    center <- ncol(z) / 2
    xmn <- center - (ncol(x) / 2)
    xmx <- center + (ncol(x) / 2)
    ymn <- center - (nrow(x) / 2)
    ymx <- center + (nrow(x) / 2)
    e <- extent(xmn, xmx, ymn, ymx)
    extent(x) <- e

    foo <- extend(x, z, value = 0)

    foo <- as(foo, "CanopyPhoto")
    foo <- cloneSlots(x, foo)
    foo@fisheye@fullframe <- FALSE
    e <- extent(0, ncol(foo), 0, nrow(foo))
    extent(foo) <- e
    foo

  }
)

#### calcExposure ####
#' @title Calculate exposure
#'
#' @description Calculate the exposure for a given shutter speed an aperture.
#'
#' @param ssDenominator numeric. Denominator of the shutter speed.
#' @param aperture numeric. The aperture or f number.
#'
#' @references Allen, E., & Triantaphillidou, S. (2010). The manual of
#'   photography. (E. Allen & S. Triantaphillidou, Eds.) (10th ed.). Amsterdam:
#'   Elsevier.
#'
#' @return numeric
#'
#' @example /inst/examples/calcExposureExample.R
#'
setGeneric("calcExposure", function(ssDenominator, aperture)
              standardGeneric("calcExposure"))
#' @export calcExposure

#' @rdname calcExposure
setMethod("calcExposure",
          signature(ssDenominator = "numeric", aperture = "numeric"),
          function (ssDenominator, aperture) {
            log2(aperture^2 * ssDenominator)
          }
)


#### addMasks ####
#' @title todo
#'
#' @description todo
#'
#' @param x todo
#' @param ... todo
#'
#' @return todo
#'
#' @examples #/inst/examples/ Example.R
#'
setGeneric("addMasks", function(x, ...)
  standardGeneric("addMasks"))
#' @export addMasks

#' @rdname addMasks
setMethod("addMasks",
          signature(x = "BinImage"),
          function (x, ...) {
            x <- stack(x, ...)
            x <- calc(x, sum)
            x[x != 0] <- 1
            as(x, "BinImage")
          }
)


#### getNumberOfPixels ####
#' @title todo
#'
#' @description todo
#'
#' @param x todo
#' @param m todo
#' @param onlyMin todo
#'
#' @return todo
#'
#' @examples #/inst/examples/ Example.R
#'
setGeneric("getNumberOfPixels", function(x, m, onlyMin = TRUE)
  standardGeneric("getNumberOfPixels"))
#' @export addMasks

#' @rdname getNumberOfPixels
setMethod("getNumberOfPixels",
          signature(x = "PolarSegmentation"),
          function (x, m, onlyMin) {
            p[!m] <- NA
            out <- tapply(p[], p[], length)
            if (onlyMin) out <- min(out)
            return(out)
          }
)

