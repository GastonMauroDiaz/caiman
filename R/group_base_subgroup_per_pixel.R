#### presetThr ####
#' @title Binarize images with a preset threshold.
#'
#' @description Given a single layer image and a global threshold value, it
#'   returns a binarized image. All pixels above the threshold are replaced with
#'   \code{1} and the rest with \code{0}.
#'
#' @param x \code{\linkS4class{RasterLayer}}.
#' @param thr numeric. Threshold value. It should be between minimum and maximum layer value.
#' @param ... Additional arguments (none implemented)
#'
#' @return \code{\linkS4class{BinImage}}.
#'
#' @seealso \code{\link{autoThr}}.
#'
#' @example /inst/examples/presetThrExample.R
#'
setGeneric("presetThr", function(x, ...) standardGeneric("presetThr"))

#' @rdname presetThr
setMethod("presetThr",
  signature(x = "RasterLayer"),
  function (x, thr)
  {
    if (class(thr) == "numeric") {
      tmp <- values(x)
      if (thr < min(tmp, na.rm = TRUE)) stop("thr must be greater than or equal to minimum layer value")
      if (thr >= max(tmp, na.rm = TRUE)) stop("thr must be lower than maximum layer value")
    }
    x <- x > thr
    x <- as(x, "BinImage")
    if (class(thr) == "numeric") x@threshold <- thr
    return(x)
  }
)
#' @export presetThr

#### autoThr ####
#' @title Binarize images with an automatic threshold.
#'
#' @description Given a single layer image, a global threshold value is
#'   automatically obtained and a binarized image is returned. All pixels above
#'   the auto-threshold are replaced by \code{1} and the rest by \code{0}.
#'   On the other hand, only a threshold value is returned for numeric vector
#'   input.
#'
#' @param x numeric, \code{\linkS4class{CanopyPhoto}} or
#'   \code{\linkS4class{RasterLayer}}.
#' @param subset numeric or character. It indicates the channel to process
#'   (represented as integer or by their name).
#' @param ... Additional arguments (none implemented).
#'
#' @details Threshold value is obtained with the Ridler and Calvard (1978)
#'   method. The implementation is based on
#'   \href{http://fiji.sc/Auto_Threshold#IsoData}{the IsoData method of Auto
#'   Threshold ImageJ plugin by Gabriel Landini}.
#'
#' @references
#' Ridler, T., Calvard, S., 1978. Picture thresholding using an iterative
#' selection method. IEEE Trans. Syst. Man Cybern. 8, 260-263.
#'
#' @return numeric or \code{\linkS4class{BinImage}}.
#'
#' @seealso \code{\link{presetThr}}.
#'
#' @example /inst/examples/autoThrExample.R
#'
setGeneric("autoThr", function(x, ...) standardGeneric("autoThr"))
#' @export autoThr

#' @describeIn autoThr Require a numeric vector of length greater than \code{1} and
#'   standard deviation greater than \code{0}. This method returns the threshold
#'   value.
setMethod("autoThr",
  signature(x = "numeric"),
  function (x)
  {
    if (length(x) <= 1) stop("length(x) must be greater than 1.")
    if (stats::sd(x, na.rm = TRUE) == 0) stop("sd(x) must be greater than 0.")
    thr <- mean(x, na.rm = TRUE)
    thr.back <- 0
    while (thr != thr.back) {
      thr.back <- thr
      x0 <- x[x <= thr]
      x1 <- x[x > thr]
      thr <- (mean(x0, na.rm = TRUE) + mean(x1, na.rm = TRUE)) / 2
    }
    return(thr)
  }
)

#' @describeIn autoThr The argument \code{subset} will be passed to
#'   \code{\link[raster]{subset}} for selecting which channel of the
#'   \code{\linkS4class{CanopyPhoto}} will be processed. It computes the
#'   threshold of the selected layer and return a \code{\linkS4class{BinImage}}.
setMethod("autoThr",
  signature(x = "CanopyPhoto"),
  function (x, subset = 3)
  {
    x <- raster::subset(x, subset)
    od <- x@file@name
    thr <- autoThr(values(x))
    x <- presetThr(x, thr)
    x@originalData <- od
    x@processedLayer <- subset
    validObject(x)
    return(x)
  }
)

#' @describeIn autoThr Compute the threshold and return a
#'   \code{\linkS4class{BinImage}}.
setMethod("autoThr",
  signature(x = "RasterLayer"),
  function (x)
  {
    thr <- autoThr(values(x))
    x <- presetThr(x, thr)
    return(x)
  }
)

#### normalize ####
normalize <- function(x, mn, mx, ...) {
  stopifnot(length(mn) == 1)
  stopifnot(length(mx) == 1)
  (x - mn) / (mx - mn)
}

#' @title Normalize data in the range \code{0} to \code{1}.
#'
#' @description Normalize data lying between mn and mx in the range \code{0} to \code{1}. Data
#'   greater than \code{mx} get values greater than \code{1}, proportionally. Conversely, data
#'   less than mn get values less than \code{0}.
#'
#' @param x \linkS4class{CanopyPhoto} or \code{\linkS4class{Raster}}.
#' @param mn One-length numeric. Minimum expected value.
#' @param mx One-length numeric. Maximum expected value.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#'
#' @return \linkS4class{CanopyPhoto} or \linkS4class{Raster}
#'
#' @example /inst/examples/normalizeExample.R
#'
setGeneric("normalize", normalize)
#' @export normalize

#' @describeIn normalize Result is a \linkS4class{Raster} of the same type that
#'   \code{x}.
setMethod("normalize",
  signature(x = "Raster"),
  function(x, mn, mx, ...) {
    fun <- .makeF8multi(function(x,...) normalize(x, mn, mx), ...)
    return(fun(x, ...))
  }
)

#' @rdname normalize
setMethod("normalize",
  signature(x = "RasterLayer"),
  function(x, mn, mx, ...) {
    fun <- .makeF8single(function(x,...) normalize(x, mn, mx), ...)
    return(fun(x, ...))
  }
)

#' @describeIn normalize Result is a \linkS4class{CanopyPhoto}.
setMethod("normalize",
  signature(x = "CanopyPhoto"),
  function(x, mn, mx, ...) {
    out <- normalize(as(x, "RasterBrick"), mn, mx, ...)
    out <- as(out, "CanopyPhoto")
    out <- cloneSlots(x, out)
    return(out)
  }
)

#### sRGB2LAB ####
#' @title Convert sRGB to LAB.
#'
#' @description Wrapper function of \code{\link[colorspace]{sRGB}} and
#'   \code{\link[colorspace]{LAB}} that convert colors from sRGB to LAB.
#'
#' @param x numeric, matrix or \code{\linkS4class{CanopyPhoto}}. Values must
#'   lying between \code{0} and \code{1}.
#' @param ... Additional arguments as for \code{\link[raster]{writeRaster}}.
#'
#' @return Matrix, or \code{\linkS4class{CanopyPhoto}}.
#'
#' @seealso \code{\link{normalize}}.
#'
#' @example /inst/examples/sRGB2LABexample.R
#'
setGeneric("sRGB2LAB", function(x, ...) standardGeneric("sRGB2LAB"))
#' @export sRGB2LAB

#' @describeIn sRGB2LAB Each row should represent a color. Argument \code{x} must have three columns.
setMethod("sRGB2LAB",
  signature(x = "matrix"),
  function (x)
  {
    stopifnot(max(x, na.rm = TRUE) <= 1)
    stopifnot(min(x, na.rm = TRUE) >= 0)
    if (ncol(x) != 3) stop("x must be a matrix with three columns")
    z <- colorspace::sRGB(x[, 1], x[, 2], x[, 3])
    x <- as(z, "LAB")
    x <- colorspace::coords(x)
    return(x)
  }
)

#' @describeIn sRGB2LAB Convert a single color from sRGB to LAB. Argument \code{x} must be of length three. The output is a matrix.
setMethod("sRGB2LAB",
  signature(x = "numeric"),
  function (x)
  {
    if (length(x) != 3) stop("x must be a numeric vector of length three")
    x <- matrix(x, nrow = 1)
    x <- sRGB2LAB(x)
    return(x)
  }
)

#' @describeIn sRGB2LAB The output is a \code{\linkS4class{CanopyPhoto}} with layer names L, A and B.
setMethod("sRGB2LAB",
  signature(x = "CanopyPhoto"),
  function (x, ...)
  {
    stopifnot(names(x) == c("Red", "Green", "Blue"))
    from <- x
    stopifnot(max(getMax(x)) <= 1)
    stopifnot(min(getMin(x)) >= 0)
    fun <- .makeF8multi(function(x, ...) sRGB2LAB(x), ...)
    x <- fun(x, ...)
    x <- as(x, "CanopyPhoto")
    cloneSlots(from, x)
    names(x) <- c("L", "A", "B")
    return(x)
  }
)


#### outOfDR ####
#' @title Get the percentages of pixels that are out of the dynamic range.
#'
#' @description Get the percentages of pixels that are out of the dynamic range,
#'   i.e., the under and overexposure pixels of an image.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param channel One-length character, "Red", "Green" or "Blue".
#' @param mask \linkS4class{BinImage}. Default value \code{NULL} means that all
#'   the pixels will be taking into account in the computations. If you provide
#'   a \linkS4class{BinImage}, it must have the same extent and resolution of
#'   \code{x}. All pixels from the image covered by pixels of the mask with
#'   value \code{1} will be taking into account in the computations.
#' @param returnImages logical. Default is \code{FALSE}, see Value.
#'
#' @details This algorithm classifies the pixels of x that start and end the
#'   dynamic range. These pixels are called under or overexposed because is
#'   highly probable that they are out of the dynamic range and not just in the
#'   limits.
#'
#'   todo
#'
#' @return By default, it returns a vector of length \code{3}. If
#'   \code{returnImages = TRUE}, then it returns a
#'   \code{\linkS4class{RasterStack}} that has two binary layers in which \code{1}
#'   means under/overexposure.
#'
#'   todo
#'
#' @seealso \code{\link{normalize}}, \code{\link{colorfulness}}.
#'
#' @example /inst/examples/outOfDRexample.R
#'
setGeneric("outOfDR", function(x, channel = NULL, mask = NULL,
  returnImages = FALSE)
    standardGeneric("outOfDR"))
#' @export outOfDR

#' @rdname outOfDR
setMethod("outOfDR",
  signature(x = "CanopyPhoto"),
  function (x, channel, mask, returnImages)
  {
    stopifnot(is.logical(returnImages))
    stopifnot(all(getMax(x) <= 1))
    stopifnot(all(getMin(x) >= 0))
    if(!is.null(mask)){
      stopifnot(compareRaster(x, mask) == TRUE)
      stopifnot(any(is(mask) == "BinImage"))
    }

    if (!is.null(channel)) {
      stopifnot(is.character(channel))
      stopifnot(length(channel) == 1)
    }

    fun <- function(x, value){

      if (is.null(channel)) {
        x <- x == value
        x[x == 1] <- NA
        x <- raster::subset(x, 1) + raster::subset(x, 2) + raster::subset(x, 3)
        x <- is.na(x)
      } else {
        x <- raster::subset(x, channel)
        x <- x == value
        x[x == 1] <- NA
        x <- is.na(x)
      }

      if(!is.null(mask)){
        x[mask == 0] <- NA
      }

      tmp <-  raster::freq(x, value = 1)

      if(is.null(mask)){
        foo <- tmp / ncell(x)
      } else {
        foo <- tmp / (ncell(x) - raster::freq(mask, value = 0))
      }

      list(foo * 100, x)
    }

    u <- fun(x, 0)
    o <- fun(x, 1)

    iuo <- c(u[[1]] + o[[1]], u[[1]], o[[1]])

    names(iuo) <- c("Total", "Underexposure", "Overexposure")

    if(returnImages) {
      ruo <- stack(u[[2]], o[[2]])
      names(ruo) <- c("Underexposure", "Overexposure")
      return(ruo)
    } else {
      return(iuo)
    }
  }
)


