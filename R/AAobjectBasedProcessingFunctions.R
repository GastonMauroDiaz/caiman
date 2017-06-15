#### getFeatureImage ####
#' @title Get an image that stores object features.
#'
#' @description Given a single layer data, a segmentation and a function, this
#'   algorithm returns \code{RasterLayer} like segmentation but with its segment-IDs
#'   replaced with the segment-features, which are the return of processing all
#'   the pixels that belong to a segment with the aforementioned function.
#'
#' @inheritParams extractFeatures
#'
#' @return \code{\linkS4class{RasterLayer}}.
#'
#' @seealso \code{\link{extractFeatures}}.
#'
#' @example /inst/examples/getFeatureImageExample.R
#'
setGeneric("getFeatureImage", function(x, ...) standardGeneric("getFeatureImage"))
#' @export getFeatureImage

#' @rdname getFeatureImage
setMethod("getFeatureImage",
  signature(x = "RasterLayer"),
  function(x, segmentation, fun) {
    stopifnot(class(segmentation)[[1]] == "PolarSegmentation")
    stopifnot(any(class(fun) == "function", class(fun) == "standardGeneric"))

    feature <- tapply(values(x), values(segmentation), fun)
    id <- as.numeric(names(feature))
    df <- data.frame(id, feature)
    featureImage <- subs(segmentation, df)
    return(featureImage)
  }
)

#### extractFeatures ####
#' @title Extract object features.
#' @description Given a single layer data, a segmentation and a function, this
#'   algorithm returns a numeric vector of length equal to the number of
#'   segments in the segmentation. Each value of the vector is the return
#'   obtained if all pixels that belong to a segment are processed with the
#'   aforementioned function.
#'
#' @param x \code{\linkS4class{RasterLayer}}. The image that you want to extract
#'   feature from.
#' @param segmentation \linkS4class{PolarSegmentation}. The segmentation of
#'   \code{x}.
#' @param fun \code{function} that take a vector as input and return a one-length
#'   numeric or logical (e.g. mean).
#' @param ... Additional arguments (none implemented).
#'
#' @return \code{numeric.}
#'
#' @seealso \code{\link{getFeatureImage}}.
#'
#' @example /inst/examples/getFeatureImageExample.R
#'
setGeneric("extractFeatures", function(x, ...) standardGeneric("extractFeatures"))
#' @export extractFeatures

#' @rdname extractFeatures
setMethod("extractFeatures",
  signature(x = "RasterLayer"),
  function(x, segmentation, fun) {

    stopifnot(class(segmentation)[[1]] == "PolarSegmentation")
    stopifnot(any(class(fun) == "function", class(fun) == "standardGeneric"))

    fe <- tapply(values(x), values(segmentation), fun)
    ids <- names(fe)
    fe <- as.numeric(fe)
    names(fe) <- ids
    return(fe)
  }
)
