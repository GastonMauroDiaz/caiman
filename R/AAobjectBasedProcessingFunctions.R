###########################
## ** getFeatureImage ** ##
###########################

#' @title Get an image that store object features.
#'
#' @description Given a single layer data, a segmentation and a function, this algorithm returns a \code{RasterLayer} as segmentation but with its segment IDs replaced with the segment features, which is the return of processing all the pixels that belong to a segment with the aforementioned function.
#'
#' @param x \code{\linkS4class{RasterLayer}}. The image that you want extract feature from.
#' @param segmentation \linkS4class{PolarSegmentation}. The segmentation that delimitate segments in \code{x}.  It must have the same extent and resolution of \code{x}.
#' @param fun function. It need to be able to take a single vector as input and return a one-length numeric or logical vector (e.g. mean).
#' @inheritParams autoThr
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

###########################
## ** extractFeatures ** ##
###########################

#' @title Extract object features.
#' @description Given a single layer data, a segmentation and a function, this algorithm returns a numeric vector of length equal to the number of segment in the segmentation. Each value which is the return of processing all the pixels that belong to a segment with the aforementioned function.
#'
#' @param x \code{\linkS4class{RasterLayer}}. The image that you want extract feature from.
#' @param segmentation \linkS4class{PolarSegmentation}. The segmentation that delimitate segments in \code{x}.  It must have the same extent and resolution of \code{x}.
#' @param fun function. It need to be able to take a single vector as input and return a one-length numeric or logical vector (e.g. mean).
#' @inheritParams autoThr
#'
#' @return Numeric.
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
